(ns flow.stream
  #+clj
  (:require [clojure.core.async :as a :refer [go go-loop alt!]]
            [clojure.core.async.impl.protocols :as ap])

  #+clj
  (:import [clojure.lang Atom])

  #+cljs
  (:require [dommy.core :as d]
            [cljs.core.async :as a]
            [cljs.core.async.impl.protocols :as ap])
  
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(def nil-sentinel
  ::nil)

(defprotocol Stream
  (stream-ch [_ cancel-ch]))

(defn ch->stream [ch]
  (let [mult-ch (a/mult ch)]
    (reify Stream
      (stream-ch [_ cancel-ch]
        (let [out-ch (a/chan)
              cancel-ch (a/chan)]
          (go-loop [old-val ::initial]
            (alt!
              :priority true
              cancel-ch ([_]
                           (a/close! out-ch))

              ch ([new-val]
                    (if-not (nil? new-val)
                      (do
                        (when-not (= old-val new-val)
                          (a/>! out-ch new-val)
                          (recur new-val)))
                      (a/close! out-ch))))))))))

(defn ->stream [obj]
  (cond
   (satisfies? Stream obj) obj
   (and (satisfies? ap/ReadPort obj)
        (satisfies? ap/Channel obj)) (ch->stream obj)))

(extend-protocol Stream
  Atom
  (stream-ch [!atom cancel-ch]
    (let [out-ch (a/chan)
          watch-key (gensym "atom-stream")]

      (a/put! out-ch @!atom)
      
      (add-watch !atom watch-key
                 (fn [_ _ _ v]
                   (a/put! out-ch v)))
      
      (go
        (a/<! cancel-ch)
        (a/close! out-ch)
        (remove-watch !atom watch-key))

      out-ch)))

#+cljs
(defn html-element-stream-ch [$el cancel-ch value-fn]
  
  (let [out-ch (a/chan (a/sliding-buffer 1))
        listener (fn [e]
                   (a/put! out-ch (value-fn (.-target e))))]

    (d/listen! $el :keyup listener)
    (d/listen! $el :change listener)
    (a/put! out-ch (value-fn $el))

    (go
      (a/<! cancel-ch)
      (d/unlisten! $el :keyup listener)
      (d/unlisten! $el :change listener)
      (a/close! out-ch))

    out-ch))

#+cljs
(extend-protocol Stream
  js/HTMLInputElement
  (stream-ch [$el cancel-ch]
    (html-element-stream-ch $el
                           cancel-ch
                           (case (.-type $el)
                             "checkbox" #(.-checked %)
                             #(.-value %))))

  js/HTMLTextAreaElement
  (stream-ch [$el cancel-ch]
    (html-element-stream-ch $el cancel-ch #(.-value %)))

  js/HTMLSelectElement
  (stream-ch [$el cancel-ch]
    (html-element-stream-ch $el cancel-ch #(.-value %))))

(defn stream-return [v]
  (reify Stream
    (stream-ch [_ _]
      (go
        (if-not (nil? v)
          v
          nil-sentinel)))))

(defn stream-bind [s f]
  ;; s :: Stream a
  ;; f :: (a -> Stream b)
  ;; returns :: Stream b

  (reify Stream
    (stream-ch [_ cancel-ch]
      (let [out-ch (a/chan)
            stream-cancel-ch (a/chan)
            stream-value-ch (stream-ch s stream-cancel-ch)]        

        (go-loop [old-stream-value ::initial
                  fn-cancel-ch (a/chan)
                  fn-stream-ch (a/chan)]

          (alt!
            :priority true

            cancel-ch ([_]
                         (a/close! stream-cancel-ch)
                         (a/close! fn-cancel-ch)
                         (a/close! out-ch))

            stream-value-ch ([new-val]
                               (if-not (nil? new-val)

                                 (let [new-val (if (= nil-sentinel new-val)
                                                 nil
                                                 new-val)]
                                   (if (= old-stream-value new-val)
                                     (recur old-stream-value fn-cancel-ch fn-stream-ch)
                                                              
                                     (let [new-fn-cancel-ch (a/chan)
                                           new-fn-stream-ch (stream-ch (f new-val) new-fn-cancel-ch)]
                                       (recur new-val new-fn-cancel-ch new-fn-stream-ch))))

                                 (do
                                   (a/close! fn-cancel-ch)
                                   (a/close! out-ch))))

            fn-stream-ch ([new-val]
                           (if-not (nil? new-val)
                             (do
                               (a/>! out-ch new-val)
                               (recur old-stream-value fn-cancel-ch fn-stream-ch))
                             
                             (recur old-stream-value fn-cancel-ch (a/chan))))))
        
        out-ch))))


