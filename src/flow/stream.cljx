(ns flow.stream
  #+clj
  (:require [clojure.core.async :as a :refer [go go-loop alt!]]
            [clojure.core.async.impl.protocols :as ap] )

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

(defn wrap-nil [v]
  (if-not (nil? v)
    v
    nil-sentinel))

(defn unwrap-nil [v]
  (when-not (= v nil-sentinel)
    v))

(defprotocol Stream
  (stream-ch [_ cancel-ch buffer-fn]))

(defn ch->stream [ch]
  (let [!last-value (atom ::initial)]

    (reify Stream
      (stream-ch [_ cancel-ch buffer-fn]
        (let [out-ch (a/chan (buffer-fn))
              last-value @!last-value]

          (when-not (= last-value ::initial)
            (a/put! out-ch last-value))
          
          (go-loop [old-val ::initial]
            (alt!
              :priority true
              cancel-ch ([_]
                           (a/close! out-ch))

              ch ([new-val]
                    (if-not (nil? new-val)
                      (do
                        (when-not (= old-val new-val)
                          (reset! !last-value new-val)
                          (a/>! out-ch new-val)
                          (recur new-val)))
                      (a/close! out-ch)))))
          
          out-ch)))))

(extend-protocol Stream
  Atom
  (stream-ch [!atom cancel-ch buffer-fn]
    (let [out-ch (a/chan (buffer-fn))
          watch-key (gensym "atom-stream")]

      (a/put! out-ch (wrap-nil @!atom))
      
      (add-watch !atom watch-key
                 (fn [_ _ _ v]
                   (a/put! out-ch (wrap-nil v))))
      
      (go
        (a/<! cancel-ch)
        (a/close! out-ch)
        (remove-watch !atom watch-key))

      out-ch)))

#+cljs
(defn html-element-stream-ch [$el cancel-ch buffer-fn value-fn]
  
  (let [value-fn #(or (value-fn %) "")
        out-ch (a/chan (buffer-fn))
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
  (stream-ch [$el cancel-ch buffer-fn]
    (html-element-stream-ch $el
                            cancel-ch
                            buffer-fn
                            (case (.-type $el)
                              "checkbox" #(.-checked %)
                              #(.-value %))))

  js/HTMLTextAreaElement
  (stream-ch [$el cancel-ch buffer-fn]
    (html-element-stream-ch $el cancel-ch buffer-fn #(.-value %)))

  js/HTMLSelectElement
  (stream-ch [$el cancel-ch buffer-fn]
    (html-element-stream-ch $el cancel-ch buffer-fn #(.-value %))))

(defn ->stream* [obj]
  (cond
   (satisfies? Stream obj) obj
   
   (and (satisfies? ap/ReadPort obj) (satisfies? ap/Channel obj)) (ch->stream obj)
   
   :else (atom obj)))

#+clj
(defmacro ->stream [obj]
  ;; This macro doesn't change the behaviour of ->stream*, just
  ;; extracts out a few equivalences at compile-time
  (let [expanded-obj (macroexpand obj)]
    (or (when (seq? expanded-obj)
          (condp = (first expanded-obj)
            'flow.stream/stream-return expanded-obj
            'flow.stream/stream-bind* expanded-obj
            'flow.stream/->stream* expanded-obj
            
            nil))
      
      `(->stream* ~expanded-obj))))

(defn stream-return [v]
  (atom v))

(defn stream-bind* [s f]
  ;; s :: Stream a
  ;; f :: (a -> Stream b)
  ;; returns :: Stream b

  (reify Stream
    (stream-ch [_ cancel-ch buffer-fn]
      (let [out-ch (a/chan (buffer-fn))
            stream-cancel-ch (a/chan)
            stream-value-ch (stream-ch s stream-cancel-ch buffer-fn)]        

        (go-loop [old-stream-value ::initial
                  fn-cancel-ch (a/chan)
                  fn-stream-ch (a/chan)]

          (alt!
            cancel-ch ([_]
                         (a/close! stream-cancel-ch)
                         (a/close! fn-cancel-ch)
                         (a/close! out-ch))

            stream-value-ch ([new-val]
                               (if-not (nil? new-val)

                                 (let [new-val (unwrap-nil new-val)]
                                   (if (= old-stream-value new-val)
                                     (recur old-stream-value fn-cancel-ch fn-stream-ch)
                                                              
                                     (let [new-fn-cancel-ch (a/chan)
                                           new-fn-stream-ch (stream-ch (f new-val) new-fn-cancel-ch buffer-fn)]
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

#+clj
(defmacro stream-bind [s f]
  ;; This macro doesn't change the behaviour of stream-bind*, just
  ;; extracts out a few equivalences at compile-time
  (let [expanded-stream (macroexpand s)]
    (or (when (and (seq? f)
                   (= 3 (count f)))
          (let [[fn-call [arg] body] f]
            (when (and (= fn-call 'clojure.core/fn)
                       (= `(stream-return ~arg) body))
              expanded-stream)))
              
        (when (and (seq? expanded-stream)
                   (= (first expanded-stream) 'flow.stream/stream-return))
          `(~f ~(second expanded-stream)))

        `(stream-bind* ~expanded-stream ~f))))
