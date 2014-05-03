(ns flow.for
  (:require #+clj [clojure.core.async :as a :refer [go go-loop alt!]]
            #+clj [flow.let :as l]
            #+cljs [cljs.core.async :as a]
            [flow.stream :refer [stream-ch Stream]]
            [flow.ioc :refer [form->binds]])

  #+cljs (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(defn parse-for<<-bindings [bindings]
  (when (seq bindings)
    (lazy-seq
     (when-let [[sym value & more] (seq bindings)]
       (cond
        (= :when sym) (cons {:type :filter
                             :pred value}
                            (parse-for<<-bindings more))

        (= :while sym) (cons {:type :take-while
                              :pred value}
                             (parse-for<<-bindings more))

        (= :let sym) (cons {:type :let
                            :bindings value}
                           (parse-for<<-bindings more))

        :else (cons {:type :vanilla
                     :sym sym
                     :form value}
                    (parse-for<<-bindings more)))))))

(defmulti for-stream-bindings
  (fn [acc binding for-chan-opts]
    (:type binding)))

(defmethod for-stream-bindings :vanilla [bindings {:keys [form]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding `(l/let<< [stream-elems# ~form]
                                 (for [tuple# (or (seq ~all-tuples-sym) [[]])
                                       stream-elem# stream-elems#]
                                   (conj tuple# stream-elem#)))]))

(defmethod for-stream-bindings :filter [bindings {:keys [pred]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding `(filter (fn [~sym-binding]
                                          ~pred)
                                       ~all-tuples-sym)]))

(defmethod for-stream-bindings :take-while [bindings {:keys [pred]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding `(take-while (fn [~sym-binding]
                                             ~pred)
                                           ~all-tuples-sym)]))

(defmethod for-stream-bindings :let [bindings {let-bindings :bindings} _]
  (conj bindings let-bindings))

(defn for-stream-form [parsed-bindings]
  (let [all-tuples-sym (gensym "all_tuples")

        syms (->> parsed-bindings
                  (map :sym)
                  (remove nil?))
        
        for-chan-opts {:all-tuples-sym all-tuples-sym
                       :sym-binding `[~@syms :as ~all-tuples-sym]}
        
        chan-bindings (reduce (fn [bindings binding]
                                (for-stream-bindings bindings binding for-chan-opts))
            
                              [[all-tuples-sym []]]
            
                              parsed-bindings)]
    
    {:syms syms
     :for-stream `(l/let<< [~@(apply concat chan-bindings)]
                    ~all-tuples-sym)}))

(comment
  (for-stream-form '[{:type :stream, :sym {:keys [x y]}, :form !my-atom}
                   {:type :vanilla, :sym z, :form (range 4)}
                   {:type :when, :pred (even? x)}
                   #_{:type :sort-by, :sort-form (fn [{:keys [x y]} z] [x (- y)])}]))

(defn process-for<<-body [for-stream body-fn]
  (reify Stream
    (stream-ch [_ cancel-ch buffer-fn]
      (let [out-ch (a/chan (buffer-fn))
            down-cancel-ch (a/chan)
            for-ch (stream-ch for-stream down-cancel-ch buffer-fn)]
        (go-loop [cache {}]
          (alt!
            :priority true
            cancel-ch ([_]
                         (a/close! down-cancel-ch)
                         (a/close! out-ch))

            for-ch ([ids]
                      (if ids
                        (let [results (-> (for [id ids]
                                            (or (get cache id)
                                                (body-fn id)))
                                          
                                          (vary-meta assoc :flow/ids ids))]
                          (a/>! out-ch results)
                          (recur (zipmap ids results)))

                        (do
                          (a/close! down-cancel-ch)
                          (a/close! out-ch))))))

        out-ch))))

#+clj
(defmacro for<< [bindings & body]
  (let [{:keys [syms for-stream]} (-> bindings
                                      parse-for<<-bindings
                                      for-stream-form)]
    `(process-for<<-body ~for-stream
                         (fn [[~@syms]]
                           (do ~@body)))))
