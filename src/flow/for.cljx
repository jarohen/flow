(ns flow.for
  (:require #+clj [clojure.core.async :as a :refer [go go-loop alt!]]
            #+clj [flow.let :as l]
            #+cljs [cljs.core.async :as a]
            [flow.stream :refer [stream-ch Stream]])

  #+cljs (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(defn parse-for<<-bindings [bindings]
  (letfn [(cons* [x coll]
            (cond->> coll
              x (cons x)))]

    (when (seq bindings)
      (lazy-seq
       (when-let [[sym bind-or-value value-or-more & more] (seq bindings)]
         (cond
          (= '<< bind-or-value) (cons {:type :stream
                                       :sym sym
                                       :form value-or-more}
                                      (parse-for<<-bindings more))

          (= :when sym) (cons {:type :filter
                               :pred bind-or-value}
                              (parse-for<<-bindings (cons* value-or-more more)))

          (= :sort-value sym) (let [[comparator & more] (case value-or-more
                                                          :sort-comparator more
                                                          :sort-reverse? (let [[reverse & more] more]
                                                                           (cons `(if ~reverse
                                                                                    #(compare %2 %1)
                                                                                    #(compare %1 %2))
                                                                                 more))
                                                          (cons 'compare (cons* value-or-more more)))]
                                (cons {:type :sort
                                       :sort-value bind-or-value
                                       :sort-comparator comparator}
                                      (parse-for<<-bindings more)))

          :else (cons {:type :vanilla
                       :sym sym
                       :form bind-or-value}
                      (parse-for<<-bindings (cons* value-or-more more)))))))))

(defmulti for-stream-bindings
  (fn [acc binding for-chan-opts]
    (:type binding)))

(defmethod for-stream-bindings :stream [bindings {:keys [form]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding '<< `(l/let<< [stream-elems# ~'<< ~form]
                                     (for [tuple# (or (seq ~all-tuples-sym) [[]])
                                           stream-elem# stream-elems#]
                                       (conj tuple# stream-elem#)))]))

(defmethod for-stream-bindings :vanilla [bindings {:keys [form]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding `(let [stream-elems# ~form]
                                 (for [tuple# (or (seq ~all-tuples-sym) [[]])
                                       stream-elem# stream-elems#]
                                   (conj tuple# stream-elem#)))]))

(defmethod for-stream-bindings :sort [bindings {:keys [sort-value sort-comparator]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding `(sort-by (fn [~sym-binding]
                                          ~sort-value)
                                        ~sort-comparator
                                        ~all-tuples-sym)]))

(defmethod for-stream-bindings :filter [bindings {:keys [pred]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding `(filter (fn [~sym-binding]
                                          ~pred)
                                        ~all-tuples-sym)]))

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

                      ;; TODO not sure what the laziness problem is
                      ;; here, but we eval body-fn twice per id if
                      ;; there's no 'doall'

                      (if ids
                        (let [results (-> (for [id ids]
                                            (or (get cache id)
                                                (body-fn id)))
                                          doall)]
                          (a/>! out-ch (-> results
                                           (vary-meta assoc :flow/ids ids)))
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
