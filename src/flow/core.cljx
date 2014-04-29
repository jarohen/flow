(ns flow.core
  (:require #+clj [clojure.core.async :as a :refer [go go-loop alt!]]
            #+cljs [cljs.core.async :as a]
            #+cljs [dommy.core :as d]
            [flow.stream :refer [nil-sentinel stream-ch stream-bind stream-return ->stream Stream]])

  #+cljs
  (:require-macros [dommy.macros :refer [node sel1]]
                   [cljs.core.async.macros :refer [go go-loop alt!]]))

#+clj
(defmacro let<< [bindings & body]
  (if-let [[sym bind-or-value value-or-more & more] (seq bindings)]
    (if (= '<< bind-or-value)
      (let [stream value-or-more]
        `(stream-bind (->stream ~stream)
                      (fn [~sym]
                        (let<< [~@more]
                          ~@body))))

      (let [value bind-or-value
            more (->> (cons value-or-more more)
                      (remove nil?))]
        `(let [~sym ~value]
           (let<< [~@more]
             ~@body))))

    `(stream-return (do ~@body))))

(defn parse-for<<-bindings [bindings]
  (when (seq bindings)
    (lazy-seq
     (when-let [[sym bind-or-value value-or-more & more] (seq bindings)]
       (cond
        (= '<< bind-or-value) (cons {:type :stream
                                     :sym sym
                                     :form value-or-more}
                                    (parse-for<<-bindings more))

        (= :when sym) (cons {:type :when
                             :pred bind-or-value}
                            (when value-or-more
                              (parse-for<<-bindings (cons value-or-more more))))

        (= :sort-val sym) (cons {:type :sort-val
                                :sort-form bind-or-value}
                               (when value-or-more
                                 (parse-for<<-bindings (cons value-or-more more))))

        :else (cons {:type :vanilla
                     :sym sym
                     :form bind-or-value}
                    (when value-or-more
                      (parse-for<<-bindings (cons value-or-more more)))))))))

(defmulti for-stream-bindings
  (fn [acc binding for-chan-opts]
    (:type binding)))

(defmethod for-stream-bindings :stream [bindings {:keys [form]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding '<< `(let<< [stream-elems# ~'<< ~form]
                                     (for [tuple# (or (seq ~all-tuples-sym) [[]])
                                           stream-elem# stream-elems#]
                                       (conj tuple# stream-elem#)))]))

(defmethod for-stream-bindings :vanilla [bindings {:keys [form]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding `(let [stream-elems# ~form]
                                 (for [tuple# (or (seq ~all-tuples-sym) [[]])
                                       stream-elem# stream-elems#]
                                   (conj tuple# stream-elem#)))]))

(defmethod for-stream-bindings :sort-val [bindings {:keys [sort-form]} {:keys [sym-binding all-tuples-sym]}]
  (conj bindings [sym-binding `(sort-by (fn [~sym-binding]
                                          ~sort-form)
                                        ~all-tuples-sym)]))

(defmethod for-stream-bindings :when [bindings {:keys [pred]} {:keys [sym-binding all-tuples-sym]}]
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
     :for-stream `(let<< [~@(apply concat chan-bindings)]
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
                        (let [results (for [id ids]
                                        (or (get cache id)
                                            (body-fn id)))]
                          (a/>! out-ch (-> results
                                           (with-meta {:ids ids})))
                          (recur (zipmap ids results)))

                        (a/close! down-cancel-ch)
                        (a/close! out-ch)))))

        out-ch))))

#+clj
(defmacro for<< [bindings & body]
  (let [{:keys [syms for-stream]} (-> bindings
                                      parse-for<<-bindings
                                      for-stream-form)]
    `(process-for<<-body ~for-stream
                         (fn [[~@syms]]
                           (do ~@body)))))

(comment
  (def !foo-atom (atom [{:x 2 :y 4} {:x 3 :y -3} {:x 2 :y 1}]))

  (repeatedly 10 (fn [] (swap! !foo-atom #(update-in % [(rand-int (count %)) :x] inc))))

  (def foo-chan
    (for<< [{:keys [x y]} << !foo-atom
            z (range 4)
            :when (even? z)
            :sort-by [x (- y)]]
      [y (inc z) x]))

  (go-loop []
    (when-let [value (a/<! foo-chan)]
      (prn value "<- ch value:")
      (recur)))


  )

#+cljs
(defn- new-container []
  (node [:div {:style {:display "inline"}}]))

#+cljs
(defn el<<* [el-stream]
  (let [$container (new-container)
        el-ch (stream-ch el-stream (a/chan) #(a/sliding-buffer 1))]
    
    (go-loop []
      (when-let [$el (a/<! el-ch)]
        (let [$el (if (= nil-sentinel $el)
                    nil
                    $el)]

          (d/replace-contents! $container $el)
          (recur))))
    
    $container))

#+clj
(defmacro el<< [el-stream]
  `(el<<* ~el-stream))

(comment
  (do
    (def !foo-atom (atom 0))
    
    (def foo-el
      (let [$select-el (node [:select
                              [:option {:value "a"} "Just an A..."]
                              [:option {:value "b"} "It's a B!"]])]
        (node [:div.container
               $select-el
               [:p
                "Input value is "
                (el<< (let<< [select-val << $select-el]
                        (node [:span select-val])))]
               
               [:p
                "Atom value is: "
                (el<< (let<< [atom-val << !foo-atom]
                        (node [:span atom-val])))]

               (doto (node [:button "inc"])
                 (d/listen! :click
                     (fn [e]
                       (swap! !foo-atom inc))))])))

    (d/replace-contents! (sel1 :#content) foo-el))

  (d/value $foo-el)

  (swap! !foo-atom + (rand-int 3))
  
  (a/reduce #(do (prn %2) %1) [] (diff-ch !foo-atom)))
