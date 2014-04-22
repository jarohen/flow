(ns flow.core
  #+clj
  (:require [clojure.core.async :as a :refer [go go-loop]]
            [flow.stream :refer [nil-sentinel stream-ch stream-bind stream-return ->stream]])

  #+cljs
  (:require [dommy.core :as d]
            [cljs.core.async :as a]
            [flow.stream :refer [nil-sentinel stream-ch stream-bind stream-return ->stream]])

  #+cljs
  (:require-macros [dommy.macros :refer [node sel1]]
                   [cljs.core.async.macros :refer [go go-loop]]))

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
            more (cons value-or-more more)]
        `(let [~sym ~value]
           (let<< [~@more]
             ~@body))))

    `(stream-return (do ~@body))))

(defn parse-for<<-bindings [bindings]
  (lazy-seq
   (when-let [[sym bind-or-value value-or-more & more] (seq bindings)]
     (cond
      (= '<< bind-or-value) (cons {:type :stream
                                   :sym sym
                                   :form value-or-more}
                                  (parse-for<<-bindings more))

      (= :when sym) (cons {:type :when
                           :pred bind-or-value}
                          (parse-for<<-bindings (cons value-or-more more)))

      (= :sort-by sym) (cons {:type :sort-by
                              :sort-form bind-or-value}
                             (parse-for<<-bindings (cons value-or-more more)))

      :else (cons {:type :vanilla
                   :sym sym
                   :form bind-or-value}
                  (parse-for<<-bindings (cons value-or-more more)))))))

(comment
  (defmulti for-chan-bindings
    (fn [acc binding]
      (:type binding)))

  (defmethod for-chan-bindings :stream [{:keys [syms all-tuples-sym bindings] :as acc} {:keys [sym form]}]
    (update-in acc [:bindings]
               conj [(vec (concat syms [sym :as all-tuples-sym])) `(conj ~all-tuples-sym ~form)]))

  (defn for-chan-form [parsed-bindings]
    (reduce for-chan-bindings
            {:all-tuples-sym (gensym "all_tuples")}
            parsed-bindings))

  (for-chan-form [{:type :stream
                   :sym 'x
                   :form '(range 4)}]))

(comment
  
  #+clj
  (defmacro for<< [bindings & body]
    (if-let [[sym bind-or-value value-or-more & more] (seq bindings)]
      (cond
       (= '<< bind-or-value) (let [stream value-or-more]
                               `(stream-bind (->stream ~stream)
                                             (fn [~sym]
                                               (let<< [~@more]
                                                 ~@body))))

       (= :when sym)
     

       :else (let [value bind-or-value
                   more (cons value-or-more more)]
               `(let [~sym ~value]
                  (let<< [~@more]
                    ~@body))))


      ;; TODO we're in the body here
      `(stream-return (do ~@body)))))

#+cljs
(defn- new-container []
  (node [:div {:style {:display "inline"}}]))

#+cljs
(defn el<< [el-stream]
  (let [$container (new-container)
        el-ch (stream-ch el-stream (a/chan))
        buffered-el-ch (a/chan (a/sliding-buffer 1))]
    
    (a/pipe el-ch buffered-el-ch)
    
    (go-loop []
      (when-let [$el (a/<! buffered-el-ch)]
        (let [$el (if (= nil-sentinel $el)
                    nil
                    $el)]

          (d/replace-contents! $container $el)
          (recur))))
    
    $container))

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
