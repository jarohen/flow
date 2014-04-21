(ns flow.core
  #+clj
  (:require [clojure.core.async :as a :refer [go go-loop]])

  #+cljs
  (:require [dommy.core :as d]
            [cljs.core.async :as a]
            [flow.stream :refer [nil-sentinel stream-ch stream-bind stream-return]])

  #+cljs
  (:require-macros [dommy.macros :refer [node sel1]]
                   [cljs.core.async.macros :refer [go go-loop]]
                   [flow.core :refer [let<<]]))

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

#+clj
(defmacro el<< [& args]
  `(el<< ~@args))

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
