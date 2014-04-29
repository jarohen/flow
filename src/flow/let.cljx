(ns flow.let
  (:require [flow.stream :refer [stream-bind stream-return ->stream]]))

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
