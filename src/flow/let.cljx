(ns flow.let
  (:require [flow.stream :refer [stream-bind stream-return ->stream]]
            #+clj [flow.ioc :refer [form->binds]]))

#+clj
(defmacro let<< [bindings & body]
  (if-let [[sym value & more] (seq bindings)]
    `(stream-bind (->stream ~(form->binds value))
                  (fn [~sym]
                    (let<< [~@more]
                      ~@body)))

    `(stream-return (do ~@body))))
