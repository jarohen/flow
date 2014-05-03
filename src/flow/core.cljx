(ns flow.core
  (:require [flow.for :as f :include-macros true]
            [flow.let :as l :include-macros true]
            [flow.el :as el :include-macros true]))

#+clj
(defmacro let<< [bindings & body]
  `(l/let<< ~bindings ~@body))

#+clj
(defmacro for<< [bindings & body]
  `(f/for<< ~bindings ~@body))

#+clj
(defmacro el<< [el-stream]
  `(el/el<< ~el-stream))

#+clj
(defmacro << [stream]
  (throw (ex-info "'<<' used outside of let<</for<<" {:stream stream})))
