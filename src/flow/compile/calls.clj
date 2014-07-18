(ns flow.compile.calls
  (:require [flow.compile :refer [compile-form]]
            [flow.bindings :as b]
            [flow.util :as u]
            [clojure.set :as set]
            [flow.protocols :as fp]))

(defmulti compile-call-form
  (fn [call opts]
    (:call-type call)))

#_(require 'flow.compile.fn-call)
#_(require 'flow.compile.unwrap-cursor)

(defmethod compile-form :call [call opts]
  (compile-call-form call opts))
