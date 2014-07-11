(ns flow.compile
  (:require [flow.util :as u]))

(alias 'fp (doto 'flow.protocols create-ns))

(defmulti compile-el
  (fn [elem opts]
    (:type elem)))

(defmulti compile-value-form
  (fn [elem opts]
    (:type elem)))

(defn compile-value [{:keys [path] :as elem} opts]
  (let [state (symbol (str path "-state"))
        decl (symbol (str path "-value"))
        {:keys [deps inline-value declarations]} (compile-value-form elem (assoc opts :state state))]
    {:deps deps
     :value `(~decl)
     :declarations (concat declarations
                           [(let [updated-vars (gensym "updated-vars")]
                              `(defn ~decl []
                                 (reify fp/DynamicValue
                                   (~'should-update-value? [_# ~updated-vars]
                                     ~(u/deps->should-update deps updated-vars))

                                   (~'current-value [_# ~state]
                                     ~inline-value))))])
     :inline-value inline-value}))

(require 'flow.compile.node)
(require 'flow.compile.simple)
(require 'flow.compile.calls)
(require 'flow.compile.call-values)





