(ns flow.compile
  (:require [flow.util :as u]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(alias 'fp (doto 'flow.protocols create-ns))

(defmulti compile-el
  (fn [elem opts]
    (:type elem)))

(defmulti compile-value-form
  (fn [form opts]
    (:type form)))

(defn compile-value [{:keys [path] :as form} {:keys [state old-state new-state updated-vars] :as opts}]
  (let [{:keys [deps wrapped-deps declarations inline-value dynamic-value decorated-value] :as compiled-value}
        (compile-value-form form opts)
        
        value-sym (symbol path)
        downstream-value-sym (symbol (str path "-value"))]
    
    (if (empty? wrapped-deps)
      {:deps deps
       :inline-value inline-value}

      {:deps (set/union deps wrapped-deps)
       :declarations (concat declarations
                             [`(defn ~value-sym []
                                 (let [~downstream-value ~(dynamic-value)]
                                   (reify fp/DynamicValue
                                     (should-update-value? [_# updated-vars#]
                                       (u/deps-updated? ~(u/quoted-deps wrapped-deps) updated-vars#))

                                     (initial-value [_# ~state]
                                       ~(fp/initial-value-form decorated-value
                                                               downstream-value-sym
                                                               state))

                                     (updated-value [_# ~old-state ~new-state ~updated-vars]
                                       ~(fp/updated-value-form decorated-value
                                                               downstream-value-sym
                                                               old-state
                                                               new-state
                                                               updated-vars)))))])
       :dynamic-value `(~value-sym)})))

(require 'flow.compile.nodes)
(require 'flow.compile.calls)

(require 'flow.compile.simple)
(require 'flow.compile.call-values)
