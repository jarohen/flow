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
    (if (empty? deps)
      {:inline-value inline-value}

      {:deps deps
       :value `(~decl)
       :declarations (conj declarations
                           (let [updated-vars (gensym "updated-vars")]
                             `(defn ~decl []
                                (reify fp/DynamicValue
                                  (~'should-update-value? [_# ~updated-vars]
                                    ~(u/deps->should-update deps updated-vars))

                                  (~'current-value [_# ~state]
                                    ~inline-value)))))})))

(require 'flow.compile.node)
(require 'flow.compile.simple)
(require 'flow.compile.calls)
(require 'flow.compile.call-values)


(require 'flow.parse)

(let [syms {:state 'flow-test-state
            :old-state 'flow-test-old-state
            :new-state 'flow-test-new-state
            :updated-vars 'flow-test-updated-vars}]
  (-> (compile-value (flow.parse/parse-form '(if (+ 1 1)
                                               (<<! !primary))
                                            {:elem? false
                                             :path "flow-test"})

                  
                     syms)
      #_(render-el syms)
      :declarations
      first
      (nth 3)
      (nth 3)))




