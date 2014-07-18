(ns flow.compile.calls
  (:require [flow.compile :refer [compile-form]]
            [flow.bindings :as b]
            [flow.util :as u]
            [clojure.set :as set]
            [flow.protocols :as fp]))

(defmulti compile-call-form
  (fn [call opts]
    (:call-type call)))

(defmulti compile-call-form
  (fn [{:keys [call-type]} opts]
    call-type))

(require 'flow.compile.fn-call)
(require 'flow.compile.unwrap-cursor)

(defmethod compile-call-form :do [{:keys [path side-effects return]} opts]
  (let [do-el (symbol path)
        compiled-return (compile-form return opts)
        deps (:deps compiled-return)]

    (if (empty? side-effects)
      compiled-return

      {:el `(~do-el)
       :deps deps
       :declarations (concat (:declarations compiled-return)

                             [`(defn ~do-el []
                                 (let [downstream-el# ~(:el compiled-return)]
                                   
                                   (reify fp/Box
                                     (~'should-update? [_# updated-vars#]
                                       (fp/should-update? downstream-el# updated-vars#))

                                     (~'build [_# state#]
                                       ~@side-effects
                                       (fp/build downstream-el#))

                                     (~'handle-update! [_# old-state# new-state# updated-vars#]
                                       (fp/handle-update! downstream-el# old-state# new-state# updated-vars#)))))])})))

(defmethod compile-call-form :if [{:keys [path test then else]} {:keys [state] :as opts}]
  (let [if-sym (symbol path)
        compiled-test (compile-form test opts)
        [compiled-then compiled-else] (map #(compile-form % opts) [then else])
        deps (mapcat :deps [compiled-test compiled-then compiled-else])]

    {:el `(~if-sym)
     :deps deps
     :declarations (concat (mapcat :declarations [compiled-test compiled-then compiled-else])
                           [`(defn ~if-sym []
                               (flow.forms.if/if->el ~(u/quote-deps deps)
                                                     (fn [~state]
                                                       ~(:inline-value compiled-test))
                                                     ~(:el compiled-then)
                                                     ~(:el compiled-else)))])}))

(defmethod compile-call-form :let [{:keys [bindings body path]} {:keys [state] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-form body opts)

        deps (b/bindings-deps compiled-bindings compiled-body)

        let-sym (symbol path)]

    {:el `(~let-sym)
     :deps deps
     :declarations (concat (mapcat :declarations (concat compiled-bindings [compiled-body]))
                           [`(defn ~let-sym []
                               (let [~@(mapcat (juxt :bind-values->map-sym :bind-values->map) compiled-bindings)]
                                 (flow.forms.let/let->el ~(u/quote-deps deps)

                                                         ~(:el compiled-body)

                                                         (fn let-bindings-state# [~state]
                                                           (let [~@(mapcat (fn [{:keys [inline-value value-sym bind-values->map-sym]}]
                                                                             `[~value-sym ~inline-value
                                                                               ~state (merge ~state (~bind-values->map-sym ~value-sym))])

                                                                           compiled-bindings)]
                                                             ~state)))))])}))

(defmethod compile-call-form :for [{:keys [bindings body path]} {:keys [state] :as opts}]
  (let [{:keys [compiled-bindings opts]} (b/compile-bindings bindings opts)

        compiled-body (compile-form body opts)

        deps (b/bindings-deps compiled-bindings compiled-body)
        
        for-sym (symbol path)]

    {:el `(~for-sym)
     :deps deps
     :declarations (concat (:declarations compiled-body)
                           
                           [`(defn ~for-sym []
                               (let [~@(mapcat (juxt :bind-values->map-sym :bind-values->map) compiled-bindings)]
                                   
                                 (flow.forms.for/for->el ~(u/quote-deps deps)

                                                         (fn for-values# [~state]
                                                           (for [~@(mapcat (fn [{:keys [inline-value value-sym bind-values->map-sym]}]
                                                                             `[~value-sym ~inline-value
                                                                               :let [~state (merge ~state (~bind-values->map-sym ~value-sym))]])

                                                                           compiled-bindings)]

                                                             {:keys (map (fn [key-fn# value#]
                                                                           (or (when key-fn#
                                                                                 (key-fn# value#))
                                                                               (::f/id value#)
                                                                               (:id value#)
                                                                               value#))
                                                                           
                                                                         [~@(map :key-fn bindings)]
                                                                         [~@(map :value-sym compiled-bindings)])
                                                              :state ~state}))
                                                           
                                                         (fn []
                                                           ~(:el compiled-body)))))])}))

(defmethod compile-form :call [call opts]
  (compile-call-form call opts))

(defmethod compile-form :call [call opts]
  (compile-call-form call opts))
