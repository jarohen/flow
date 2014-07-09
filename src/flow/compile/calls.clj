(ns flow.compile.calls
  (:require [flow.compile :refer [compile-el compile-value]]
            [flow.bindings :as b]
            [flow.util :as u]))

(alias 'fd (doto 'flow.dom create-ns))
(alias 'f (doto 'flow.core create-ns))
(alias 'fp (doto 'flow.protocols create-ns))

(defmulti compile-call
  (fn [call opts]
    (:call-type call)))

(defmethod compile-call :do [{:keys [path side-effects return]} opts]
  (let [el (symbol path)
        compiled-return (compile-el return opts)
        deps (:deps compiled-return)]

    (if (empty? side-effects)
      compiled-return

      {:el `(~el)
       :deps deps
       :declarations (concat (:declarations compiled-return)

                             [`(defn ~el []
                                 (let [downstream-el# ~(:el compiled-return)]
                                   
                                   (reify fp/DynamicElement
                                     (~'should-update-el? [_# updated-vars#]
                                       (fp/should-update-el? downstream-el# updated-vars#))

                                     (~'build-element [_# state#]
                                       ~@side-effects
                                       (fp/build-element downstream-el#))

                                     (~'handle-update! [_# old-state# new-state# updated-vars#]
                                       (fp/handle-update! downstream-el# old-state# new-state# updated-vars#)))))])})))

(defmethod compile-call :if [{:keys [path test then else]} {:keys [state old-state new-state updated-vars] :as opts}]
  (let [if-sym (symbol path)
        compiled-test (compile-value test opts)
        [compiled-then compiled-else] (map #(compile-el % opts) [then else])
        deps (mapcat :deps [compiled-test compiled-then compiled-else])]

    {:el `(~if-sym)
     :deps deps
     :declarations (concat (mapcat :declarations [compiled-test compiled-then compiled-else])
                           [`(defn ~if-sym []
                               (let [!placeholder-el# (atom nil)
                                     test-value# ~(:value compiled-test)
                                     then-branch# ~(:el compiled-then)
                                     else-branch# ~(:el compiled-else)
                                     
                                     !last-test-value# (atom nil)]
                                 
                                 (reify fp/DynamicElement
                                   (~'should-update-el? [_# ~updated-vars]
                                     ~(u/deps->should-update deps updated-vars))

                                   (~'build-element [_# state#]
                                     (let [initial-value# (fp/current-value test-value# state#)
                                           initial-el# (if (fp/current-value test-value# state#)
                                                         (fp/build-element then-branch# state#)
                                                         (fp/build-element else-branch# state#))]
                                       (reset! !last-test-value# initial-value#)
                                       (reset! !placeholder-el# initial-el#)
                                       initial-el#))

                                   (~'handle-update! [_# old-state# new-state# updated-vars#]
                                     (letfn [(update-branch# [test#]
                                               (if test#
                                                 (when (fp/should-update-el? then-branch# updated-vars#)
                                                   (fp/handle-update! then-branch# old-state# new-state# updated-vars#))

                                                 (when (fp/should-update-el? else-branch# updated-vars#)
                                                   (fp/handle-update! else-branch# old-state# new-state# updated-vars#))))]
                                       
                                       (if (fp/should-update-value? test-value# updated-vars#)
                                         (let [old-test# @!last-test-value#
                                               new-test# (fp/current-value test-value# new-state#)]
                                           (if (not= (boolean old-test#) (boolean new-test#))
                                             (do
                                               (reset! !last-test-value# new-test#)

                                               (let [new-el# (if new-test#
                                                               (fp/build-element then-branch# new-state#)
                                                               (fp/build-element else-branch# new-state#))]
                                                 (fd/swap-elem! @!placeholder-el# new-el#)
                                                 (reset! !placeholder-el# new-el#)))
                                             
                                             (update-branch# @!last-test-value#)))

                                         (update-branch# @!last-test-value#)))))))])}))

(defmethod compile-call :let [{:keys [bindings body]} {:keys [state old-state new-state updated-vars] :as opts}]
  (if (empty? bindings)
    (compile-el body opts)
    
    (let [[{:keys [bind value path]} & more-bindings] bindings

          compiled-value (compile-value value opts)

          dynamic-value? (empty (:deps compiled-value))
          
          compiled-body (compile-call {:call-type :let
                                       :bindings more-bindings
                                       :body body}

                                      (update-in opts
                                                 [(if (empty (:deps compiled-value))
                                                    :local-syms
                                                    :dynamic-syms)]
                                                 conj bind))
          
          let-sym (symbol path)

          deps (mapcat :deps [compiled-value compiled-body])]

      {:el `(~let-sym)
       :deps deps
       :declarations (concat (mapcat :declarations [compiled-value compiled-body])
                             [`(defn ~let-sym []
                                 (let [!placeholder-el# (atom nil)
                                       value# ~(:value compiled-value)
                                       body# ~(:el compiled-body)
                                       
                                       !last-value# (atom nil)]
                                   
                                   (reify fp/DynamicElement
                                     (~'should-update-el? [_# ~updated-vars]
                                       ~(u/deps->should-update deps updated-vars))

                                     (~'build-element [_# state#]
                                       (let [initial-value# (fp/current-value value# state#)
                                             initial-el# (fp/build-element body# (assoc state# (quote ~bind) initial-value#))]
                                         
                                         (reset! !last-value# initial-value#)
                                         (reset! !placeholder-el# initial-el#)
                                         initial-el#))

                                     (~'handle-update! [_# old-state# new-state# updated-vars#]
                                       (letfn [(update-body# [old-value# new-value# updated-vars#]
                                                 (when (fp/should-update-el? body# updated-vars#)
                                                   (fp/handle-update! body#
                                                                      (assoc old-state#
                                                                        (quote ~bind) old-value#)
                                                                      (assoc new-state#
                                                                        (quote ~bind) new-value#)
                                                                      updated-vars#)))]
                                         
                                         (if (fp/should-update-value? value# updated-vars#)
                                           (let [old-value# @!last-value#
                                                 new-value# (fp/current-value value# new-state#)]
                                             (if (not= old-value# new-value#)
                                               (do
                                                 (reset! !last-value# new-value#)
                                                 (update-body# @!last-value# new-value# (conj updated-vars# (quote ~bind))))
                                               
                                               (update-body# @!last-value# @!last-value# updated-vars#)))

                                           (update-body# @!last-value# @!last-value# updated-vars#)))))))])})))

(comment
  (require 'flow.parse)
  (require 'flow.render)
  (def foo-syms
    (let [el-sym (gensym "flow-el")]
      {:el-sym el-sym
       :state-sym (symbol (str el-sym "-state"))
       :!state-sym (symbol (str el-sym "-!state"))
       :old-state-sym (symbol (str el-sym "-old-state"))
       :new-state-sym (symbol (str el-sym "-new-state"))
       :updated-vars-sym (symbol (str el-sym "-updated-vars"))}))
  
  (flow.render/render-el
   (compile-el
    (flow.parse/parse-form '(let [x 4]
                              [:h1 {:data-flow x}
                               "Hello world!"])
                           {:elem? true
                            :path (str (gensym "flow-el"))})
    foo-syms)
   foo-syms)
  )

(defmethod compile-call :fn-call [{:keys [args]} opts]
  ;; TODO
  )

(defmethod compile-call :unwrap-cursor [{:keys [cursor path]}
                                        {:keys [dynamic-syms state-sym new-state-sym updated-vars] :as opts}]
  (let [el (symbol (str path))
        deps #{cursor}]
    
    {:el `(~el)
     :deps #{cursor}
     :declarations [`(defn ~el []
                       (let [!el# (atom nil)]
                         (reify fp/DynamicElement
                           (~'should-update-el? [_# ~updated-vars]
                             ~(u/deps->should-update deps updated-vars))

                           (~'build-element [_# state#]
                             (let [new-el# (get state# (quote ~cursor))
                                   new-el# (if (.-nodeType new-el#)
                                             new-el#
                                             (-> new-el#
                                                 (str)
                                                 (js/document.createTextNode)))]
                               
                               (when-let [old-el# @!el#]
                                 (fd/swap-elem! old-el# new-el#))
                               
                               (reset! !el# new-el#)
                               new-el#))
                           
                           (~'handle-update! [this# old-state# new-state# updated-vars#]
                             (fp/build-element this# new-state#)))))]}))

(defmethod compile-call :wrap-cursor [{:keys [cursor]}
                                      {:keys [dynamic-syms state-sym updated-vars-sym]
                                       :as opts}]
  )

(comment
  (require 'flow.parse)

  (defn test-compile []
    (let [syms {:state 'flow-test-state
                :old-state 'flow-test-old-state
                :new-state 'flow-test-new-state
                :updated-vars 'flow-test-updated-vars
                :dynamic-syms #{}
                :local-syms #{}}]
      (-> (compile-el (flow.parse/parse-form '(let [primary (<<! !primary)]
                                                [:div
                                                 [:h1 {::f/style {:color primary}}]
                                                 #_(if (= primary "#000")
                                                     (do
                                                       [:p "is black"]))])
                                             {:elem? true
                                              :path "flow-test"})
                      syms)
          #_(render-el syms)))

    {:el (flow-test-let-0), :deps (!primary),
     :declarations ((clojure.core/defn flow-test-let-bind-0-unwrap-cursor-!primary-value [] (clojure.core/reify flow.protocols/DynamicValue (should-update-value? [___10581__auto__ updated-vars11384] (clojure.core/boolean (clojure.core/some (fn* [p1__10545__10546__auto__] (clojure.core/contains? #{(quote !primary)} p1__10545__10546__auto__)) updated-vars11384))) (current-value [___10581__auto__ flow-test-let-bind-0-unwrap-cursor-!primary-state] (clojure.core/get flow-test-let-bind-0-unwrap-cursor-!primary-state (quote !primary)))))
                    (clojure.core/defn flow-test-let-body-div-node [] (clojure.core/let [binds11387 (clojure.core/atom nil)] (clojure.core/reify flow.protocols/DynamicElement (should-update-el? [___10700__auto__ flow-test-updated-vars] nil) (build-element [___10700__auto__ flow-test-state] (clojure.core/reset! binds11387 {(quote elem11385) (js/document.createElement "div"), (quote elem11386) (js/document.createElement "h1")}) (clojure.core/let [elem11385 (clojure.core/get (clojure.core/deref binds11387) (quote elem11385)) elem11386 (clojure.core/get (clojure.core/deref binds11387) (quote elem11386))] (flow.dom/set-style! elem11386 :color (clojure.core/get nil (quote primary))) (.appendChild elem11385 elem11386) elem11385)) (handle-update! [___10700__auto__ flow-test-old-state flow-test-new-state flow-test-updated-vars] (clojure.core/let [elem11385 (clojure.core/get (clojure.core/deref binds11387) (quote elem11385)) elem11386 (clojure.core/get (clojure.core/deref binds11387) (quote elem11386))])))))
                    (clojure.core/defn flow-test-let-0 [] (clojure.core/let [!placeholder-el__11266__auto__ (clojure.core/atom nil) value__11267__auto__ (flow-test-let-bind-0-unwrap-cursor-!primary-value) body__11268__auto__ (flow-test-let-body-div-node) !last-value__11269__auto__ (clojure.core/atom nil)] (clojure.core/reify flow.protocols/DynamicElement (should-update-el? [___11270__auto__ flow-test-updated-vars] (clojure.core/boolean (clojure.core/some (fn* [p1__10545__10546__auto__] (clojure.core/contains? #{(quote !primary)} p1__10545__10546__auto__)) flow-test-updated-vars))) (build-element [___11270__auto__ state__11271__auto__] (clojure.core/let [initial-value__11272__auto__ (flow.protocols/current-value value__11267__auto__ state__11271__auto__) initial-el__11273__auto__ (flow.protocols/build-element body__11268__auto__ (clojure.core/assoc state__11271__auto__ primary initial-value__11272__auto__))] (clojure.core/reset! !last-value__11269__auto__ initial-value__11272__auto__) (clojure.core/reset! !placeholder-el__11266__auto__ initial-el__11273__auto__) initial-el__11273__auto__)) (handle-update! [___11270__auto__ old-state__11274__auto__ new-state__11275__auto__ updated-vars__11276__auto__] (clojure.core/letfn [(update-body__11277__auto__ [old-value__11278__auto__ new-value__11279__auto__ updated-vars__11276__auto__] (clojure.core/when (flow.protocols/should-update-el? body__11268__auto__ updated-vars__11276__auto__) (flow.protocols/handle-update! body__11268__auto__ (clojure.core/assoc old-state__11274__auto__ primary old-value__11278__auto__) (clojure.core/assoc new-value__11279__auto__ primary new-state__11275__auto__) updated-vars__11276__auto__)))] (if (flow.protocols/should-update-value? value__11267__auto__ updated-vars__11276__auto__) (clojure.core/let [old-value__11278__auto__ (clojure.core/deref !last-value__11269__auto__) new-value__11279__auto__ (flow.protocols/current-value value__11267__auto__ new-state__11275__auto__)] (if (clojure.core/not= old-value__11278__auto__ new-value__11279__auto__) (do (clojure.core/reset! !last-value__11269__auto__ new-value__11279__auto__) (update-branch__11280__auto__ (clojure.core/deref !last-value__11269__auto__) new-value__11279__auto__ (clojure.core/conj updated-vars__11276__auto__ primary))) (update-branch__11280__auto__ (clojure.core/deref !last-value__11269__auto__) (clojure.core/deref !last-value__11269__auto__) updated-vars__11276__auto__))) (update-branch__11280__auto__ (clojure.core/deref !last-value__11269__auto__) (clojure.core/deref !last-value__11269__auto__) updated-vars__11276__auto__)))))))
                    )}))

(defmethod compile-el :call [call opts]
  (compile-call call opts))
