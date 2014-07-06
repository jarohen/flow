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

(comment
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

  (defmethod compile-call :let [{:keys [bindings body]} {:keys [state-sym old-state-sym new-state-sym updated-vars-sym] :as opts}]
    (if (empty? bindings)
      (compile-el body opts)
      
      (let [[{:keys [bind value path]} & more-bindings] bindings
            compiled-value (compile-el value opts)
            compiled-body (compile-el body (cond-> opts
                                             (not-empty (:deps compiled-value)) (update-in [:dynamic-syms] conj bind)))

            !el-return-sym (symbol (str path "-el"))
            !last-value-sym (symbol (str "!" path "-last-value"))
            eval-value-sym (symbol (str "eval-" path "-value"))

            {body-el-bindings :el-bindings
             {init-body-sym :init-sym
              body-on-update-sym :on-update-sym} :syms}
            (compile-block body compiled-body !el-return-sym opts)]
        
        {:deps (set (concat (:deps compiled-value)
                            (disj (:deps compiled-body) bind)))

         :as-value `(let [~bind ~(:as-value compiled-value)]
                      ~(:as-value compiled-body))

         :el-bindings (concat [[!el-return-sym `(atom (fd/null-elem))]
                               [!last-value-sym `(atom nil)]
                               
                               [eval-value-sym `(fn [~state-sym]
                                                  ~(:as-value compiled-value))]]
                              
                              body-el-bindings)
         
         :el-return `(deref ~!el-return-sym)

         :el-init (when (empty? (:deps compiled-value))
                    `[(let [new-value# (~eval-value-sym {})]
                        (reset! ~!last-value-sym new-value#)
                        (let [~bind new-value#]
                          (~init-body-sym {})))])

         :on-update (if (empty? (:deps compiled-value))
                      [(body-on-update-sym old-state-sym new-state-sym updated-vars-sym)]

                      `(if (some #(contains? #{~@(for [dep (:deps compiled-value)]
                                                   `(quote ~dep))
                                               :all}
                                             %)
                                 ~updated-vars-sym)
                         
                         [(let [old-val# @~!last-value-sym
                                new-val# (~eval-value-sym ~new-state-sym)]
                            (reset! ~!last-value-sym new-val#)
                            
                            (cond
                             (contains? ~updated-vars-sym :all)
                             (do
                               (~init-body-sym (assoc ~new-state-sym
                                                 (quote ~bind) new-val#))
                               
                               (~body-on-update-sym (assoc ~old-state-sym
                                                      (quote ~bind) old-val#)

                                                    (assoc ~new-state-sym
                                                      (quote ~bind) new-val#)
                                                    
                                                    #{:all}))
                             
                             (not= old-val# new-val#)
                             (~body-on-update-sym (assoc ~old-state-sym
                                                    (quote ~bind) old-val#)

                                                  (assoc ~new-state-sym
                                                    (quote ~bind) new-val#)
                                                  
                                                  (conj ~updated-vars-sym (quote ~bind)))

                             ;; value unchanged
                             :else (~body-on-update-sym ~old-state-sym ~new-state-sym ~updated-vars-sym)))]
                         
                         [(let [~bind @~!last-value-sym]
                            (~body-on-update-sym ~old-state-sym ~new-state-sym ~updated-vars-sym))]))}))))

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
                :updated-vars 'flow-test-updated-vars}]
      (-> (compile-el (flow.parse/parse-form '(let [primary (<<! !primary)]
                                                [:div
                                                 [:h1 {::f/style {:color primary}}]
                                                 (if (= primary "#000")
                                                   (do
                                                     [:p "is black"]))])
                                             {:elem? true
                                              :path "flow-test"})
                      syms)
          #_(render-el syms)))))

(defmethod compile-el :call [call opts]
  (compile-call call opts))
