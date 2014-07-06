(ns flow.compile.calls
  (:require [flow.compile :refer [compile-el]]
            [flow.compile.update :refer [on-update-form]]))

(alias 'fd (doto 'flow.dom create-ns))
(alias 'f (doto 'flow.core create-ns))

(defmulti compile-call
  (fn [call opts]
    (:call-type call)))

(defmethod compile-call :do [{:keys [side-effects return]} opts]
  (let [compiled-return (compile-el return opts)]
    {:el-bindings (:el-bindings compiled-return)
     :el-init `[~@side-effects ~@(:el-init compiled-return)]
     :el-return (:el-return compiled-return)

     :deps (:deps compiled-return)

     ;; TODO not sure about this...
     :as-value (when-let [as-val (:as-value compiled-return)]
                 `(do
                    ~@side-effects
                    ~as-val))
     
     :on-update (:on-update compiled-return)}))

(defn init-bindings [bindings]
  (->> (for [[bind value] bindings]
         `[(quote ~bind) ~value])
      
       (into {})))

(defn read-bindings [!bindings-sym bindings]
  (mapcat (fn [[bind _]]
            [bind `(get @~!bindings-sym (quote ~bind))])
          
          bindings))

(defn init-block [!el-return-sym compiled-block !bindings-sym]
  (let [bindings (:el-bindings compiled-block)]
    `(do
       (reset! ~!bindings-sym ~(init-bindings bindings))
       (let [~@(read-bindings !bindings-sym bindings)
             $elem# ~(:el-return compiled-block)]
         (fd/swap-elem! @~!el-return-sym $elem#)
         (reset! ~!el-return-sym $elem#)
         ~@(:el-init compiled-block)))))

(defn compile-block [form compiled-form !el-return-sym
                     & [{:keys [state-sym old-state-sym new-state-sym updated-vars-sym] :as opts}]]
  (let [{:keys [path]} form
        !block-bindings-sym (symbol (str path "-binds"))
        init-block-sym (symbol (str path "-init"))
        block-on-update-sym (symbol (str path "-on-update"))]

    {:el-bindings [[!block-bindings-sym `(atom nil)]
                   
                   [init-block-sym `(fn [~state-sym]
                                      ~(init-block !el-return-sym compiled-form !block-bindings-sym))]
                   
                   [block-on-update-sym `(fn [~old-state-sym ~new-state-sym ~updated-vars-sym]
                                           (let [~@(read-bindings !block-bindings-sym (:el-bindings compiled-form))]
                                             ~(on-update-form compiled-form opts)))]]
     
     :syms {:init-sym init-block-sym
            :on-update-sym block-on-update-sym}}))

(defmethod compile-call :if [{:keys [path test then else]} {:keys [el-sym state-sym old-state-sym new-state-sym updated-vars-sym] :as opts}]
  (let [el-sym (symbol (str path "-el"))
        
        [compiled-test compiled-then compiled-else] (map #(compile-el % opts) [test then else])

        !el-return-sym (symbol (str "!" path "-return"))

        eval-test-sym (symbol (str "eval-" path "-test"))
        !last-test-value-sym (symbol (str "!" path "-last-test-value"))
        
        {then-el-bindings :el-bindings
         {init-then-sym :init-sym
          then-on-update-sym :on-update-sym} :syms}
        (compile-block then compiled-then !el-return-sym opts)

        {else-el-bindings :el-bindings
         {init-else-sym :init-sym
          else-on-update-sym :on-update-sym} :syms}
        (compile-block else compiled-else !el-return-sym opts)

        update-branches-sym (symbol (str path "-update-branches!"))]

    {:deps (set (mapcat :deps [compiled-test compiled-then compiled-else]))
     
     :as-value `(if ~(:as-value compiled-test)
                  ~(:as-value compiled-then)
                  ~(:as-value compiled-else))

     :el-bindings (concat [[!el-return-sym `(atom (fd/null-elem))]
                           [!last-test-value-sym `(atom nil)]

                           [eval-test-sym `(fn [~state-sym]
                                             ~(:as-value compiled-test))]]

                          then-el-bindings
                          else-el-bindings
                          
                          [[update-branches-sym `(fn [test-value# ~old-state-sym ~new-state-sym ~updated-vars-sym]
                                                   (if test-value#
                                                     (~then-on-update-sym ~old-state-sym ~new-state-sym ~updated-vars-sym)
                                                     (~else-on-update-sym ~old-state-sym ~new-state-sym ~updated-vars-sym)))]])
     
     :el-return `(deref ~!el-return-sym)

     :el-init (when (empty? (:deps compiled-test))
                `[(let [new-test# (~eval-test-sym nil)]
                    (reset! ~!last-test-value-sym new-test#)
                    (if new-test#
                      (~init-then-sym {})
                      (~init-else-sym {})))])
     
     :on-update (if (not-empty (:deps compiled-test))
                  `[(if (some #(contains? #{~@(for [dep (:deps compiled-test)]
                                                `(quote ~dep))
                                            :all}
                                          %)
                              ~updated-vars-sym)
                      (let [old-test# @~!last-test-value-sym
                            new-test# (~eval-test-sym ~new-state-sym)]
                        (if (or (contains? ~updated-vars-sym :all)
                                (not= (boolean old-test#) (boolean new-test#)))
                             
                          (do
                            (reset! ~!last-test-value-sym new-test#)
                            (if new-test#
                              (~init-then-sym ~new-state-sym)
                              (~init-else-sym ~new-state-sym))

                            ;; test value changed, branch change
                            (~update-branches-sym new-test# ~old-state-sym ~new-state-sym #{:all}))

                          ;; test value unchanged
                          (~update-branches-sym new-test# ~old-state-sym ~new-state-sym ~updated-vars-sym)))

                      ;; test deps not updated
                      (~update-branches-sym @~!last-test-value-sym ~old-state-sym ~new-state-sym ~updated-vars-sym))]

                  ;; test has no deps
                  `[(~update-branches-sym @~!last-test-value-sym ~old-state-sym ~new-state-sym ~updated-vars-sym)])}))

(defmethod compile-call :fn-decl [{:keys [fn-decl]} opts]
  {:as-value fn-decl})

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

(defmethod compile-call :let [{:keys [bindings body path]} {:keys [old-state-sym new-state-sym updated-vars-sym] :as opts}]
  (if (empty? bindings)
    (compile-el body opts)
    
    (let [[{:keys [bind value]} & more-bindings] bindings
          compiled-value (compile-el value opts)
          compiled-body (compile-el body (cond-> opts
                                           (not-empty (:deps compiled-value)) (update-in [:dynamic-syms] conj bind)))

          !el-return-sym (symbol (str path "-el"))

          {body-el-bindings :el-bindings
           {init-body-sym :init-sym
            body-on-update-sym :on-update-sym} :syms}
          (compile-block body compiled-body !el-return-sym opts)]
      
      {:deps (set (concat (:deps compiled-value)
                          (disj (:deps compiled-body) bind)))

       :as-value `(let [~bind ~(:as-value compiled-value)]
                    ~(:as-value compiled-body))

       :el-bindings (concat [[!el-return-sym `(atom (fd/null-elem))]]

                            body-el-bindings)
     
       :el-return `(deref ~!el-return-sym)

       :el-init (when (empty? (:deps compiled-value))
                  `[(let [~bind ~(:as-value compiled-value)]
                      (~init-body-sym {}))])

       :on-update (if (empty? (:deps compiled-value))
                    [(body-on-update-sym old-state-sym new-state-sym updated-vars-sym)]

                    `(if (some #(contains? #{~@(for [dep (:deps compiled-value)]
                                                 `(quote ~dep))
                                             :all}
                                           %)
                               ~updated-vars-sym)
                       
                       
                       
                       [(let [~bind ~(:as-value compiled-value)]
                          (~body-on-update-sym ~old-state-sym ~new-state-sym ~updated-vars-sym))]))})))

(defmethod compile-call :fn-call [{:keys [args]} opts]
  (let [compiled-args (map #(compile-el % opts) args)
        deps (set (mapcat :deps compiled-args))
        value (map :as-value compiled-args)]
    {:deps deps
     :as-value value
     :el-return value}))

(defmethod compile-call :unwrap-cursor [{:keys [cursor path]}
                                        {:keys [dynamic-syms state-sym new-state-sym] :as opts}]
  (let [!el-sym (symbol (str path "-!el"))
        el-value-sym (symbol (str path "-value"))]
    
    {:deps #{cursor}
     :as-value `(get ~state-sym (quote ~cursor))

     :el-bindings `[[~!el-sym (atom (fd/null-elem))]]
     :el-return `(deref ~!el-sym)
     :on-update [`(let [~el-value-sym (get ~new-state-sym (quote ~cursor))
                        ~el-value-sym (if (.-nodeType ~el-value-sym)
                                        ~el-value-sym
                                        (-> ~el-value-sym
                                            (str)
                                            (js/document.createTextNode)))]
                    (fd/swap-elem! @~!el-sym ~el-value-sym)
                    (reset! ~!el-sym ~el-value-sym))]}))

(defmethod compile-call :wrap-cursor [{:keys [cursor]}
                                      {:keys [dynamic-syms state-sym updated-vars-sym]
                                       :as opts}]
  )

(comment
  (require 'flow.parse)

  (defn test-compile []
    (let [syms {:state-sym 'flow-test-state
                :old-state-sym 'flow-test-old-state
                :new-state-sym 'flow-test-new-state
                :updated-vars-sym 'flow-test-updated-vars}]
      (-> (compile-el (flow.parse/parse-form '(if (<<! !show-heading?)
                                                (do
                                                  [:h1 {::f/style {:color (:secondary (<<! !colors))
                                                                   :padding "0.5em"
                                                                   :background-color (:primary (<<! !colors))}
                                                        ;; ::f/on {:click (fn [e]
                                                        ;;                  (a/put! change-colors-ch :change!))}
                                                        }
                                                   (<<! !heading)]))
                                             {:elem? true
                                              :path "flow-test"})
                      syms)
          #_(render-el syms)))

    ))

(defmethod compile-el :call [call opts]
  (compile-call call opts))

