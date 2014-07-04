(ns flow.compile.calls
  (:require [flow.compile :refer [compile-el]]
            [flow.compile.update :refer [on-update-form]]))

(alias 'fd (doto 'flow.dom create-ns))


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

(defmethod compile-call :if [{:keys [test then else]} {:keys [state-sym old-state-sym new-state-sym updated-var-sym] :as opts}]
  (let [[compiled-test compiled-then compiled-else] (map #(compile-el % opts) [test then else])

        !el-sym (gensym (str "if"))

        !then-bindings-sym (symbol (str !el-sym "-then-binds"))
        !else-bindings-sym (symbol (str !el-sym "-else-binds"))

        eval-test-sym (symbol (str !el-sym "-eval-test"))
        !last-test-value-sym (symbol (str "!" !el-sym "-last-test-value"))
        
        init-then-sym (symbol (str !el-sym "-init-then"))
        init-else-sym (symbol (str !el-sym "-init-else"))
        
        then-on-update-sym (symbol (str !el-sym "-then-on-update"))
        else-on-update-sym (symbol (str !el-sym "-else-on-update"))

        update-branches-sym (gensym "update-branches")]
    
    (letfn [(init-branch [compiled-branch !bindings-sym]
              (let [bindings (:el-bindings compiled-branch)]
                `(do
                   (reset! ~!bindings-sym ~(init-bindings bindings))
                   (let [~@(read-bindings !bindings-sym bindings)
                         $elem# ~(:el-return compiled-branch)]
                     (fd/swap-elem! @~!el-sym $elem#)
                     (reset! ~!el-sym $elem#)
                     ~@(:el-init compiled-branch)))))]
      
      {:deps (set (mapcat :deps [compiled-test compiled-then compiled-else]))
       :as-value `(if ~(:as-value compiled-test)
                    ~(:as-value compiled-then)
                    ~(:as-value compiled-else))

       
       :el-bindings `[[~!el-sym (atom (fd/null-elem))]
                      [~!last-test-value-sym (atom nil)]
                      [~!then-bindings-sym (atom nil)]
                      [~!else-bindings-sym (atom nil)]

                      [~init-then-sym (fn [~state-sym]
                                        ~(init-branch compiled-then !then-bindings-sym))]
                      
                      [~init-else-sym (fn [~state-sym]
                                        ~(init-branch compiled-else !else-bindings-sym))]
                      
                      [~eval-test-sym (fn [~state-sym]
                                        ~(:as-value compiled-test))]
                      
                      [~then-on-update-sym (fn [~old-state-sym ~new-state-sym ~updated-var-sym]
                                             (let [~@(read-bindings !then-bindings-sym (:el-bindings compiled-then))]
                                               ~(on-update-form compiled-then opts)))]
                      
                      [~else-on-update-sym (fn [~old-state-sym ~new-state-sym ~updated-var-sym]
                                             (let [~@(read-bindings !else-bindings-sym (:el-bindings compiled-else))]
                                               ~(on-update-form compiled-else opts)))]
                      
                      [~update-branches-sym (fn [test-value# ~old-state-sym ~new-state-sym ~updated-var-sym]
                                              (if test-value#
                                                (~then-on-update-sym ~old-state-sym ~new-state-sym ~updated-var-sym)
                                                (~else-on-update-sym ~old-state-sym ~new-state-sym ~updated-var-sym)))]]
       
       :el-return `(deref ~!el-sym)

       :el-init `[~@(when (empty? (:deps compiled-test))
                      [`(let [new-test# (~eval-test-sym nil)]
                          (reset! ~!last-test-value-sym new-test#)
                          (if new-test#
                            (~init-then-sym {})
                            (~init-else-sym {})))])]
       
       :on-update `[~@(if (not-empty (:deps compiled-test))
                        [`(if (contains? #{~@(for [dep (:deps compiled-test)]
                                               `(quote ~dep))
                                           :all}
                                         ~updated-var-sym)
                            (let [old-test# @~!last-test-value-sym
                                  new-test# (~eval-test-sym ~new-state-sym)]
                              (if (or (= ~updated-var-sym :all)
                                      (not= (boolean old-test#) (boolean new-test#)))
                                
                                (do
                                  (reset! ~!last-test-value-sym new-test#)
                                  (if new-test#
                                    (~init-then-sym ~new-state-sym)
                                    (~init-else-sym ~new-state-sym))

                                  ;; test value changed, branch change
                                  (~update-branches-sym new-test# ~old-state-sym ~new-state-sym :all))

                                ;; test value unchanged
                                (~update-branches-sym new-test# ~old-state-sym ~new-state-sym ~updated-var-sym)))

                            ;; test deps not updated
                            (~update-branches-sym @~!last-test-value-sym ~old-state-sym ~new-state-sym ~updated-var-sym))]

                        ;; test has no deps
                        `[(~update-branches-sym @~!last-test-value-sym ~old-state-sym ~new-state-sym ~updated-var-sym)])]})))

(comment
  (require 'flow.parse)
  (require 'flow.render)
  (let [syms {:state-sym (gensym "state")
              :old-state-sym (gensym "old-state")
              :new-state-sym (gensym "new-state")
              :updated-var-sym (gensym "updated-var")}]
    (-> '(do
           [:h1 {:flow.core/style {:color (:secondary (<<! !colors))
                                   :background-color (:secondary (<<! !colors))}}
            "Hello world!"])
        (flow.parse/parse-form {:elem? true})
        (compile-el syms)
        #_(flow.render/render-el syms)))

  )

(defmethod compile-call :let [{:keys [bindings side-effects body]}]
  )

(defmethod compile-call :fn-call [{:keys [args]} opts]
  (let [compiled-args (map #(compile-el % opts) args)
        deps (set (mapcat :deps compiled-args))
        value (map :as-value compiled-args)]
    {:deps deps
     :as-value value
     :el-return value}))

(defmethod compile-call :unwrap-cursor [{:keys [cursor]}
                                        {:keys [dynamic-syms state-sym new-state-sym updated-var-sym]
                                         :as opts}]
  (let [unshadowed-sym (if-let [{:keys [unshadowed-sym]} (get dynamic-syms cursor)]
                         unshadowed-sym
                         cursor)
        !el-sym (gensym (str cursor))
        el-value-sym (gensym (str cursor "-value"))]
    
    {:deps #{unshadowed-sym}
     :as-value `(get ~state-sym (quote ~unshadowed-sym))

     :el-bindings `[[~!el-sym (atom (fd/null-elem))]]
     :el-return `(deref ~!el-sym)
     :on-update [`(let [~el-value-sym (get ~new-state-sym (quote ~unshadowed-sym))
                        ~el-value-sym (if (.-nodeType ~el-value-sym)
                                        ~el-value-sym
                                        (-> ~el-value-sym
                                            (str)
                                            (js/document.createTextNode)))]
                    (fd/swap-elem! @~!el-sym ~el-value-sym)
                    (reset! ~!el-sym ~el-value-sym))]}))

(defmethod compile-call :wrap-cursor [{:keys [cursor]}
                                      {:keys [dynamic-syms state-sym updated-var-sym]
                                       :as opts}]
  )

(comment
  (require 'flow.parse)

  (compile-el (flow.parse/parse-form '(if (+ 1 1)
                                        [:div {:flow.core/style {:color (<<! !color)}}])
                                     {:elem? true})
              {:state-sym (gensym "state")
               :old-state-sym (gensym "old-state")
               :new-state-sym (gensym "new-state")
               :updated-var-sym (gensym "updated-var")}))

(defmethod compile-el :call [call opts]
  (compile-call call opts))

