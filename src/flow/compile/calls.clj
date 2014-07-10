(ns flow.compile.calls
  (:require [flow.compile :refer [compile-el compile-value]]
            [flow.bindings :as b]
            [flow.util :as u]
            [clojure.set :as set]))

(alias 'fd (doto 'flow.dom create-ns))
(alias 'f (doto 'flow.core create-ns))
(alias 'fp (doto 'flow.protocols create-ns))
(alias 'set (doto 'clojure.set create-ns))

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

(defn destructuring-bind-syms [bind]
  (letfn [(dbs* [bind] (cond
                        (map? bind) (let [{:keys [as]
                                           ks :keys} bind]
                                      (concat (mapcat dbs* ks)
                                              (dbs* as)
                                              (mapcat dbs* (keys (dissoc bind :keys :as)))))
                        (vector? bind) (disj (set (mapcat dbs* bind)) '&)
                        (symbol? bind) [(symbol (name bind))]))]
    (set (dbs* bind))))

(defmethod compile-call :let [{:keys [bindings body]} {:keys [state old-state new-state updated-vars] :as opts}]
  (if (empty? bindings)
    (compile-el body opts)
    
    (let [[{:keys [bind value path]} & more-bindings] bindings

          compiled-value (compile-value value opts)

          dynamic-value? (empty? (:deps compiled-value))

          bind-syms (destructuring-bind-syms bind)
          
          compiled-body (compile-call {:call-type :let
                                       :bindings more-bindings
                                       :body body}

                                      (update-in opts
                                                 [(if (empty? (:deps compiled-value))
                                                    :local-syms
                                                    :dynamic-syms)]
                                                 (comp set #(concat % bind-syms))))
          
          let-sym (symbol path)

          deps (-> (set (:deps compiled-body))
                   (set/difference bind-syms)
                   (concat (:deps compiled-value))
                   set)]

      {:el `(~let-sym)
       :deps deps
       :declarations (concat (mapcat :declarations [compiled-value compiled-body])
                             [`(defn ~let-sym []
                                 (letfn [(bind-values-map# [value#]
                                           (let [~bind value#]
                                             ~(->> (for [bind-sym bind-syms]
                                                     `[(quote ~bind-sym) ~bind-sym])
                                                   (into {}))))

                                         (bind-updated-vars# [old-value# new-value#]
                                           (let [old-map# (bind-values-map# old-value#)
                                                 new-map# (bind-values-map# new-value#)]
                                             (set (filter #(not= (get old-map# %)
                                                                 (get new-map# %))
                                                          #{~@(for [bind-sym bind-syms]
                                                                `(quote ~bind-sym))}))))]
                                   
                                   (let [!placeholder-el# (atom nil)
                                         value# ~(:value compiled-value)
                                         body# ~(:el compiled-body)
                                         
                                         !last-value# (atom nil)]
                                     
                                     (reify fp/DynamicElement
                                       (~'should-update-el? [_# ~updated-vars]
                                         ~(u/deps->should-update deps updated-vars))

                                       (~'build-element [_# state#]
                                         (let [initial-value# (fp/current-value value# state#)
                                               initial-el# (fp/build-element body# (merge state# (bind-values-map# initial-value#)))]
                                           
                                           (reset! !last-value# initial-value#)
                                           (reset! !placeholder-el# initial-el#)
                                           initial-el#))

                                       (~'handle-update! [_# old-state# new-state# updated-vars#]
                                         (letfn [(update-body# [old-value# new-value# updated-vars#]
                                                   (when (fp/should-update-el? body# updated-vars#)
                                                     (fp/handle-update! body#
                                                                        (merge old-state# (bind-values-map# old-value#))
                                                                        (merge new-state# (bind-values-map# new-value#))
                                                                        updated-vars#)))]
                                           
                                           (if (fp/should-update-value? value# updated-vars#)
                                             (let [old-value# @!last-value#
                                                   new-value# (fp/current-value value# new-state#)]
                                               (if (not= old-value# new-value#)
                                                 (do
                                                   (reset! !last-value# new-value#)
                                                   (update-body# @!last-value# new-value# (set/union updated-vars# (bind-updated-vars# old-value# new-value#))))
                                                 
                                                 (update-body# @!last-value# @!last-value# updated-vars#)))

                                             (update-body# @!last-value# @!last-value# updated-vars#))))))))])})))

(let [dynamic-syms #{}
      local-syms #{}
      opts {}
      bindings [{:bind 'todo
                 :value (flow.parse/parse-form '(<<! !todos) {:elem? false :path "flow-test"})
                 :key-fn :todo-id
                 :path "1"}
                {:bind 'y
                 :value (flow.parse/parse-form (range 4) {:elem? false :path "flow-test"})
                 :path "2"}
                {:bind 'subtask
                 :value (flow.parse/parse-form 'todo {:elem? false :path "flow-test"})
                 :path "3"}]]
  (reduce (fn [{:keys [compiled-values dynamic-syms local-syms] :as acc} {:keys [bind value path]}]
            (let [{:keys [deps] :as compiled-value} (compile-value value (assoc opts
                                                                           :dynamic-syms dynamic-syms
                                                                           :local-syms local-syms))
                  destructured-binds (destructuring-bind-syms bind)]
                                              
              (-> acc
                  (update-in [(if (empty? deps) :local-syms :dynamic-syms)] set/union destructured-binds)
                  (update-in [:compiled-values] conj compiled-value))))
                                
          {:dynamic-syms dynamic-syms
           :local-syms local-syms
           :compiled-values []}
                
          bindings))

(defn parse-for-bindings [bindings {:keys [dynamic-syms local-syms] :as opts}]
  (reduce (fn [{:keys [compiled-values dynamic-syms local-syms] :as acc} {:keys [bind value path]}]
            (prn acc)
            (let [{:keys [deps] :as compiled-value} (compile-value value (assoc opts
                                                                           :dynamic-syms dynamic-syms
                                                                           :local-syms local-syms))
                  destructured-binds (destructuring-bind-syms bind)]
                                              
              (-> acc
                  (update-in [(if (empty? deps) :local-syms :dynamic-syms)] set/union destructured-binds)
                  (update-in [:compiled-values] conj (assoc compiled-value
                                                       :bind bind
                                                       :destructured-binds destructured-binds)))))
                                                                  
          {:dynamic-syms dynamic-syms
           :local-syms local-syms
           :compiled-values []}
                
          bindings))

(defn for-deps [compiled-values compiled-body]
  (reduce (fn [deps-acc {:keys [deps destructured-binds]}]
            (-> deps-acc
                (set/difference destructured-binds)
                (set/union deps)))
          (:deps compiled-body)
          (reverse compiled-values)))

(comment
  (compile-el (flow.parse/parse-form '(for [x (<<! !color)
                                            y (range 4)]
                                        [:div x y])
                                     {:elem? true
                                      :path "flow-test"})
              {:dynamic-syms #{}
               :local-syms #{}
               :state 'test-state
               :old-state 'test-old-state
               :new-state 'test-new-state}))

(defmethod compile-call :for [{:keys [bindings body path]} {:keys [dynamic-syms local-syms state updated-vars] :as opts}]
  (let [{:keys [compiled-values dynamic-syms local-syms]} (parse-for-bindings bindings opts)

        compiled-body (compile-el body (assoc opts
                                         :dynamic-syms dynamic-syms
                                         :local-syms local-syms))

        deps (for-deps compiled-values compiled-body)

        for-sym (symbol path)]

    {:el `(~for-sym)
     :deps deps
     :declarations (concat (mapcat :declarations (concat compiled-values [compiled-body]))
                           
                           [(let [values (map #(gensym (str "for-value-" %)) (range (count bindings)))
                                  initial-values (map #(gensym (str "for-initial-value-" %)) (range (count bindings)))]
                              `(defn ~for-sym []
                                 (letfn [#_(bind-values-map# [value#]
                                                             (let [~bind value#]
                                                               ~(->> (for [bind-sym bind-syms]
                                                                       `[(quote ~bind-sym) ~bind-sym])
                                                                     (into {}))))

                                         #_(bind-updated-vars# [old-value# new-value#]
                                                               (let [old-map# (bind-values-map# old-value#)
                                                                     new-map# (bind-values-map# new-value#)]
                                                                 (set (filter #(not= (get old-map# %)
                                                                                     (get new-map# %))
                                                                              #{~@(for [bind-sym bind-syms]
                                                                                    `(quote ~bind-sym))}))))]

                                   (let [!placeholder-el# (atom nil)
                                         ~@(mapcat (fn [value compiled-value]
                                                     `[(quote ~value) ~(:value compiled-value)])
                                                   values compiled-values)
                                         body# ~(:el compiled-body)]
                                      
                                     (reify fp/DynamicElement
                                       (~'should-update-el? [_# ~updated-vars]
                                         ~(u/deps->should-update deps updated-vars))

                                       #_(~'build-element [_# ~state]
                                           (let [~@(mapcat (fn [initial-value value]
                                                             `[(quote ~initial-value) (fp/current-value ~value ~state)]))
                                                 
                                                 initial-el# (fp/build-element body# (merge state# (bind-values-map# initial-value#)))]
                                            
                                             (reset! !last-value# initial-value#)
                                             (reset! !placeholder-el# initial-el#)
                                             initial-el#))

                                       #_(~'handle-update! [_# old-state# new-state# updated-vars#]
                                           (letfn [(update-body# [old-value# new-value# updated-vars#]
                                                     (when (fp/should-update-el? body# updated-vars#)
                                                       (fp/handle-update! body#
                                                                          (merge old-state# (bind-values-map# old-value#))
                                                                          (merge new-state# (bind-values-map# new-value#))
                                                                          updated-vars#)))]
                                            
                                             (if (fp/should-update-value? value# updated-vars#)
                                               (let [old-value# @!last-value#
                                                     new-value# (fp/current-value value# new-state#)]
                                                 (if (not= old-value# new-value#)
                                                   (do
                                                     (reset! !last-value# new-value#)
                                                     (update-body# @!last-value# new-value# (set/union updated-vars# (bind-updated-vars# old-value# new-value#))))
                                                  
                                                   (update-body# @!last-value# @!last-value# updated-vars#)))

                                               (update-body# @!last-value# @!last-value# updated-vars#)))))))))])})

  )

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
                                                (if primary
                                                  (do
                                                    
                                                    [:div
                                                     [:h1 {::f/style {:color primary}}]
                                                     #_(if (= primary "#000")
                                                         (do
                                                           [:p "is black"]))])))
                                             {:elem? true
                                              :path "flow-test"})
                      syms)
          #_(render-el syms)))

    ))

(defmethod compile-el :call [call opts]
  (compile-call call opts))
