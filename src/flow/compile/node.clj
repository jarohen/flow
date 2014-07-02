(ns flow.compile.node
  (:require [flow.compile :refer [compile-el]]))

(alias 'fd (doto 'flow.dom create-ns))


(defn compile-attr [elem-sym [k v] {:keys [state-sym new-state-sym] :as opts}]
  (let [{:keys [deps as-value]} (compile-el v opts)
        set-attr-form `(let [v# ~as-value]
                         (fd/set-attr! ~elem-sym ~k v#))]
    {:deps deps

     :el-init (when (empty? deps)
                [set-attr-form])
     
     :on-update (when (not-empty deps)
                  [`(let [~state-sym ~new-state-sym]
                      ~set-attr-form)])}))

(defn compile-style [elem-sym [k v] {:keys [state-sym new-state-sym] :as opts}]
  (let [{:keys [deps as-value]} (compile-el v opts)
        set-style-form `(let [v# ~as-value]
                          (fd/set-style! ~elem-sym ~k v#))]
    {:deps deps

     :el-init (when (empty? deps)
                [set-style-form])

     :on-update (when (not-empty deps)
                  [`(let [~state-sym ~new-state-sym]
                      ~set-style-form)])}))

(defn compile-classes [elem-sym classes {:keys [state-sym old-state-sym new-state-sym] :as opts}]
  (let [compiled-classes (map #(compile-el % opts) classes)
        deps (set (mapcat :deps compiled-classes))]
    {:deps deps

     :el-init [`(doseq [class# (set [~@(->> compiled-classes
                                            (remove (comp not-empty :deps))
                                            (map :as-value))])]
                  (fd/add-class! ~elem-sym class#))]
     
     :on-update (when (not-empty deps)
                  [`(let [old-classes# (let [~state-sym ~old-state-sym]
                                         (set [~@(map :as-value compiled-classes)]))
                          
                          new-classes# (let [~state-sym ~new-state-sym]
                                         (disj (set [~@(map :as-value compiled-classes)]) nil))]
                      
                      (doseq [class# (clojure.set/difference new-classes# old-classes#)]
                        (fd/add-class! ~elem-sym class#))
                      
                      (doseq [class# (clojure.set/difference old-classes# new-classes#)]
                        (fd/remove-class! ~elem-sym class#)))])}))

(defn compile-child [elem-sym child opts]
  (let [{:keys [el-init el-bindings el-return deps on-update]} (compile-el child opts)]
    {:el-init `[~@el-init
                (.appendChild ~elem-sym ~el-return)]
     
     :el-bindings el-bindings
     
     :on-update on-update
     
     :deps deps}))

(comment
  (require 'flow.parse)

  (compile-el (flow.parse/parse-form '[:div {:flow.core/style {:color (<<! !color)}}] {:elem? true}) {:state-sym (gensym "state")}))

(defmethod compile-el :node [{:keys [tag id style classes attrs children]} {:keys [updated-var-sym] :as opts}]
  (let [elem-sym (gensym "elem")
        compiled-attrs (map #(compile-attr elem-sym % opts) attrs)
        compiled-styles (map #(compile-style elem-sym % opts) style)
        compiled-classes (compile-classes elem-sym classes opts)
        compiled-children (map #(compile-child elem-sym % opts) children)]
    
    {:el-bindings `[[~elem-sym (js/document.createElement ~tag)]
                    ~@(mapcat :el-bindings compiled-children)]

     :el-return elem-sym
     
     :el-init `[~@(when id
                    [`(set! (.-id ~elem-sym) ~id)])
               
                ~@(mapcat :el-init compiled-attrs)
                ~@(mapcat :el-init compiled-styles)
                ~@(:el-init compiled-classes)
                ~@(mapcat :el-init compiled-children)]     
     
     :deps (set (concat (mapcat :deps compiled-attrs)
                        (mapcat :deps compiled-styles)
                        (:deps compiled-classes)
                        (mapcat :deps compiled-children)))

     :on-update (letfn [(update! [{:keys [deps on-update]}]
                          (when (seq deps)
                            `(when (contains? #{~@(for [dep deps]
                                                    `(quote ~dep))
                                                :all}
                                              ~updated-var-sym)
                               ~@on-update)))]
                  
                  (concat (map update! compiled-attrs)
                          (map update! compiled-styles)
                          [(update! compiled-classes)]
                          (map update! compiled-children)))}))

