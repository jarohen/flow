(ns flow.compile.node
  (:require [flow.compile :refer [compile-el]]))

(alias 'fd (doto 'flow.dom create-ns))


(defn compile-attr [elem-sym [k v] {:keys [state-sym new-state-sym] :as opts}]
  (let [{:keys [deps as-value]} (compile-el v opts)
        set-attr-form [`(let [~state-sym ~new-state-sym
                              v# ~as-value]
                          (fd/set-attr! ~elem-sym ~k v#))]]
    {:deps deps

     :el-init (when (empty? deps)
                [set-attr-form])
     
     :on-update (when (not-empty deps)
                  [set-attr-form])}))

(defn compile-style [elem-sym [k v] {:keys [state-sym new-state-sym] :as opts}]
  (let [{:keys [deps as-value]} (compile-el v opts)
        set-style-form `(let [~state-sym ~new-state-sym
                              v# ~as-value]
                          (fd/set-style! ~elem-sym ~k v#))]
    {:deps deps

     :el-init (when (empty? deps)
                [set-style-form])

     :on-update (when (not-empty deps)
                  [set-style-form])}))

(defn compile-classes [elem-sym classes {:keys [state-sym old-state-sym new-state-sym] :as opts}]
  (let [compiled-classes (map #(compile-el % opts) classes)
        deps (set (mapcat :deps compiled-classes))]
    {:deps deps

     :el-init (when (empty? deps)
                [`(doseq [class# (set [~@(map :as-value compiled-classes)])]
                    (fd/add-class! ~elem-sym class#))])
     
     :on-update (when (not-empty deps)
                  [`(let [old-classes# (let [~state-sym ~old-state-sym]
                                         (set [~@(map :as-value compiled-classes)]))
                          
                          new-classes# (let [~state-sym ~new-state-sym]
                                         (set [~@(map :as-value compiled-classes)]))]
                      
                      (doseq [class# (clojure.set/difference new-classes# old-classes#)]
                        (fd/add-class! ~elem-sym class#))
                      
                      (doseq [class# (clojure.set/difference old-classes# new-classes#)]
                        (fd/remove-class! ~elem-sym class#)))])}))

(defn compile-child [elem-sym child opts]
  (let [{:keys [init]} (compile-el child opts)]
    {:init `(when-let [child# ~init]
              (.appendChild ~elem-sym child#))}))

(comment
  (require 'flow.parse)

  (compile-el (flow.parse/parse-form '[:div {:flow.core/style {:color (<<! !color)}}] {:elem? true}) {:state-sym (gensym "state")}))

(defmethod compile-el :node [{:keys [tag id style classes attrs children]} opts]
  (let [elem-sym (gensym "elem")
        compiled-attrs (map #(compile-attr elem-sym % opts) attrs)
        compiled-styles (map #(compile-style elem-sym % opts) style)
        compiled-classes (compile-classes elem-sym classes opts)
        compiled-children (map #(compile-child elem-sym % opts) children)]
    
    {:el-init [`(let [~elem-sym (js/document.createElement ~tag)]
                  ~@(when id
                      [`(set! (.-id ~elem-sym) ~id)])

                  ~@(mapcat :el-init compiled-attrs)

                  ~@(mapcat :el-init compiled-styles)
              
                  ~@(mapcat :el-init compiled-classes)
              
                  ~@(mapcat :el-init compiled-children)
              
                  ~elem-sym)]}))

