(ns flow.compile.node
  (:require [flow.compile :refer [compile-el]]
            [flow.compile.update :refer [on-update-form]]))

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
  (when (seq classes)
    (let [compiled-classes (map #(compile-el % opts) classes)
          deps (set (mapcat :deps compiled-classes))]
      {:deps deps

       :el-init [`(fd/add-classes! ~elem-sym (set [~@(->> compiled-classes
                                                          (remove (comp not-empty :deps))
                                                          (map :as-value))]))]
       
       :on-update (when (not-empty deps)
                    [`(fd/update-classes! ~elem-sym
                                          (let [~state-sym ~old-state-sym]
                                            (set [~@(map :as-value compiled-classes)]))

                                          (let [~state-sym ~new-state-sym]
                                            (disj (set [~@(map :as-value compiled-classes)]) nil)))])})))

(defn compile-listener [elem-sym {:keys [event listener]} opts]
  {:el-init [`(fd/add-listener! ~elem-sym ~event ~(:as-value (compile-el listener opts)))]})

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

(defmethod compile-el :node [{:keys [tag id style classes attrs children listeners]} {:keys [updated-var-sym] :as opts}]
  (let [elem-sym (gensym "elem")
        compiled-attrs (map #(compile-attr elem-sym % opts) attrs)
        compiled-styles (map #(compile-style elem-sym % opts) style)
        compiled-classes (compile-classes elem-sym classes opts)
        compiled-children (map #(compile-child elem-sym % opts) children)
        compiled-listeners (map #(compile-listener elem-sym % opts) listeners)]
    
    {:el-bindings `[[~elem-sym (js/document.createElement ~tag)]
                    ~@(mapcat :el-bindings compiled-children)]

     :el-return elem-sym
     
     :el-init `[~@(when id
                    [`(set! (.-id ~elem-sym) ~id)])
               
                ~@(mapcat :el-init compiled-attrs)
                ~@(mapcat :el-init compiled-styles)
                ~@(mapcat :el-init compiled-listeners)
                ~@(:el-init compiled-classes)
                ~@(mapcat :el-init compiled-children)]     
     
     :deps (set (concat (mapcat :deps compiled-attrs)
                        (mapcat :deps compiled-styles)
                        (:deps compiled-classes)
                        (mapcat :deps compiled-children)))

     :on-update (concat (map (on-update-form opts) compiled-attrs)
                        (map (on-update-form opts) compiled-styles)
                        [(on-update-form compiled-classes opts)]
                        (map (on-update-form opts) compiled-children))}))

