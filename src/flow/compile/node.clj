(ns flow.compile.node
  (:require [flow.compile :refer [compile-el]]))

(alias 'fd (doto 'flow.dom create-ns))


(defn compile-attr [elem-sym [k v] opts]
  (let [{:keys [init]} (compile-el v opts)]
    {:init `(let [v# ~init]
              (if (nil? v#)
                (.removeAttribute ~elem-sym ~(name k))
                (.setAttribute ~elem-sym ~(name k) v#)))}))

(defn compile-style [elem-sym [k v] opts]
  (let [{:keys [init]} (compile-el v opts)]
    {:init `(when-let [v# ~init]
              (aset (.-style ~elem-sym) ~(name k) (cond-> v#
                                                    (keyword? v#) name)))}))

(defn compile-class [elem-sym {:keys [type class-name class]} opts]
  (case type
    :static {:init `(fd/add-class! ~elem-sym ~class-name)}
    :dynamic (let [{:keys [init]} (compile-el class opts)]
               {:init `(when-let [class# ~init]
                         (fd/add-class! ~elem-sym class#))})))

(defn compile-child [elem-sym child opts]
  (let [{:keys [init]} (compile-el child opts)]
    {:init `(when-let [child# ~init]
              (.appendChild ~elem-sym child#))}))

(defmethod compile-el :node [{:keys [tag id style classes attrs children]} opts]
  (let [elem-sym (gensym "elem")
        compiled-attrs (map #(compile-attr elem-sym % opts) attrs)
        compiled-styles (map #(compile-style elem-sym % opts) style)
        compiled-classes (map #(compile-class elem-sym % opts) classes)
        compiled-children (map #(compile-child elem-sym % opts) children)]
    
    {:init `(let [~elem-sym (js/document.createElement ~tag)]
              ~@(when id
                  [`(set! (.-id ~elem-sym) ~id)])

              ~@(map :init compiled-attrs)

              ~@(map :init compiled-styles)
              
              ~@(map :init compiled-classes)
              
              ~@(map :init compiled-children)
              
              ~elem-sym)}))

