(ns flow.dom.elements
  (:require [flow.dom.attributes :as fda]))

(defn text-el [s]
  (js/document.createTextNode s))

;; pinched from Dommy
;; https://github.com/Prismatic/dommy/blob/5d75be9d24b0016f419bb1e23fcbf700421be6c7/src/dommy/template.cljs#L6-L7
(def svg-ns "http://www.w3.org/2000/svg")
(def svg-tag? #{"svg" "g" "rect" "circle" "clipPath" "path" "line" "polygon" "polyline" "text" "textPath"})

(defn new-element [tag]
  (if (svg-tag? tag)
    (js/document.createElementNS svg-ns tag)
    (js/document.createElement tag)))

(defn append-child! [$parent $child]
  (.appendChild $parent $child))

(defn insert-before! [$parent $sibling $child]
  (.insertBefore $parent $child $sibling))

(defn remove-child! [$parent $child]
  (when (and $parent $child)
    (.removeChild $parent $child)))

(defn clear! [$el]
  (loop []
    (when-let [$child (.-firstChild $el)]
      (remove-child! $el $child)
      (recur))))

(defn replace-child! [$parent $old-child $new-child]
  (if $old-child
    (.replaceChild $parent $new-child $old-child)
    (append-child! $parent $new-child)))

(defn next-sibling [$parent $child]
  (.-nextSibling $child))

(defn add-event-listener! [$el event listener]
  (if (exists? js/window.jQuery)
    (.. (js/$ $el) (on (name event) listener))
    (.addEventListener $el (name event) listener)))

(defn value [$el]
  (case (.-type $el)
    "checkbox" (.-checked $el)
    (.-value $el)))

(defn bind-value! [!atom]
  (fn [e]
    (reset! !atom (value (.-target e)))))

(defn null-elem []
  (doto (js/document.createElement "span")
    (fda/set-style! :display :none)
    (fda/set-attr! :data-flow-placeholder true)))

(defn ->el [el-ish]
  (cond
    (map? el-ish) (->el (pr-str el-ish))
    
    (coll? el-ish) (map ->el el-ish)

    (or (nil? el-ish)
        (and (seq? el-ish)
             (empty? el-ish)))
    (null-elem)
    
    (.-nodeType el-ish) el-ish

    :else (text-el (if (string? el-ish)
                     el-ish
                     (pr-str el-ish)))))
