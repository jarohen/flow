(ns flow.dom
  (:require [clojure.set :as set]
            [clojure.string :as s]))

(def !debug (atom false))

;; pinched from Dommy
;; https://github.com/Prismatic/dommy/blob/5d75be9d24b0016f419bb1e23fcbf700421be6c7/src/dommy/template.cljs#L6-L7
(def svg-ns "http://www.w3.org/2000/svg")
(def svg-tag? #{"svg" "g" "rect" "circle" "clipPath" "path" "line" "polygon" "polyline" "text" "textPath"})

(defn new-element [tag]
  (if (svg-tag? tag)
    (js/document.createElementNS svg-ns tag)
    (js/document.createElement tag)))

(defn remove! [$el]
  (when $el
    (when-let [$parent (.-parentNode $el)]
      (.removeChild $parent $el))))

(defn append-child! [$parent child-or-children]
  (if (coll? child-or-children)
    (doseq [$child child-or-children]
      (.appendChild $parent $child))
    
    (.appendChild $parent child-or-children)))

(defn insert-child-before! [$parent $el $before-sibling]
  (if $before-sibling
    (.insertBefore $parent $el $before-sibling)
    (append-child! $parent $el)))

(defn set-classes! [$el new-classes]
  (set! (.-className $el) (s/join " " new-classes)))

(defn set-style! [$el k v]
  (when @!debug
    (js/console.log "setting style on" $el ":" (pr-str k) "=" (pr-str v)))
  
  (.setProperty (.-style $el)
                (name k)
                (cond-> v
                  (keyword? v) name)))

(defn set-attr! [$el k v]
  (when @!debug
    (js/console.log "setting attr on" $el ":" (pr-str k) "=" (pr-str v)))

  (if (= k :value)
    (set! (.-value $el) v)
    
    (if-not (nil? v)
      (.setAttribute $el (name k) v)
      (.removeAttribute $el (name k) v))))

(defn add-listener! [$el event listener]
  (when @!debug
    (js/console.log "adding" (pr-str event) "listener on" $el))
  
  (.addEventListener $el (name event) listener))

(defn value [$el]
  (case (.-type $el)
    "checkbox" (.-checked $el)
    (.-value $el)))

(defn bind-value! [!atom]
  (fn [e]
    (reset! !atom (value (.-target e)))))

(let [$null-elem (doto (js/document.createElement "span")
                   (set-style! :display :none)
                   (set-attr! :data-flow-placeholder true))]
  (defn null-elem []
    (.cloneNode $null-elem)))

(defn ->node [$el-or-val]
  (cond
   (or (nil? $el-or-val)
       (and (seq? $el-or-val)
            (empty? $el-or-val)))
   (null-elem)
            
   (seq? $el-or-val) (map ->node $el-or-val)
   
   (.-nodeType $el-or-val) $el-or-val
   
   :else (js/document.createTextNode (if (string? $el-or-val)
                                       $el-or-val
                                       (pr-str $el-or-val)))))


