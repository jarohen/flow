(ns flow.dom
  (:require [clojure.set :as set]))

(def !debug (atom false))

;; pinched from Dommy
;; https://github.com/Prismatic/dommy/blob/5d75be9d24b0016f419bb1e23fcbf700421be6c7/src/dommy/template.cljs#L6-L7
(def svg-ns "http://www.w3.org/2000/svg")
(def svg-tag? #{"svg" "g" "rect" "circle" "clipPath" "path" "line" "polygon" "polyline" "text" "textPath"})

(defn new-element [tag]
  (if (svg-tag? tag)
    (js/document.createElementNS svg-ns tag)
    (js/document.createElement tag)))

(defn remove-child! [$parent $el]
  (when $el
    (.removeChild $parent $el)))

(defn append-child! [$parent child-or-children]
  (if (coll? child-or-children)
    (doseq [$child child-or-children]
      (.appendChild $parent $child))
    
    (.appendChild $parent child-or-children)))

(defn insert-child-before! [$parent $el $before-sibling]
  (if $before-sibling
    (.insertBefore $parent $el $before-sibling)
    (append-child! $parent $el)))

(defn add-class! [$el class-name]
  (when @!debug
    (js/console.log "adding class" (pr-str class-name) "to" $el))
  
  (.. $el
      -classList
      (add class-name)))

(defn remove-class! [$el class-name]
  (when @!debug
    (js/console.log "removing class" (pr-str class-name) "from" $el))
  
  (.. $el
      -classList
      (remove class-name)))

(defn add-classes! [$el classes]
  (doseq [class-name classes]
    (add-class! $el class-name)))

(defn update-classes! [$el old-classes new-classes]
  (doseq [class-name (set/difference new-classes old-classes)]
    (add-class! $el class-name))
  
  (doseq [class-name (set/difference old-classes new-classes)]
    (remove-class! $el class-name)))

(defn set-style! [$el k v]
  (when @!debug
    (js/console.log "setting style on" $el ":" (pr-str k) "=" (pr-str v)))
  
  (aset (.-style $el) (name k) (cond-> v
                                 (keyword? v) name)))

(defn set-attr! [$el k v]
  (when @!debug
    (js/console.log "setting attr on" $el ":" (pr-str k) "=" (pr-str v)))

  (if (= k :value)
    (set! (.-value $el) v)
    
    (if-not (nil? v)
      (.setAttribute $el (name k) v)
      (.removeAttribute $el (name k) v))))

(let [$null-elem (doto (js/document.createElement "span")
                   (set-style! :display :none))]
  (defn null-elem [& [id]]
    (let [$elem (.cloneNode $null-elem)]
      (when id
        (set! (.-id $elem) id))
      
      $elem)))

(defn swap-elem! [$old $new]
  (when @!debug
    (js/console.log "swapping" $old "for" $new))
  
  (when-let [$parent (.-parentNode $old)]
    (.replaceChild $parent $new $old)))

(defn add-listener! [$el event listener]
  (when @!debug
    (js/console.log "adding" (pr-str event) "listener on" $el))
  
  (.addEventListener $el (name event) listener))

(defn ->node [$el-or-val]
  (cond
   (nil? $el-or-val) (null-elem)
   
   (.-nodeType $el-or-val) $el-or-val
   
   :else (js/document.createTextNode (if (string? $el-or-val)
                                       $el-or-val
                                       (pr-str $el-or-val)))))

(defn value [$el]
  (case (.-type $el)
    "checkbox" (.-checked $el)
    (.-value $el)))

(defn bind-value! [!atom]
  (fn [e]
    (reset! !atom (value (.-target e)))))
