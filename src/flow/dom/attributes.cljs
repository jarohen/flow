(ns flow.dom.attributes)

(defn set-id! [$el id]
  (set! (.-id $el) id))

(defn set-style! [$el attr value]
  (.setProperty (.-style $el)
                (name attr)
                (cond-> value
                  (keyword? value) name)))

(defn set-attr! [$el attr value]
  (case attr
    :value (set! (.-value $el) value)
    :checked (if (boolean value)
               (set! (.-checked $el) true)
               (set! (.-checked $el) nil))
    
    (if-not (nil? value)
      (.setAttribute $el (name attr) value)
      (.removeAttribute $el (name attr) value))))

(defn set-classes! [$el new-classes]
  (set! (.-className $el) (s/join " " new-classes)))
