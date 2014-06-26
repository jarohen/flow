(ns flow.el)

(defn parse-vector [[tagish possible-attrs & body]]
  (let [tagish (name tagish)
        attrs (when (or (map? possible-attrs)
                        (::attrs (meta possible-attrs)))
                possible-attrs)

        children (if attrs
                   body
                   (cons possible-attrs body))]

    {:tag (second (re-find #"^([^#.]+)" tagish))
     :id (second (re-find #"#([^.]+)" tagish))
     :classes (map second (re-seq #"\.([^.]+)" tagish))
     :attrs attrs
     :children children}))

(defn add-classes! [$elem-sym classes]
  (letfn [(add-class! [class]
            `(.. ~$elem-sym
                 ~'-classList
                 (~'add ~class)))]
    
    (for [class classes]
      (cond
       (or (list? class) (symbol? class)) (let [class-sym (gensym)]
                                            `(when-let [~class-sym ~class]
                                               ~(add-class! class-sym)))
       
       (or (string? class) (keyword? class)) (add-class! (name class))))))

(defn effect-style! [$elem-sym style]
  (letfn [(set-style! [k v]
            `(aset (.-style ~$elem-sym) ~(name k) ~v))]
    
    (for [[k v] style]
      (cond
       (or (list? v) (symbol? v)) (let [value-sym (gensym)]
                                    `(when-let [~value-sym ~v]
                                       ~(set-style! k value-sym)))
       
       (or (string? v) (keyword? v)) (set-style! k (name v))))))

(defn effect-attrs! [$elem-sym attrs]
  (cond
   (map? attrs) (for [[k v] attrs]
                  (case k
                    :style `(do ~@(effect-style! $elem-sym v))
                    :classes `(do ~@(add-classes! $elem-sym v))
                    `(.setAttribute ~$elem-sym ~(name k) v)))))

(effect-attrs! 'elem {:classes '["abc"
                                 (when (zero? (rand-int 3))
                                   "blah")]
                      :style '{:float (when (zero? (rand-int 3))
                                        "right")}})

(declare ->node)

(defn render-elem [{:keys [tag id classes attrs children]}]
  (let [$elem-sym (gensym)]
    `(let [~$elem-sym (js/document.createElement ~tag)]
       ~@(when id
           [`(set! (.-id ~$elem-sym) ~id)])
       
       ~@(for [$child children]
           `(.appendChild ~$elem-sym ~(->node $child)))

       ~@(add-classes! $elem-sym classes)

       ~@(effect-attrs! $elem-sym attrs)
       
       ~$elem-sym)))


#_(->node '[:div {:style {:color "#549"}}
            [:h1 "Hello world!"]
            [:p.copy
             "If I can get this working tonight, "
             [:strong "I'll be a happy man :)"]]])

(defn ->node [elem]
  (cond
   (vector? elem) (render-elem (parse-vector elem))
   (string? elem) `(js/document.createTextNode ~elem)
   (list? elem) `(flow.el/runtime-el ~elem)))

#+clj
(defmacro el [elem]
  (->node elem))

#+cljs
(defn runtime-el [elem]
  (println "got" (pr-str elem)))

