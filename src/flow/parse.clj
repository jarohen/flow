(ns flow.parse)

(alias 'f (doto 'flow.core create-ns))


(declare parse-form)

(defn parse-map-vals [m {:keys [path] :as opts}]
  (->> (for [[k v] m]
         [k (parse-form v (assoc opts :path (str path "-" (name k))))])
       (into {})))

(defn parse-node [[tagish possible-attrs & body] {:keys [path] :as opts}]
  (let [tagish (name tagish)
        attrs (when (map? possible-attrs)
                possible-attrs)

        children (if attrs
                   body
                   (cons possible-attrs body))

        tag (second (re-find #"^([^#.]+)" tagish))

        path (str path "-" tag)]

    {:type :node

     :path path

     :tag tag
     
     :id (second (re-find #"#([^.]+)" tagish))
     
     :classes (concat (for [class-name (map second (re-seq #"\.([^.]+)" tagish))]
                        (parse-form class-name {:elem? false
                                                :path (str path "-classes-" class-name)}))
                      
                      (for [[class idx] (map vector (::f/classes attrs) (range))]
                        (parse-form class {:path (str path "-classes-" idx)
                                           :elem? false})))

     :style (parse-map-vals (::f/style attrs) {:path (str path "-style")
                                               :elem? false})

     :listeners (for [[event listener] (::f/on attrs)]
                  {:event event
                   :listener (parse-form listener {:path (str path "-on" (name event))
                                                   :elem? false})})
     
     :attrs (parse-map-vals (dissoc attrs ::f/classes ::f/style ::f/on) {:path (str path "-attrs")
                                                                         :elem? false})
     
     :children (map #(parse-form %1 {:elem? true, :path (str path "-" %2)}) children (range))}))

(defmulti parse-call
  (fn [call elem?]
    (first call)))

(defmethod parse-call 'let [[_ bindings & body] {:keys [elem? path]}]
  (let [path (str path "-let")]
    {:call-type :let
     :path path
     :bindings (for [[[bind value] idx] (map vector (partition 2 bindings) (range))]
                 {:bind bind
                  :value (parse-form value {:elem? false
                                            :path (str path "-bind-" idx)})
                  :path (str path "-" idx)})
     
     :body (parse-form `(do ~@body) {:elem? elem?
                                     :path (str path "-body")})}))

(defmethod parse-call 'for [[_ bindings body] {:keys [elem? path] :as opts}]
  (let [path (str path "-for")]
    {:call-type :for
     :path path
     :bindings (for [[[bind value] idx] (map vector (partition 2 bindings) (range))]
                 (let [path (str path "-" idx)]
                   {:bind bind
                    :value (parse-form value (assoc opts :path path))
                    :key-fn (::f/key-fn (meta value))
                    :path path}))
     :body (parse-form body (assoc opts :path (str path "-body")))}))

(defmethod parse-call 'fn* [decl {:keys [path]}]
  {:call-type :fn-decl
   :path path
   :fn-decl decl})

(defmethod parse-call 'if [[_ test then else] {:keys [elem? path]}]
  (let [path (str path "-if")]
    {:call-type :if
     :path path
     :test (parse-form test {:elem? false, :path (str path "-test")})
     :then (parse-form then {:elem? elem?, :path (str path "-then")})
     :else (parse-form else {:elem? elem?, :path (str path "-else")})}))

(defmethod parse-call 'do [[_ & body] {:keys [path] :as opts}]
  {:call-type :do
   :path path
   :side-effects (butlast body)
   :return (parse-form (last body) opts)})

(defmethod parse-call '<<! [[_ cursor] {:keys [path]}]
  {:call-type :unwrap-cursor
   :path (str path "-unwrap-cursor-" cursor)
   :cursor cursor})

(defmethod parse-call '!>> [[_ cursor] _]
  {:call-type :wrap-cursor
   :cursor cursor})

(defmethod parse-call :default [[& args] {:keys [path] :as opts}]
  {:call-type :fn-call
   :path path
   :args (map #(parse-form % (update-in opts [:path] str "-call")) args)})

(defn parse-form [form & [{:keys [elem? path]
                           :or {elem? false}
                           :as opts}]]
  (-> (cond
       (and elem? (vector? form)) (parse-node form opts)

       (seq? form) (assoc (parse-call form opts)
                     :type :call)

       (symbol? form) {:type :symbol
                       :sym form
                       :path (str (gensym (str path "-" form)))}

       (map? form) {:type :map
                    :map (->> (for [[[k v] idx] (map vector form (range))]
                                [(parse-form k (update-in opts [:path] str "-" idx "-k"))
                                 (parse-form v (update-in opts [:path] str "-" idx "-v"))])
                              (into {}))
                    :path path}
   
       (coll? form) {:type :coll
                     :coll (->> form
                                (map parse-form)
                                (into (empty form)))
                     :path path}
   
       :else {:type :primitive
              :primitive form
              :path path})
      
      (assoc :elem? elem?)))

