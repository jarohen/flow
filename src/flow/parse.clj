(ns flow.parse)

(alias 'f (doto 'flow.core create-ns))


(declare parse-form)

(defn parse-map-vals [m]
  (->> (for [[k v] m]
         [k (parse-form v)])
       (into {})))

(defn parse-node [[tagish possible-attrs & body]]
  (let [tagish (name tagish)
        attrs (when (map? possible-attrs)
                possible-attrs)

        children (if attrs
                   body
                   (cons possible-attrs body))]

    {:type :node

     :tag (second (re-find #"^([^#.]+)" tagish))
     
     :id (second (re-find #"#([^.]+)" tagish))
     
     :classes (concat (for [class-name (map second (re-seq #"\.([^.]+)" tagish))]
                        {:class (parse-form class-name)})
                      (for [class (::f/classes attrs)]
                        {:class (parse-form class)}))

     :style (parse-map-vals (::f/style attrs))

     :listeners (parse-map-vals (::f/on attrs))
     
     :attrs (parse-map-vals (dissoc attrs ::f/classes ::f/style ::f/on))
     
     :children (map #(parse-form % {:elem? true}) children)}))

(defmulti parse-call
  (fn [call elem?]
    (first call)))

(defmethod parse-call 'clojure.core/let [[_ bindings & body] elem?]
  {:call-type :let
   :bindings (for [[bind value] (partition 2 bindings)]
               {:bind bind
                :value (parse-form value)})
   :side-effects (butlast body)
   :body (parse-form (last body) {:elem? elem?})})

(defmethod parse-call 'clojure.core/for [[_ bindings body] elem?]
  {:call-type :for
   :bindings (for [[bind value] (partition 2 bindings)]
               {:bind bind
                :value (parse-form value)})
   :body (parse-form body {:elem? elem?})})

(defmethod parse-call 'if [[_ test then else] elem?]
  {:call-type :if
   :test (parse-form test)
   :then (parse-form then {:elem? elem?})
   :else (parse-form else {:elem? elem?})})

(defmethod parse-call 'do [[_ & body] elem?]
  {:call-type :do
   :side-effects (butlast body)
   :return (parse-form (last body) {:elem? elem?})})

(defmethod parse-call '<<! [[_ cursor] _]
  {:call-type :unwrap-cursor
   :cursor cursor})

(defmethod parse-call '!>> [[_ cursor] _]
  {:call-type :wrap-cursor
   :cursor cursor})

(defmethod parse-call :default [[& args] _]
  {:call-type :fn-call
   :args (map parse-form args)})

(defn parse-form [form & [{:keys [elem?]
                           :or {elem? false}}]]
  (-> (cond
       (and elem? (vector? form)) (parse-node form)

       (seq? form) (assoc (parse-call form elem?)
                     :type :call)

       (symbol? form) {:type :symbol
                       :symbol form}

       (map? form) {:type :map
                    :map (->> form
                              (map (partial map parse-form))
                              (into {}))}
   
       (coll? form) {:type :coll
                     :coll (->> form
                                (map parse-form)
                                (into (empty form)))}
   
       :else {:type :primitive
              :primitive form})
      
      (assoc :elem? elem?)))

