(ns flow.parse)

(alias 'f (doto 'flow.core create-ns))


(declare parse-form)

(defn parse-map-vals [m opts]
  (->> (for [[k v] m]
         [k (parse-form v opts)])
       (into {})))

(defn parse-node [[tagish possible-attrs & body] opts]
  (let [tagish (name tagish)
        attrs (when (map? possible-attrs)
                possible-attrs)

        children (if attrs
                   body
                   (cons possible-attrs body))

        tag (second (re-find #"^([^#.]+)" tagish))]

    {:type :node

     :tag tag
     
     :id (second (re-find #"#([^.]+)" tagish))
     
     :classes (concat (for [class-name (map second (re-seq #"\.([^.]+)" tagish))]
                        (parse-form class-name {:elem? false}))
                      
                      (for [class (::f/classes attrs)]
                        (parse-form class {:elem? false})))

     :style (parse-map-vals (::f/style attrs) {:elem? false})

     :listeners (for [[event listener] (::f/on attrs)]
                  {:event event
                   :listener (parse-form listener {:elem? false})})
     
     :attrs (parse-map-vals (dissoc attrs ::f/classes ::f/style ::f/on)
                            {:elem? false})
     
     :children (map #(parse-form % {:elem? true}) children)}))

(defmulti parse-call
  (fn [call elem?]
    (first call)))

(defmethod parse-call 'let [[_ bindings & body] {:keys [elem?]}]
  {:call-type :let
   :bindings (for [[bind value] (partition 2 bindings)]
               {:bind bind
                :value (parse-form value {:elem? false})})
   
   :body (parse-form `(do ~@body) {:elem? elem?})})

(defmethod parse-call 'for [[_ bindings body] {:keys [elem?] :as opts}]
  {:call-type :for
   :bindings (for [[bind value] (partition 2 bindings)]
               {:bind bind
                :value (parse-form value opts)
                :key-fn (::f/key-fn (meta value))})
   :body (parse-form body opts)})

(defmethod parse-call 'fn* [[_ & decl] opts]
  (let [[possible-name & more-decl] decl
        [fn-name decl] (if (symbol? possible-name)
                         [possible-name more-decl]
                         [nil decl])]
    
    
    {:call-type :fn-decl
     :fn-name fn-name
     :arities (if (every? seq? decl)
                (for [[args & body] decl]
                  {:args args
                   :body (parse-form `(do ~@body) opts)})
                
                (let [[args & body] decl]
                  [{:args args
                    :body (parse-form `(do ~@body) opts)}]))}))

(defmethod parse-call 'if [[_ test then else] {:keys [elem?]}]
  {:call-type :if
   :test (parse-form test {:elem? false})
   :then (parse-form then {:elem? elem?})
   :else (parse-form else {:elem? elem?})})

(defmethod parse-call 'do [[_ & body] opts]
  {:call-type :do
   :side-effects (butlast body)
   :return (parse-form (last body) opts)})

(defmethod parse-call 'case [[_ case-expr & clauses] opts]
  {:call-type :case
   :case-expr (parse-form case-expr (assoc opts :elem? false))
   :clauses (for [[[test expr] idx] (map vector (partition 2 clauses) (range))]
              {:test test
               :expr (parse-form expr opts)
               :idx idx})
   :default (when (odd? (count clauses))
              (parse-form (last clauses) opts))})

(defmethod parse-call '<< [[_ cursor] _]
  {:call-type :unwrap-cursor
   :cursor cursor})

(defmethod parse-call '!<< [[_ cursor] _]
  {:call-type :wrap-cursor
   :cursor cursor})

(defmethod parse-call :default [[& args] opts]
  {:call-type :fn-call
   :args (map #(parse-form % {:elem? false}) args)})

(defn parse-form [form & [{:keys [elem?]
                           :or {elem? false}
                           :as opts}]]
  (-> (cond
       (and elem? (vector? form)) (parse-node form opts)

       (seq? form) (assoc (parse-call form opts)
                     :type :call)

       (symbol? form) {:type :symbol
                       :sym form}

       (map? form) {:type :map
                    :map (->> (for [[k v] form]
                                [(parse-form k {:elem? false})
                                 (parse-form v {:elem? false})])
                              (into {}))}

       ;; handles set and vec
       (coll? form) {:type :coll
                     :coll (->> form
                                (map parse-form)
                                (into (empty form)))}
   
       :else {:type :primitive
              :primitive form})
      
      (assoc :elem? elem?)))
