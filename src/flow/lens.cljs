(ns flow.lens)

;; Most of this adapted from either from CLJS core or Om, thanks David
;; Nolan & contributors for the design ideas, inspiration and code :)

(defprotocol Lens
  (-value [_])
  (->atom [_ extra-path]))

(defn lens? [v]
  (satisfies? Lens v))

(declare ->lens)

(defn lens->atom [!state path]
  ;; This part adapted from CLJS core.
  (let [!watches (atom {})]
    (reify
      IEquiv
      (-equiv [this other]
        (identical? this other))

      IMeta
      (-meta [_]
        {:!state !state
         :path path})
      
      IDeref
      (-deref [_]
        (get-in @!state path))

      IReset
      (-reset! [this new-value]
        (let [old-value (-deref this)]
          (swap! !state assoc-in path new-value)
          (-notify-watches this old-value new-value)
          new-value))

      ISwap
      (-swap! [this f]
        (reset! this (f (-deref this))))
      (-swap! [this f a1]
        (reset! this (f (-deref this) a1)))
      (-swap! [this f a1 a2]
        (reset! this (f (-deref this) a1 a2)))
      (-swap! [this f a1 a2 as]
        (reset! this (apply f (-deref this) a1 a2 as)))

      IPrintWithWriter
      (-pr-writer [this writer opts]
        (-write writer "#<Atom: ")
        (-write writer (pr-str @this))
        (-write writer ">"))

      IWatchable
      (-notify-watches [this oldval newval]
        (doseq [[key f] @!watches]
          (f key this oldval newval)))
      (-add-watch [this key f]
        (swap! !watches assoc key f))
      (-remove-watch [this key]
        (swap! !watches dissoc key))

      IHash
      (-hash [this]
        (goog/getUid this)))))

(defn map-lens [value !state path]
  (reify
    Lens
    (-value [_] value)
    (->atom [_ extra-path]
      (lens->atom !state (vec (concat path extra-path))))
    
    IWithMeta
    (-with-meta [_ new-meta]
      (map-lens (with-meta value new-meta) !state path))
    IMeta
    (-meta [_]
      (meta value))

    ICloneable
    (-clone [_]
      (map-lens value !state path))
    
    ICounted
    (-count [_]
      (-count value))

    ICollection
    (-conj [this o]
      (->lens (-conj value o) !state path))

    ILookup
    (-lookup [this k]
      (-lookup this k nil))
    (-lookup [this k not-found]
      (let [v (-lookup value k not-found)]
        (if-not (= v not-found)
          (->lens v !state (conj path k))
          not-found)))
    
    IFn
    (-invoke [this k]
      (-lookup this k))
    (-invoke [this k not-found]
      (-lookup this k not-found))

    ISeqable
    (-seq [this]
      (when (pos? (count value))
        (map (fn [[k v]]
               [k (->lens v !state (conj path k))])
             value)))
    
    IAssociative
    (-contains-key? [_ k]
      (-contains-key? value k))
    (-assoc [_ k v]
      (map-lens (-assoc value k v) !state path))

    IMap
    (-dissoc [_ k]
      (map-lens (-dissoc value k) !state path))

    IEquiv
    (-equiv [_ other]
      (if (lens? other)
        (= value (-value other))
        (= value other)))

    IPrintWithWriter
    (-pr-writer [_ writer opts]
      (-write writer (pr-str value)))))

(defn vec-lens [value !state path]
  (reify
    Lens
    (-value [_] value)
    (->atom [_ extra-path]
      (lens->atom !state (vec (concat path extra-path))))
    
    ISequential

    IWithMeta
    (-with-meta [_ new-meta]
      (vec-lens (with-meta value new-meta) !state path))
    IMeta
    (-meta [_]
      (meta value))

    ICloneable
    (-clone [_]
      (vec-lens value !state path))

    ICounted
    (-count [_]
      (-count value))
    ICollection
    (-conj [_ o]
      (vec-lens (-conj value o) !state path))

    ILookup
    (-lookup [this n]
      (-nth this n nil))
    (-lookup [this n not-found]
      (-nth this n not-found))

    IFn
    (-invoke [this k]
      (-lookup this k))
    (-invoke [this k not-found]
      (-lookup this k not-found))

    IIndexed
    (-nth [this n]
      (->lens (-nth value n) !state (conj path n)))
    (-nth [this n not-found]
      (if (< n (-count value))
        (->lens (-nth value n) !state (conj path n))
        not-found))

    ISeqable
    (-seq [this]
      (when (pos? (count value))
        (map (fn [v i] (->lens v !state (conj path i))) value (range))))

    IAssociative
    (-contains-key? [_ k]
      (-contains-key? value k))
    (-assoc [this n v]
      (->lens (-assoc-n value n v) !state path))

    IStack
    (-peek [this]
      (->lens (-peek value) !state path))
    (-pop [this]
      (->lens (-pop value) !state path))
    
    IEquiv
    (-equiv [_ other]
      (if (lens? other)
        (= value (-value other))
        (= value other)))
    
    IPrintWithWriter
    (-pr-writer [_ writer opts]
      (-write writer (pr-str value)))))

(defn cloneable->lens [value !state path]
  (specify value
    Lens
    (-value [_] value)
    (->atom [_]
      (lens->atom !state path))
    
    IEquiv
    (-equiv [_ other]
      (if (lens? other)
        (= val (-value other))
        (= val other)))))

(defn ->lens [value !state path]
  (cond
   (lens? value) value
   (indexed? value) (vec-lens value !state path)
   (map? value) (map-lens value !state path)
   (satisfies? ICloneable value) (cloneable->lens value !state path)
   :else value))

(defn wrap-lens [lens & [extra-path]]
  (->atom lens extra-path))

(defn unwrap-lens [!atom]
  (->lens @!atom !atom []))

(comment
  (def !foo
    (atom {:a {:c 3, :d 4}, :b 2}))

  (def foo (unwrap-lens !foo))

  (def foo-a (:a foo))

  (def !foo-a (wrap-lens foo-a))

  (def !foo-b (wrap-lens foo [:b])))
