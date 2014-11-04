(ns flow.lenses)

;; Most of this adapted from either from CLJS core or Om, thanks David
;; Nolan & contributors for the design ideas, inspiration and code :)

;; The Clojure implementation is a small subset of the CLJS lens
;; functionality (due to the difference in interfaces between Clojure
;; 1.6 and CLJS). It's really only meant for testing Flow.

(defprotocol Lens
  (-value [_])
  (-!state [_])
  (-path [_])
  (->atom [_ extra-path]))

(defn lens? [v]
  (satisfies? Lens v))

(defn get-at-path [m [p & more-path :as path]]
  (if (and m (seq path))
    (cond
      (or #+clj (instance? clojure.lang.ILookup m)
          #+cljs (satisfies? #+cljs ILookup m)
          (nil? m))
      
      (get-at-path (get m p) more-path)
      
      (number? p)
      (get-at-path (nth m p) more-path))
    
    m))

(defn assoc-at-path [m [p & more-path :as path] v]
  (if (seq path)
    (cond
      (or #+clj (instance? clojure.lang.Associative m)
          #+cljs (satisfies? #+cljs IAssociative m)
          (nil? m))
      (assoc m p (assoc-at-path (get m p) more-path v))

      (and (seq? m)
           (number? p))
      (if (zero? p)
        (cons (assoc-at-path (first m) more-path v)
              (rest m))
        (cons (first m)
              (assoc-at-path (rest m) (cons (dec p) more-path) v))))
    
    v))

#+clj
(defn lens->atom [!state path]
  (reify
    Lens
    (-value [this] (deref this))
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (lens->atom !state (vec (concat path extra-path))))
    
    clojure.lang.IDeref
    (deref [this]
      (get-at-path @!state path))))

#+cljs
(defn lens->atom [!state path]
  ;; This part adapted from CLJS core.
  (reify
    Lens
    (-value [this] @this)
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (lens->atom !state (vec (concat path extra-path))))
    
    IEquiv
    (-equiv [this other]
      (identical? this other))

    IMeta
    (-meta [_]
      {:!state !state
       :path path})
    
    IDeref
    (-deref [_]
      (get-at-path @!state path))

    IReset
    (-reset! [this new-value]
      (let [old-value (-deref this)]
        (swap! !state assoc-at-path path new-value)
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

    IHash
    (-hash [this]
      (goog/getUid this))))

(declare ->lens)

#+clj
(defn map-lens [value !state path]
  (reify
    Lens
    (-value [_] value)
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (lens->atom !state (vec (concat path extra-path))))
    
    clojure.lang.ILookup
    (valAt [this k]
      (get this k nil))
    (valAt [this k not-found]
      (let [v (get value k not-found)]
        (if-not (= v not-found)
          (->lens v !state (conj path k))
          not-found)))
    
    clojure.lang.IFn
    (invoke [this k]
      (get this k))
    (invoke [this k not-found]
      (get this k not-found))

    clojure.lang.Seqable
    (seq [this]
      (when (pos? (count value))
        (map (fn [[k v]]
               (clojure.lang.MapEntry. k (->lens v !state (conj path k))))
             value)))
    
    clojure.lang.IPersistentMap
    (assoc [_ k v]
      (map-lens (assoc value k v) !state path))
    (without [_ k]
      (map-lens (dissoc value k) !state path))))

#+cljs
(defn map-lens [value !state path]
  (reify
    Lens
    (-value [_] value)
    (-!state [_] !state)
    (-path [_] path)
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
      (count value))

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

#+clj
(defn vec-lens [value !state path]
  (reify
    Lens
    (-value [_] value)
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (lens->atom !state (vec (concat path extra-path))))
    
    clojure.lang.Sequential

    clojure.lang.IPersistentCollection
    (count [_]
      (count value))

    clojure.lang.ILookup
    (valAt [this n]
      (nth this n nil))
    (valAt [this n not-found]
      (nth this n not-found))

    clojure.lang.IFn
    (invoke [this k]
      (get this k))
    (invoke [this k not-found]
      (get this k not-found))

    clojure.lang.Indexed
    (nth [this n]
      (->lens (nth value n) !state (conj path n)))
    (nth [this n not-found]
      (if (< n (count value))
        (->lens (nth value n) !state (conj path n))
        not-found))

    clojure.lang.Seqable
    (seq [this]
      (when (pos? (count value))
        (map (fn [v i] (->lens v !state (conj path i))) value (range))))

    clojure.lang.Associative
    (assoc [this n v]
      (->lens (assoc value n v) !state path))))

#+cljs
(defn vec-lens [value !state path]
  (reify
    Lens
    (-value [_] value)
    (-!state [_] !state)
    (-path [_] path)
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
      (count value))
    ICollection
    (-conj [_ o]
      (vec-lens (conj value o) !state path))

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
      (->lens (nth value n) !state (conj path n)))
    (-nth [this n not-found]
      (if (< n (-count value))
        (->lens (nth value n) !state (conj path n))
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

#+cljs
(defn cloneable->lens [value !state path]
  (specify value
    Lens
    (-value [_] value)
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (lens->atom !state (vec (concat path extra-path))))
    
    IEquiv
    (-equiv [_ other]
      (if (lens? other)
        (= val (-value other))
        (= val other)))))

(defn ->lens [value !state path]
  (cond
    (lens? value) value

    (map? value) (map-lens value !state path)
    (or (coll? value) (seq? value)) (vec-lens value !state path)

    #+cljs (satisfies? ICloneable value)
    #+cljs (cloneable->lens value !state path)

    :else value))
