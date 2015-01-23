(ns flow.cursors)

;; Most of this adapted from either from CLJS core or Om, thanks David
;; Nolan & contributors for the design ideas, inspiration and code :)

;; The Clojure implementation is a small subset of the CLJS cursor
;; functionality (due to the difference in interfaces between Clojure
;; 1.6 and CLJS). It's really only meant for testing Flow.

(defprotocol Cursor
  (-value [_])
  (-!state [_])
  (-path [_])
  (->atom [_ extra-path]))

(defprotocol Keyable
  (-keyed-by [_ f]))

(defn cursor? [v]
  (satisfies? Cursor v))

(defn get-at-path [m [p & more-path :as path]]
  (if (and m (seq path))
    (cond
      (and (vector? p) (= (first p) ::pk))
      (let [[_ key-value] p
            {key-fn ::key-fn} (meta p)]
        (get-at-path (first (filter (comp #(= key-value %) key-fn) m)) more-path))

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
      (and (vector? p) (= (first p) ::pk))
      (let [[_ key-value] p
            {key-fn ::key-fn} (meta p)]
        (into (empty m) (for [el m]
                          (if (= key-value (key-fn el))
                            (assoc-at-path el more-path v)
                            el))))

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
(defn cursor->atom [!state path]
  (reify
    Cursor
    (-value [this] (deref this))
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (cursor->atom !state (vec (concat path extra-path))))
    
    clojure.lang.IDeref
    (deref [this]
      (get-at-path @!state path))))

#+cljs
(defn cursor->atom [!state path]
  ;; This part adapted from CLJS core.
  (reify
    Cursor
    (-value [this] @this)
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (cursor->atom !state (vec (concat path extra-path))))
    
    IEquiv
    (-equiv [this other]
      (and (cursor? other)
           (identical? !state (-!state other))
           (= path (path other))))

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

(declare ->cursor)

#+clj
(defn map-cursor [value !state path]
  (reify
    Cursor
    (-value [_] value)
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (cursor->atom !state (vec (concat path extra-path))))
    
    clojure.lang.ILookup
    (valAt [this k]
      (get this k nil))
    (valAt [this k not-found]
      (let [v (get value k not-found)]
        (if-not (= v not-found)
          (->cursor v !state (conj path k))
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
               (clojure.lang.MapEntry. k (->cursor v !state (conj path k))))
             value)))
    
    clojure.lang.IPersistentMap
    (assoc [_ k v]
      (map-cursor (assoc value k v) !state path))
    (without [_ k]
      (map-cursor (dissoc value k) !state path))))

#+cljs
(defn map-cursor [value !state path]
  (reify
    Cursor
    (-value [_] value)
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (cursor->atom !state (vec (concat path extra-path))))
    
    IWithMeta
    (-with-meta [_ new-meta]
      (map-cursor (with-meta value new-meta) !state path))
    IMeta
    (-meta [_]
      (meta value))

    ICloneable
    (-clone [_]
      (map-cursor value !state path))
    
    ICounted
    (-count [_]
      (count value))

    ICollection
    (-conj [this o]
      (->cursor (-conj value o) !state path))

    ILookup
    (-lookup [this k]
      (-lookup this k nil))
    (-lookup [this k not-found]
      (let [v (-lookup value k not-found)]
        (if-not (= v not-found)
          (->cursor v !state (conj path k))
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
               [k (->cursor v !state (conj path k))])
             value)))
    
    IAssociative
    (-contains-key? [_ k]
      (-contains-key? value k))
    (-assoc [_ k v]
      (map-cursor (-assoc value k v) !state path))

    IMap
    (-dissoc [_ k]
      (map-cursor (-dissoc value k) !state path))

    IEquiv
    (-equiv [_ other]
      (if (cursor? other)
        (= value (-value other))
        (= value other)))

    IPrintWithWriter
    (-pr-writer [_ writer opts]
      (-write writer (pr-str value)))))

#+clj
(defn vec-cursor [value !state path key-fn]
  (letfn [(pk [v i]
            (if key-fn
              (with-meta [::pk (key-fn v)]
                {::key-fn key-fn})
              i))]
    (reify
      Cursor
      (-value [_] value)
      (-!state [_] !state)
      (-path [_] path)
      (->atom [_ extra-path]
        (cursor->atom !state (vec (concat path extra-path))))

      Keyable
      (-keyed-by [_ f]
        (vec-cursor value !state path f))
    
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
        (->cursor (nth value n) !state (conj path n)))
      (nth [this n not-found]
        (if (< n (count value))
          (->cursor (nth value n) !state (conj path n))
          not-found))

      clojure.lang.Seqable
      (seq [this]
        (when (not-empty value)
          (map-indexed (fn [i v]
                         (->cursor v !state (conj path (pk v i))))
                       value)))

      clojure.lang.Associative
      (assoc [this n v]
        (->cursor (assoc value n v) !state path)))))

#+cljs
(defn vec-cursor [value !state path key-fn]
  (letfn [(pk [v i]
            (if key-fn
              (with-meta [::pk (key-fn v)]
                {::key-fn key-fn})
              i))]
    (reify
      Cursor
      (-value [_] value)
      (-!state [_] !state)
      (-path [_] path)
      (->atom [_ extra-path]
        (cursor->atom !state (vec (concat path extra-path))))

      Keyable
      (-keyed-by [_ f]
        (vec-cursor value !state path f))
      
      ISequential

      IWithMeta
      (-with-meta [_ new-meta]
        (vec-cursor (with-meta value new-meta) !state path key-fn))
      IMeta
      (-meta [_]
        (meta value))

      ICloneable
      (-clone [_]
        (vec-cursor value !state path key-fn))

      ICounted
      (-count [_]
        (count value))
      ICollection
      (-conj [_ o]
        (vec-cursor (conj value o) !state path key-fn))

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
        (let [v (nth value n)]
          (->cursor v !state (conj path (pk v n)))))
      
      (-nth [this n not-found]
        (if (< n (-count value))
          (let [v (nth value n)]
            (->cursor v !state (conj path (pk v n))))
          
          not-found))

      ISeqable
      (-seq [this]
        (when (not-empty value)
          (map-indexed (fn [i v]
                         (->cursor v !state (conj path (pk v i))))
                       value)))

      IAssociative
      (-contains-key? [_ k]
        (-contains-key? value k))
      (-assoc [this n v]
        (vec-cursor (-assoc-n value n v) !state path key-fn))

      IStack
      (-peek [this]
        (vec-cursor (-peek value) !state path key-fn))
      (-pop [this]
        (vec-cursor (-pop value) !state path key-fn))
      
      IEquiv
      (-equiv [_ other]
        (if (cursor? other)
          (= value (-value other))
          (= value other)))
      
      IPrintWithWriter
      (-pr-writer [_ writer opts]
        (-write writer (pr-str value))))))

#+clj
(defn set-cursor [value !state path key-fn]
  (letfn [(pk [v]
            (if key-fn
              (with-meta [::pk (key-fn v)]
                {::key-fn key-fn})
              v))]
    (reify
      Cursor
      (-value [_] value)
      (-!state [_] !state)
      (-path [_] path)
      (->atom [_ extra-path]
        (cursor->atom !state (vec (concat path extra-path))))

      Keyable
      (-keyed-by [_ f]
        (vec-cursor value !state path f))
    
      clojure.lang.Sequential

      clojure.lang.IPersistentCollection
      (count [_]
        (count value))

      clojure.lang.IFn
      (invoke [this k]
        (get this k))
      (invoke [this k not-found]
        (get this k not-found))

      clojure.lang.Seqable
      (seq [this]
        (when (not-empty value)
          (map (fn [v]
                 (->cursor v !state (conj path (pk v))))
               value))))))

#+cljs
(defn set-cursor [value !state path key-fn]
  (letfn [(pk [v]
            (if key-fn
              (with-meta [::pk (key-fn v)]
                {::key-fn key-fn})
              v))]
    (reify
      Cursor
      (-value [_] value)
      (-!state [_] !state)
      (-path [_] path)
      (->atom [_ extra-path]
        (cursor->atom !state (vec (concat path extra-path))))

      Keyable
      (-keyed-by [_ f]
        (set-cursor value !state path f))
      
      IWithMeta
      (-with-meta [_ new-meta]
        (set-cursor (with-meta value new-meta) !state path key-fn))
      
      IMeta
      (-meta [_]
        (meta value))

      ICloneable
      (-clone [_]
        (set-cursor value !state path key-fn))

      ICounted
      (-count [_]
        (count value))
      ICollection
      (-conj [_ o]
        (set-cursor (conj value o) !state path key-fn))

      IEmptyableCollection
      (-empty [coll] (with-meta #{} meta))

      ILookup
      (-lookup [this v]
        (-lookup this v nil))
      
      (-lookup [this v not-found]
        (if (-lookup value v)
          (->cursor v !state (conj path (pk v)))
          not-found))

      ISet
      (-disjoin [this v]
        (set-cursor (-disjoin value v) !state path key-fn))
      
      IFn
      (-invoke [this k]
        (-lookup this k))
      (-invoke [this k not-found]
        (-lookup this k not-found))

      ISeqable
      (-seq [this]
        (when (not-empty value)
          (map (fn [v]
                 (->cursor v !state (conj path (pk v))))
               value)))

      IEquiv
      (-equiv [_ other]
        (if (cursor? other)
          (= value (-value other))
          (= value other)))
      
      IPrintWithWriter
      (-pr-writer [_ writer opts]
        (-write writer (pr-str value))))))

#+cljs
(defn cloneable->cursor [value !state path]
  (specify value
    Cursor
    (-value [_] value)
    (-!state [_] !state)
    (-path [_] path)
    (->atom [_ extra-path]
      (cursor->atom !state (vec (concat path extra-path))))
    
    IEquiv
    (-equiv [_ other]
      (if (cursor? other)
        (= val (-value other))
        (= val other)))))

(defn ->cursor [value !state path]
  (cond
    (cursor? value) value

    (map? value) (map-cursor value !state path)
    (set? value) (set-cursor value !state path nil)
    (or (coll? value) (seq? value)) (vec-cursor value !state path nil)

    #+cljs (satisfies? ICloneable value)
    #+cljs (cloneable->cursor value !state path)

    :else value))

(defn keyed-by [f coll]
  (cond
    (satisfies? Keyable coll) (-keyed-by coll f)

    (or (sequential? coll)
        #+cljs (seqable? coll))
    (into (empty coll)
          (map (fn [el]
                 (if (cursor? el)
                   (->cursor (-value el)
                             (-!state el)
                             (concat (butlast (-path el)) [(with-meta [::pk (f el)]
                                                             {::key-fn f})]))
                   el))
               coll))
    
    :else coll))

