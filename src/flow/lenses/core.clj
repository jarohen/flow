(ns flow.lenses.core
  (:require [flow.lenses.common :refer [Lens lens? get-at-path assoc-at-path]]))

;; This is an implementation of a small subset of the CLJS lens
;; functionality (due to the difference in interfaces between Clojure
;; 1.6 and CLJS). It's really only meant for testing Flow.

(declare ->lens)

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

(defn ->lens [value !state path]
  (cond
    (lens? value) value
    (map? value) (map-lens value !state path)
    (or (coll? value) (seq? value)) (vec-lens value !state path)
    :else value))

