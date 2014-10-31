(ns flow.lenses.common)

;; Most of this adapted from either from CLJS core or Om, thanks David
;; Nolan & contributors for the design ideas, inspiration and code :)

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
