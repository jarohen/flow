(ns flow.dom.elements)

(defn text-el [s]
  (atom {:text s}))

(defn new-element [tag]
  (atom {:tag tag}))

(defn clear! [!el]
  (swap! !el update-in [:children] empty))

(defn append-child! [!parent !child]
  (swap! !parent update-in [:children] (comp #(conj % !child) vec)))

(defn insert-before! [!parent !sibling !child]
  (swap! !parent update-in [:children] #(mapcat (some-fn {!sibling [!child !sibling]} vector) %)))

(defn remove-child! [!parent !child]
  (swap! !parent update-in [:children] #(remove #{!child} %)))

(defn replace-child! [!parent !old-child !new-child]
  (if !old-child
    (swap! !parent update-in [:children] #(map (some-fn {!old-child !new-child} identity) %))
    (append-child! !parent !new-child)))

(defn next-sibling [!parent !child]
  (->> (:children @!parent)
       (partition-all 2 1)
       (filter (comp #{!child} first))
       first
       second))

(defn add-event-listener! [!el event listener]
  (swap! !el assoc-in [:listeners event] listener))

(defn null-elem []
  (atom nil))

(defn ->el [el-ish]
  (cond
    (coll? el-ish) (map ->el el-ish)

    (and (= clojure.lang.Atom (class el-ish))
         (or (:text @el-ish)
             (:tag @el-ish)))
    el-ish

    (or (nil? el-ish)
        (and (seq? el-ish)
             (empty? el-ish)))
    (null-elem)
    
    :else (text-el (if (string? el-ish)
                     el-ish
                     (pr-str el-ish)))))

