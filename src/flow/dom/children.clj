(ns flow.dom.children)

(defn clear! [!el]
  (swap! !el update-in [:children] empty))

(defn append-child! [!parent !child]
  (swap! !parent update-in [:children] concat [!child]))
