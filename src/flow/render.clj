(ns flow.render)

(def ^:dynamic *!render-queue* nil)

(defmacro foo-with-render-queue [& body]
  `(binding [*!render-queue* (ref [])]
     ~@body))

(defn foo-render-frame! []
  (let [frame (dosync
               (let [frame (first @*!render-queue*)]
                 (alter *!render-queue* (comp vec rest))
                 frame))]
    (when frame
      (frame))))

(defn schedule-rendering-frame [f]
  (dosync
   (alter *!render-queue* conj f)))
