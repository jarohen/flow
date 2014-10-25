(ns flow.render)

(def ^:dynamic *!render-queue* nil)

(defmacro foo-with-render-queue [& body]
  `(binding [*!render-queue* (ref [])]
     ~@body))

(defn foo-render-frame! []
  (let [frame (dosync
               (when-let [frame (first @*!render-queue*)]
                 (alter *!render-queue* subvec 1)
                 frame))]
    (when frame
      (frame))))

(defn foo-render-all-frames! []
  (doseq [frame (dosync
                 (let [frames @*!render-queue*]
                   (ref-set *!render-queue* [])
                   frames))]
    (frame)))

(defn schedule-rendering-frame [f]
  (dosync
   (alter *!render-queue* conj f)))
