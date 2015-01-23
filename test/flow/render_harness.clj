(ns flow.render-harness
  (:require [flow.dom.render :refer [schedule-rendering-frame]]))

(def ^:dynamic *!render-queue* nil)

(defn render-frame! []
  (let [frame (dosync
               (when-let [frame (first @*!render-queue*)]
                 (alter *!render-queue* subvec 1)
                 frame))]
    (when frame
      (frame))))

(defn render-all-frames! []
  (doseq [frame (dosync
                 (let [frames @*!render-queue*]
                   (ref-set *!render-queue* [])
                   frames))]
    (frame)))


(defn with-render-harness* [f]
  (with-redefs [schedule-rendering-frame (fn [frame]
                                           (dosync
                                            (alter *!render-queue* conj frame)))]
    (binding [*!render-queue* (ref [])]
      (f))))

(defmacro with-render-harness [& body]
  `(with-render-harness* (fn [] ~@body)))

(defn el-snapshot [!el]
    (-> @!el
        (update-in [:children] #(map el-snapshot %))
        (dissoc :uid)))
