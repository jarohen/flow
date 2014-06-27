(ns flow.exit)

(defmulti wrap-exit-vectors-in-form
  (fn [form f]
    (symbol (name (first form)))))

(defn wrap-exit-vectors [form f]
  (cond
   (vector? form) (f form)
   (seq? form) (wrap-exit-vectors-in-form form f)
   :else form))

(defmethod wrap-exit-vectors-in-form 'if [[_ test then else] f]
  `(if ~test
     ~(wrap-exit-vectors then f)
     ~(wrap-exit-vectors else f)))

(defmethod wrap-exit-vectors-in-form 'let [[_ bindings & body] f]
  `(let ~bindings
     ~@(butlast body)
     ~(wrap-exit-vectors (last body) f)))

(defmethod wrap-exit-vectors-in-form 'for [[_ bindings & body] f]
  `(for ~bindings
     ~@(butlast body)
     ~(wrap-exit-vectors (last body) f)))

(defmethod wrap-exit-vectors-in-form 'do [[_ & body] f]
  `(do
     ~@(butlast body)
     ~(wrap-exit-vectors (last body) f)))

(defmethod wrap-exit-vectors-in-form 'case [[_ expr & pairs] f]
  `(case ~expr
     ~@(mapcat (fn [[test res]] 
                 [test (wrap-exit-vectors res f)])
               pairs)))

(defmethod wrap-exit-vectors-in-form :default [form _]
  form)
