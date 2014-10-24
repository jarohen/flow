(ns flow.compiler)

(defn el-form-type [el-form]
  (cond
    (string? el-form) :text
    
    (and (vector? el-form)
         (keyword (first el-form))) :node))

(defmulti compile-el-form
  (fn [el-form opts]
    (el-form-type el-form)))

(defn value-form-type [value-form]
  (cond
    (list? value-form) :fn-call
    (coll? value-form) :coll
    :else :primitive))

(defmulti compile-value-form
  (fn [value-form opts]
    (value-form-type value-form)))

(require 'flow.forms.text)
(require 'flow.forms.node)
(require 'flow.forms.primitive)

(defn compile-el [el-form]
  ;; TODO I think the big macro-expand goes in here?!
  (compile-el-form el-form {}))
