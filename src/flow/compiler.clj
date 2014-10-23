(ns flow.compiler)

(defn el-form-type [el-form]
  (cond
    (string? el-form) :text
    
    (and (vector? el-form)
         (keyword (first el-form))) :node))

(defmulti compile-el-form #'el-form-type)

(defn value-form-type [el-form]
  (cond
    (list? el-form) :fn-call
    (coll? el-form) :coll
    :else :primitive))

(defmulti compile-value-form #'value-form-type)

(require 'flow.forms.text)
(require 'flow.forms.node)
(require 'flow.forms.primitive)

(defn compile-el [el-form]
  ;; TODO I think the big macro-expand goes in here?!
  (compile-el-form el-form))
