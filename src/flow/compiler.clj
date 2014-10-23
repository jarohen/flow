(ns flow.compiler)

(defn form-type [el-form]
  (cond
    (string? el-form) :text
    
    (and (vector? el-form)
         (keyword (first el-form)))
    :node))

(defmulti compile-form #'form-type)

(require 'flow.forms.text)
(require 'flow.forms.node)

(defn compile-el [el-form]
  (compile-form el-form))
