(ns flow.compiler
  (:require [flow.expand :refer [expand-macros]]))

(defn fn-call-type [form]
  (condp = (first form)
    '<< :unwrap-lens

    :fn-call))

(defn form-type [form {:keys [type]}]
  (cond
    (string? form) (case type
                        :el :text
                        :value :primitive)
    
    (and (= type :el)
         (vector? form)
         (keyword (first form)))
    :node

    (list? form) (fn-call-type form)
    
    (coll? form) :coll

    :else :primitive))

(defmulti compile-el-form
  (fn [el-form opts]
    (form-type el-form {:type :el})))

(defn value-form-type [value-form]
  (cond
    (list? value-form) (fn-call-type value-form)
    (coll? value-form) :coll
    :else :primitive))

(defmulti compile-value-form
  (fn [value-form opts]
    (form-type value-form {:type :value})))

(require 'flow.forms.text)
(require 'flow.forms.node)
(require 'flow.forms.primitive)
(require 'flow.forms.lenses)

(defn compile-el [el-form macro-env]
  (-> el-form
      (expand-macros macro-env)
      (compile-el-form {})))
