(ns flow.compiler
  (:require [flow.expand :refer [expand-macros]]))

(defn resolve-fn [called-fn]
  (let [resolved-fn (or (when (symbol? called-fn)
                          (resolve called-fn))
                        called-fn)]
    (if (and (var? resolved-fn)
             (contains? #{'clojure.core 'cljs.core} (symbol (str (:ns (meta resolved-fn))))))
      (keyword "core" (str (:name (meta resolved-fn))))
      resolved-fn)))

(defn fn-call-type [[called-fn & args]]
  (condp = (resolve-fn called-fn)
    '<< :unwrap-lens

    'if :if
    :core/case :case
    :core/let :let
    :core/for :for

    'quote (throw (UnsupportedOperationException. "(quote ...) not supported in Flow's 'el' (yet?!)"))
    'loop* (throw (UnsupportedOperationException. "loop/recur not supported in Flow's 'el'"))
    
    :fn-call))

(defn form-type [form {:keys [type]}]
  (cond
    (string? form) (case type
                        :el :text
                        :value :primitive)

    (symbol? form) :symbol
    
    (and (= type :el)
         (vector? form)
         (keyword (first form)))
    :node

    (list? form) (fn-call-type form)
    
    (map? form) :map
    (coll? form) :coll

    :else :primitive))

(defmulti compile-el-form
  (fn [el-form opts]
    (form-type el-form {:type :el})))

(defmulti compile-value-form
  (fn [value-form opts]
    (form-type value-form {:type :value})))

(require 'flow.forms.text)
(require 'flow.forms.node)
(require 'flow.forms.primitive)
(require 'flow.forms.lenses)
(require 'flow.forms.collections)
(require 'flow.forms.symbols)
(require 'flow.forms.fn-calls)
(require 'flow.forms.do)
(require 'flow.forms.if)
(require 'flow.forms.case)

(defn compile-el [el-form macro-env]
  (-> el-form
      (expand-macros macro-env)
      (compile-el-form {:dynamic-syms #{}})))
