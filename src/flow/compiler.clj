(ns flow.compiler
  (:require [flow.expand :refer [expand-macros]]))

(defn resolve-fn [called-fn]
  (let [resolved-fn (or (when (symbol? called-fn)
                          (resolve called-fn))
                        called-fn)]
    (if (and (var? resolved-fn)
             (contains? #{'clojure.core 'cljs.core}
                        (symbol (str (:ns (meta resolved-fn))))))
      (keyword "core" (str (:name (meta resolved-fn))))
      resolved-fn)))

(defn fn-call-type [[called-fn & args]]
  (condp = (resolve-fn called-fn)
    '<< :watch
    '!<< :wrap

    'if :if

    :core/case :case
    'case :case

    :core/list :list
    'list :list
    
    :core/let :let
    'let :let
    
    :core/for :for
    'for :for

    'do :do
    'fn* :fn-decl

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
         (vector? form))
    (if (keyword? (first form))
      :node
      :sub-component)

    (seq? form) (fn-call-type form)
    
    (map? form) :map
    (coll? form) :coll

    :else :primitive))

(defmulti compile-el-form
  (fn [el-form opts]
    (form-type el-form {:type :el})))

(defmulti compile-value-form
  (fn [value-form opts]
    (form-type value-form {:type :value})))

(defn compile-el [el-form macro-env]
  (-> el-form
      (expand-macros macro-env)
      (compile-el-form {:bound-syms #{}})))
