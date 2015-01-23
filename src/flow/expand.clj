(ns flow.expand
  (:require [clojure.walk :as w]))

(def ^:dynamic *macro-env*)
(def ^:dynamic *macroexpand-1*)

(defn environment []
  (if (and (find-ns 'cljs.env)
           (some-> (ns-resolve 'cljs.env '*compiler*)
                   deref))
    :cljs
    :clj))

(defn expand-1 []
  (case (environment)
    :clj clojure.core/macroexpand-1
    :cljs (eval #((ns-resolve 'cljs.analyzer 'macroexpand-1) *macro-env* %))))

(defn prewalk-with-meta [f form]
  (w/walk (fn [form]
            (let [walked-form (prewalk-with-meta f form)]
              (if (instance? clojure.lang.IObj walked-form)
                (with-meta walked-form
                  (meta form))
                
                walked-form)))

          identity

          (f form)))

(def leave-call-alone?
  (set (->> (concat (for [fn-ns ['clojure.core 'cljs.core]
                          :when (find-ns fn-ns)
                          fn-name ['case 'for 'let 'list]]
                      (ns-resolve fn-ns fn-name))
                    [(ns-resolve (doto 'flow.core create-ns) 'el)]
                    ['let 'if 'for 'case 'list])
            
            (remove nil?))))

(defn macroexpand-until-known [form]
  (loop [form form]
    (if (seq? form)
      (let [expanded-form (*macroexpand-1* form)]
        (if (or (= form expanded-form)
                (leave-call-alone? (or (resolve (first form))
                                       (first form))))
          form
          
          (recur expanded-form)))

      form)))

(defn expand-macros [form macro-env]
  (binding [*macro-env* macro-env
            *macroexpand-1* (expand-1)]
    (prewalk-with-meta macroexpand-until-known form)))
