(ns flow.expand
  (:require [cljs.analyzer :as cljs]
            [clojure.walk :as w]))

(def ^:dynamic *macroexpand-env*)
(def ^:dynamic *macroexpand-1* #(cljs/macroexpand-1 *macroexpand-env* %))

(def unalias-sym
  {'clojure.core/let 'let
   'clojure.core/case 'case
   'clojure.core/for 'for})

(defn unalias-form [[fn-sym & args :as form]]
  (with-meta `(~(or (unalias-sym fn-sym)
                    fn-sym)

               ~@args)
    (meta form)))

(defn macroexpand-until-known [form]
  (loop [form form]
    (if (seq? form)
      (let [expanded-form (*macroexpand-1* form)]
        (if (or (= form expanded-form)
                (#{'let 'case 'if 'do 'for
                   'clojure.core/let 'clojure.core/case 'clojure.core/for
                   'el 'flow.core/el} (first form)))
          (with-meta (-> form unalias-form)
            (meta form))
          
          (recur (with-meta expanded-form
                   (meta form)))))

      form)))

(defn postwalk-with-meta [f form]
  (w/walk (fn [form]
            (if (instance? clojure.lang.IMeta form)
              (with-meta (postwalk-with-meta f form)
                (meta form))
              (postwalk-with-meta f form)))
          f
          form))

(defn expand-macros [elem env]
  (binding [*macroexpand-env* env]
    (postwalk-with-meta macroexpand-until-known elem)))

