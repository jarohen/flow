(ns flow.expand
  (:require [cljs.analyzer :refer [macroexpand-1] :rename {macroexpand-1 cljs-macroexpand-1}]
            [clojure.walk :as w]))

(def ^:dynamic *macroexpand-env*)
(def ^:dynamic *macroexpand-1* #(cljs-macroexpand-1 *macroexpand-env* %))

(defn macroexpand-until-known [form]
  (loop [form form]
    (let [expanded-form (*macroexpand-1* form)]
      (if (or (#{'let 'case 'if 'do 'for 
                 'clojure.core/let 'clojure.core/case 'clojure.core/for
                 'el 'flow.core/el} (first form))
              
              (= form expanded-form))
        form
        
        (recur expanded-form)))))

(defn expand-macros [elem env]
  (binding [*macroexpand-env* env]
    (w/postwalk macroexpand-until-known
                elem)))
