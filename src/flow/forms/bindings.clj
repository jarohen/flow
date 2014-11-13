(ns flow.forms.bindings
  (:require [flow.compiler :as fc]
            [clojure.set :as set]))

(defn bind-syms [binding]
  (letfn [(bind-syms* [binding] (cond
                                   (map? binding) (let [{:keys [as]
                                                         ks :keys} binding]
                                                    (concat (mapcat bind-syms* ks)
                                                            (bind-syms* as)
                                                            (->> (dissoc binding :keys :as)
                                                                 keys 
                                                                 (mapcat bind-syms*))))

                                   (vector? binding) (-> (mapcat bind-syms* binding)
                                                         set
                                                         (disj '& :as))
                              
                                   (symbol? binding) [(symbol (name binding))]))]
    
    (set (bind-syms* binding))))

(defn parse-bindings [bindings]
  (let [paired-bindings (partition-all 2 bindings)]
    (for [[binding value] paired-bindings]
      {:binding binding
       :bind-syms (bind-syms binding)
       :value value})))

(defn compile-el-bindings [bindings opts]
  (reduce (fn [{:keys [compiled-bindings opts]} {:keys [binding bind-syms value]}]
            {:compiled-bindings (conj compiled-bindings
                                      {:value-fn `(fn []
                                                    ~(fc/compile-value-form value opts))
                                       :destructure-fn `(fn [~binding]
                                                          ~(->> (for [bind-sym bind-syms]
                                                                  [`(quote ~bind-sym) bind-sym])
                                                                (into {})))
                                       :pk-fn (:flow.core/pk (meta value))})
             :opts (update-in opts [:bound-syms] set/union bind-syms)})

          {:compiled-bindings []
           :opts opts}

          (parse-bindings bindings)))

(defn compile-value-bindings [bindings opts]
  (reduce (fn [{:keys [compiled-bindings opts]} {:keys [binding bind-syms value]}]
            {:compiled-bindings (conj compiled-bindings
                                      [binding (fc/compile-value-form value opts)])
             :opts (update-in opts [:bound-syms] set/difference bind-syms)})

          {:compiled-bindings []
           :opts opts}

          (parse-bindings bindings)))

