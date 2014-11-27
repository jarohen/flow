(ns flow.forms.case
  (:require #+clj [flow.compiler :as fc]))

(defn build-case [value-fn values build-branch]
  (fn []
    (letfn [(update-case [old-value update-current-branch!]
              (let [new-value (get values (value-fn) ::default)
                    
                    new-branch (if (not= old-value new-value)
                                 (build-branch new-value)
                                 update-current-branch!)
                     
                    [$branch-el update-branch!] (new-branch)]
                
                [$branch-el #(update-case new-value update-branch!)]))]
      
      (update-case ::init nil))))

#+clj
(defn parse-case-clauses [clauses]
  (let [paired-clauses (partition-all 2 clauses)]
    {:clauses (filter (comp #{2} count) paired-clauses)
     :default (let [last-clause (last paired-clauses)]
                (when (= 1 (count last-clause))
                  (first last-clause)))}))

#+clj
(defmethod fc/compile-el-form :case [[_ value & case-clauses] opts]
  (let [{:keys [clauses default]} (parse-case-clauses case-clauses)]
    `(build-case (fn []
                   ~(fc/compile-value-form value opts))

                 #{~@(map first clauses)}
                 
                 (fn [case-value#]
                   (case case-value#
                     ~@(->> (for [[test-value expr] clauses]
                              [test-value (fc/compile-el-form expr opts)])
                            (apply concat))
                     
                     ~@(when default
                         [::default (fc/compile-el-form default opts)]))))))

#+clj
(defmethod fc/compile-value-form :case [[_ value & case-clauses] opts]
  (let [{:keys [clauses default]} (parse-case-clauses case-clauses)]
    `(case ~(fc/compile-value-form value opts)
       ~@(->> (for [[test-value expr] clauses]
                [test-value (fc/compile-value-form expr opts)])
              (apply concat))
                     
       ~@(when default
           [(fc/compile-value-form default opts)]))))
