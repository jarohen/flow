(ns flow.forms.for
  (:require [flow.util :as u]
            [flow.dom :as fd]
            [flow.protocols :as fp]
            [flow.diff :refer [vector-diff]]))

(defn for->el [quoted-deps for-values make-body-el]
  (let [!last-els (atom nil)
        $marker (fd/null-elem)]

    (letfn [(with-el [{:keys [state $el], value-keys :keys, :as value} el-cache]
              (if-let [{:keys [$!body $el]} (get el-cache value-keys)]
                (assoc value
                  :$!body $!body
                  :$el $el)

                (let [$!body (make-body-el)]
                  (assoc value
                    :$!body $!body
                    :$el (fp/build $!body state)
                    :new? true))))]
      
      (reify fp/DynamicValue
        (should-update? [_ updated-vars]
          (u/deps-updated? quoted-deps updated-vars))

        (build [_ state]
          (let [initial-els (->> (for-values state)
                                 (map #(with-el % {}))
                                 (map #(dissoc % :new?)))]

            (reset! !last-els initial-els)

            (or (seq (map :$el initial-els))
                $marker)))

        (handle-update! [_ old-state new-state updated-vars]
          )))))

  

