(ns flow.el
  (:require [flow.dom.children :as fdc]
            [flow.state :as fs]
            [flow.render :as fr]))

(defn root [$container el]
  (fr/schedule-rendering-frame
   (fn []
     (binding [fs/*ctx* (reify fs/Context
                          (-read-lens [_ lens]
                            (prn "dereffing!" lens)
                            @lens))]

       (let [[$el update-el!] ((el))]
         (fdc/clear! $container)
         (fdc/append-child! $container $el))))))

(defn render-el [el]
  (fn []
    (let [parent-ctx fs/*ctx*]
      (binding [fs/*ctx* (reify fs/Context
                           (-read-lens [_ lens]
                             (prn "dereffing in el:" lens)
                             (fs/-read-lens parent-ctx lens)))]
        (el)))))
