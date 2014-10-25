(ns flow.dom.attributes)

(defn set-id! [!el id]
  (swap! !el assoc :id id))

(defn set-style! [!el attr value]
  (swap! !el assoc-in [:style attr] value))

(defn set-attr! [!el attr value]
  (swap! !el assoc-in [:attrs attr] value))
