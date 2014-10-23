(ns flow.dom.attributes)

(defn set-style! [!el attr value]
  (swap! !el assoc-in [:style attr] value))
