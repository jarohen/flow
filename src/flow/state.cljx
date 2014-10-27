(ns flow.state)

(defprotocol Context
  (-read-lens [_ lens]))

(def ^:dynamic *ctx*
  (reify Context
    (-read-lens [_ lens]
      (deref lens))))

(defn read-lens [lens]
  (-read-lens *ctx* lens))

(def ^:dynamic *state* {})

