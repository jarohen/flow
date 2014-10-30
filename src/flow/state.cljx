(ns flow.state)

(defprotocol Context
  (-read-lens [_ lens])
  (-peek-lens [_ lens]))

(def ^:dynamic *ctx*
  (reify Context
    (-read-lens [_ lens]
      (deref lens))

    (-peek-lens [_ lens]
      (deref lens))))

(defn read-lens [lens]
  (-read-lens *ctx* lens))

(defn peek-lens [lens]
  (-peek-lens *ctx* lens))

(def ^:dynamic *state* {})

