(ns flow.state)

(defprotocol Context
  (-read-lens [_ lens]))

(def ^:dynamic *ctx* nil)

(defn read-lens [lens]
  (-read-lens *ctx* lens))


