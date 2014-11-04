(ns flow.protocols)

(defprotocol CompiledIdentity
  (hard-deps [_])
  (soft-deps [_])

  (declarations [_])
  (build-form [_]))

(extend-protocol CompiledIdentity
  nil
  (hard-deps [_] nil)
  (soft-deps [_] nil)

  (declarations [_] nil)
  (build-form [_] nil))

(defprotocol CompiledValue
  (value-deps [_])
  (inline-value-form [_]))

(extend-protocol CompiledValue
  nil
  (value-deps [_] nil)
  (inline-value-form [_] nil))
