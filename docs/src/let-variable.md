# `let`-Declared Variables

```easel
  let s_Initial = 999.0
  ...
  let beta =
    cond:
      gamma              if time <= policy_days0
      gamma + 0.1486983  if time <= policy_days1
      gamma + 0.0717734  otherwise
```

Variables declared with `let` are bound to expressions - these may be constants,
in the case of `s_Initial`, or more complex, state-dependent expressions, in the
case of `beta`. As they can depend on `state` variables, unlike
[`parameter`](./parameter-variable.md)s, `let`s may change their value
throughout a model's simulation. Unlike `state` variables, `let`s cannot be
directly modified in an `event`'s `effect` section.
