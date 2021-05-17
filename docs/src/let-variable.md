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

Variables declared with `let` may represent a constant (in the case of
`s_Initial`) or an expression (in the case of `beta`). During model
simulation/execution, at use sites, `let` variable right-hand sides are
inlined. It follows that the value of a `let` variable might not remain the same
throughout a model (indeed, `beta` does not). It is also the case that `let`
variables cannot be modified in an `event`'s `effect` section.
