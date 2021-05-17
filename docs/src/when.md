# `when`

```easel
    when:
      Susceptible > 0.0 and Infected > 0.0
```

An expression defined on `let` variables and `state` variables, evaluating to a
Boolean, which determines whether or not an event is able to fire. An event must
have a true `when` _and_ be chosen stochastically based on its rate in order to
fire.
