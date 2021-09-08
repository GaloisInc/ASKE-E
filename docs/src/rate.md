# `rate`

```easel
    rate:
      beta * contact_multiplier * Susceptible * Infected / total_pop
```

An expression defined on `let` variables and `state` variables, evaluating to a
real, which represents the frequency with which an event fires. In simulation,
events would be chosen stochastically based on this value - a rate of `0.8`
means at any given time step an event would have an 80% chance of firing.
