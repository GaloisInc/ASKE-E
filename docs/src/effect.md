# `effect`

```easel
    effect:
      Susceptible -= 1.0
      Infected    = Infected + 1.0
```

What happens when the model fires. This is a series of assignments, executed in
order, that may change the value of some or all of the `state` variables in the
`model`.
