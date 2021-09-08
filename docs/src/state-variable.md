# `state`-Declared Variables

```easel
  state Susceptible = s_Initial
  state Infected    = i_Initial
  state Recovered   = r_Initial
```

Variables declared with `state` differ internally from `let`-declared variables
in that they may be modified in an `event`'s `effect` block. These variables may
be used to represent what the `model` is actually modeling - that is, they may
be the output of an execution/simulation of a model (though they needn't be).
