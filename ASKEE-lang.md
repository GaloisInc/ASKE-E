# ASKEE

ASKEE is a modeling language, able to represent compartmental and other
scientific models in a concise, human- and machine-readable way. It builds off
Galois's experience with the AMIDOL IR, and is intended to supplant it in the
long term. It, like the AMIDOL IR, is meant to represent stochastic Petri nets
with a form of inhibitor arcs.

What follows is an explanation of this language's format and features by
example, the example being an adapted, subset version of the CHIME
epidemiological model from UPenn.


## `model`s

A `model` is a named entity (`CHIME` in the below example), followed by a set of
declarations of state and other variables, followed by a set of declarations of
events that may depend on or modify these variables, modulo some rules.

`model`s are constructed blockwise in a whitespace-sensitive way that should be
familiar to users of YAML and other similar languages.

### `let`-Declared Variables
```
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

### `state`-Declared Variables
```
  state Susceptible = s_Initial
  state Infected    = i_Initial
  state Recovered   = r_Initial
```

Variables declared with `state` differ internally from `let`-declared variables
in that they may be modified in an `event`'s `effect` block. These variables may
be used to represent what the `model` is actually modeling - that is, they may
be the output of an execution/simulation of a model (though they needn't be).


### `event`s
```
  event Infect:
  ...
```

An `event` consists of an enabling predicate (a `when` block), a `rate` (how
likely this event is to "fire"), and an `effect`, which describes what happens
when the event fires (an "output predicate", in some previous Galois
literature).

#### `when`
```
    when:
      Susceptible > 0.0 and Infected > 0.0
```

An expression defined on `let` variables and `state` variables, evaluating to a
Boolean, which determines whether or not an event is able to fire. An event must
have a true `when` _and_ be chosen stochastically based on its rate in order to
fire.

#### `rate`
```
    rate: 
      beta * (1 - contact_rate) * Susceptible * Infected / total_pop
```

An expression defined on `let` variables and `state` variables, evaluating to a
real, which represents the frequency with which an event fires. In simulation,
events would be chosen stochastically based on this value - a rate of `0.8`
means at any given time step an event would have an 80% chance of firing.


#### `effect`
```
    effect:
      Susceptible -= 1.0
      Infected    = Infected + 1.0
```

What happens when the model fires. This is a series of assignments, executed in
order, that may change the value of some or all of the `state` variables in the
`model`.


## Example
```
model CHIME_GTRI_IR:
  let  s_Initial = 990.0
  let  i_Initial = 10.0
  let  r_Initial = 0.0

  state Susceptible = s_Initial
  state Infected    = i_Initial
  state Recovered   = r_Initial

  let total_pop = Susceptible + Infected + Recovered
  
  let gamma = 1.0 / 14.0
  let policy_days0 = 37.0
  let policy_days1 = 77.0
  let contact_rate = 0.05
  
  let beta =
    cond:
      gamma              if time <= policy_days0
      gamma + 0.1486983  if time <= policy_days1
      gamma + 0.0717734  otherwise


  event Infect:
    when:
      Susceptible > 0.0 and Infected > 0.0

    rate: 
      beta * (1 - contact_rate) * Susceptible * Infected / total_pop

    effect:
      Susceptible -= 1.0
      Infected    = Infected + 1.0

  event Cure:
    when:
      Infected > 0.0

    rate:
      gamma * Infected

    effect:
      Infected  = Infected - 1.0
      Recovered = Recovered + 1.0

```
