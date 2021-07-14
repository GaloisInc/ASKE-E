# `parameter`-Declared Variables

```easel
parameter contact_rate = 0.05
parameter contact_multiplier = 1 - contact_rate
```

Variables declared with `parameter` are meant to function as the "inputs" to a
model. They are optionally bound to expressions in a model's specification, as
is the case with both above parameters. These bindings represent parameter
defaults. If a default is not provided, it is incumbent on the user to provide a
parameter value when requesting the simulation of a model. See
[`donu`](./donu.md) for more on simulation requests. Though CHIME does not
include any, a parameter declaration without a default is formed in this way:

```easel
parameter A
```

While a user is required to provide values for parameters without defaults, they
are also allowed to override the defaults specified for any other parameter(s)
they choose, when simulating. This has implications for the interplay between
`contact_rate` and `contact_multiplier`: as the model sits, the default for
`contact_rate` is `0.05` and the default for `contact_multiplier` is `0.95`. If
a user manually overrides `contact_rate` but does not manually override
`contact_multiplier`, their parameterization will nonetheless affect the value
of `contact_multiplier`, as it depends on `contact_rate`.

Note: `parameter` variables _may not_ depend on any of the model's state - that
is, they must be constant throughout a model's simulation. If you want to create
a variable that can depend on state, see [`let`](./let-variable.md).
