#` event`s

```askee
  event Infect:
  ...
```

An `event` consists of an enabling predicate (a `when` block), a `rate` (how
likely this event is to "fire"), and an `effect`, which describes what happens
when the event fires (an "output predicate", in some previous Galois
literature).
