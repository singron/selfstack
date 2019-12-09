# selfstack

selfstack is a procedural macro that produces a stack-like self-referential
data structure with a safe interface. This is safe because layers in the stack
can only reference layers below them, and lower layers outlive higher layers.
This restriction prevents cycles, dangling references, and other unsoundness
that would generally be possible with self-reference.

The tests are also run with miri to check generated unsafe code for compliance
with stacked borrows.

## selfstack vs rental

[rental](https://docs.rs/rental) is a similar approach to a similar problem,
but it has a few differences.

* Rental requires all fields except the last to implement `StableDeref`.
  Typically this means you must `Box` all but the last element. However, this
  allows the rental to be moved. selfstack does not require fields to implement
  any trait, but the store struct cannot be moved once it is initialized
  (although the substruct used to access the fields can be moved).
* Rental must be fully constructed in 1 call to `new` or `try_new`. selfstack
  incrementally builds each field in successive calls. The selfstack can be
  safely used at any level of initialization. You can emulate this feature in
  rental by boxing a rental and making it the first field of another rental.
* Rental has more convenience features like `IntoSuffix`, which derefs the
  rental as the last field and `into_head`, which drops all fields but the
  first and returns it.

## License

Licensed under either of

* Apache License, Version 2.0
* MIT License

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
