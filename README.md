# clp-intro

## Constraint Logic Programming

Backward execution:
- regular functional:  ```(cons 1 '(2 3))  =>  '(1 2 3)```
- CLP:                 ```(conso 1 _ '(1 2 3))   => _ unifies with '(2 3)```

## Examples

- Peano number addition:
      - ***see code...***
- List/seq append / concat
      - ***see code...***

## How To Use

Unlikely "in isolation"; more likely embedded within regular Clojure code.
Therefore:

- How to combine / compose with regular functions?
     - calling CLP functions from within regular functions
     - calling regular functions from within CLP functions
- Also: how to compose CLP functions themselves?

## My View...

- A mental image / analogy?:

  Monads
  - using core.logic forms gets you into a 'Monad': once in you stay in
  - you can introduce values into lvar through unification (Monad type constructor)
  - ONLY the "top level" construct 'run* gets you out

  Quanta
  - lvars are "quantum objects": they a the superposition of states (all possible values)
    that match the current constraints
  - by "looking at them" (i.e., calling 'run*') they collapse into regular values

## Questions

- Given a function, is it "easy" (*) to derive the CLP-equivalent?
- Given a non-primitive function F based on primitive P, does that imply that
  CLP-F is also non-primitive and can be expressed using a CLP-P?

- How about Integer / Float arithmetic?
    - Integer is *countable infinite*, so at least a lazy approach could work
    - Float is *uncountable infinite*, so no go
    - or are we restricted to types with *GROUNDED VALUES* / algebraic types? (seem that way, no?)


(*) Easy: we can think of a simple recipe to get from a functional to CLP equivalent implementation
