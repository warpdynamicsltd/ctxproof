# Writing Proofs in CtxProof

This guide explains how to write proofs for the CtxProof kernel. This document focuses on the kernel-level proof format.

## Overview

- A proof file states a top-level goal formula at reference “.” (the root), followed by a block of statements that justify it.
- Statements may open nested blocks to prove new formulas (contexts).
- Each statement has:
  - a structured reference (like 0, 0.1, 1.0.3),
  - a formula,
  - an inference annotation (assumption, axiom, or rule),
  - optionally a nested context block.

The kernel enforces correctness: reference discipline, rule/axiom schemas, Skolem/generalization constraints, and proper scoping.

## File Skeleton

```
. GoalFormula
{
  0 SubgoalFormula {ASM} {.};
  1 AnotherFormula {R:MOD} {0; 0.0};
}
```

- `.` is the root reference (the main goal).
- Inside `{ ... }`, write statements that collectively justify the goal.

## References

References indicate where a statement lives in the proof tree and what it may depend on.

- Root: `.`
- Child indices: `0`, `1`, `2`, …
- Nested children: `0.0`, `0.1`, `1.0.3`, …

Examples:
- `.`          root
- `0`          first child under root
- `0.2`        third child of `0`
- `1.0.3`      fourth child of `1.0`

## Formulas

- Variables: uppercase identifiers (e.g., `X`, `Y`, `N`)
- Predicates/functions/constants: lowercase (e.g., `p`, `q`, `a(X)`, `f(X,Y)`, `c`)
- Built-ins: `$true`, `$false`
- Connectives:
  - `~ φ`        negation
  - `φ & ψ`      conjunction
  - `φ | ψ`      disjunction
  - `φ => ψ`     implication
  - `φ <=> ψ`    equivalence
- Quantifiers:
  - `![X]: φ`    for all X, φ
  - `?[X]: φ`    there exists X, φ
  - Multiple variables: `![X,Y]: φ`, `?[X,Y]: φ`
- Parentheses for grouping: `( φ )`

Examples:
- `p(X)`
- `a(X) => b`
- `![X]: (a(X) => b)`
- `(p & q) => r`
- `~(p | q) <=> (~p & ~q)`

## Statements

A statement either ends with a semicolon or opens a nested block.

- Inference (axiom/rule) with dependencies and optional terms:
  ```
  ref formula {MODE} {dependencies} {terms};
  ref formula {MODE} {dependencies};
  ```

- Context block:
  ```
  ref a => b {
    ... inner statements ...
  }
  ```

Components:
- `ref`: a reference like `.`, `0`, `0.1`
- `formula`, `a`, `b`: any well-formed formulas
- `{MODE}`:
  - `{ASM}` — assumption
  - `{A:NAME}` — apply axiom NAME
  - `{R:NAME}` — apply rule NAME
- `{dependencies}`: a brace-enclosed list (semicolon-separated) of generalized formulas:
  - References: `{0}` or `{0; 1.2}`
  - Inline formulas: `{ p(X) => q }`

- `{terms}`: a brace-enclosed list of terms (when required by an axiom/rule)

Each statement line ends with a semicolon `;`. Inner statements in a block also end with `;`.

## Generalized Formulas (Dependencies)

Dependencies can be:
- References to previously stated lines: `0`, `0.1`, `1.2`, …
- Inline formulas written directly: `p(X)`, `![X]: p(X)`, …

Examples:
- `{0}`
- `{0; 1.2}`
- `{ p(X) => q }`
- `{ a(X) => b; 0.4 }`

You can mix references and inline formulas in a single dependency list.

## Terms and Skolem Terms

Terms:
- Variables: `X`
- Constants: `c`
- Functions: `f(t1, …)`
- Skolem terms:
  - Skolem constant: `sk.<ref>` (e.g., `sk.0.2`)
  - Skolem function: `sk.<ref>(t1,…)` (e.g., `sk.1.0(X, f(Y))`)

The `<ref>` in a Skolem term is a dot-separated numeric path corresponding to a proof reference. The kernel checks Skolem compatibility with the current context and enforces scoping constraints.

## Modes (Axioms and Rules)

- Assumption:
  - `{ASM}` — recalls an assumption.

- Axioms `{A:NAME}` — common names include:
  - `LEM`, `IMP`, `AND`, `ANL`, `ANR`, `ORL`, `ORR`, `DIS`, `CON`, `IFI`, `IFO`, `ALL`, `EXT`

- Rules `{R:NAME}` — common rules:
  - `MOD` — Modus Ponens
  - `GEN` — Generalization
  - `SKO` — Skolemization
  - `IDN` — Identity

The kernel enforces the schema and side conditions for each axiom/rule (e.g., allowed references, freshness, variable conditions).


### Axioms `{A:NAME}`

Usage pattern:
```
ref Formula {A:NAME} { generalized_formulas } { terms };
```

- `generalized_formulas` are references or inline formulas inside `{ ... }`, separated by semicolons.
- `terms` is a list of terms separated by commas (terms bracket can be skiped if a list of terms is empty).

A:LEM — Law of Excluded Middle
- From `a`, conclude `a | ~a`
- Usage:
  ```
  r (a | ~a) {A:LEM} {a};
  ```

A:IMP
- From `a` and `b`, conclude `a => (b => a)`
- Usage:
  ```
  r (a => (b => a)) {A:IMP} {a; b};
  ```

A:ANL — And-Left
- From `a` and `b`, conclude `(a & b) => a`
- Usage:
  ```
  r ((a & b) => a) {A:ANL} {a; b};
  ```

A:ANR — And-Right
- From `a` and `b`, conclude `(a & b) => b`
- Usage:
  ```
  r ((a & b) => b) {A:ANR} {a; b};
  ```

A:AND — And-Introduction (curried)
- From `a` and `b`, conclude `a => (b => (a & b))`
- Usage:
  ```
  r (a => (b => (a & b))) {A:AND} {a; b};
  ```

A:ORL — Or-Left
- From `a` and `b`, conclude `a => (a | b)`
- Usage:
  ```
  r (a => (a | b)) {A:ORL} {a; b};
  ```

A:ORR — Or-Right
- From `a` and `b`, conclude `b => (a | b)`
- Usage:
  ```
  r (b => (a | b)) {A:ORR} {a; b};
  ```

A:DIS — Or-Elimination (curried)
- From `a`, `b`, `c`, conclude `(a => c) => ((b => c) => ((a | b) => c))`
- Usage:
  ```
  r ((a => c) => ((b => c) => ((a | b) => c))) {A:DIS} {a; b; c};
  ```

A:CON — Contradiction pattern
- From `a` and `b`, conclude `~a => (a => b)`
- Usage:
  ```
  r (~a => (a => b)) {A:CON} {a; b};
  ```

A:IFI — Iff-Introduction
- From `a` and `b`, conclude `(a => b) => ((b => a) => (a <=> b))`
- Usage:
  ```
  r ((a => b) => ((b => a) => (a <=> b))) {A:IFI} {a; b};
  ```

A:IFO — Iff-Elimination
- From `a` and `b`, conclude `(a <=> b) => ((a => b) & (b => a))`
- Usage:
  ```
  r ((a <=> b) => ((a => b) & (b => a))) {A:IFO} {a; b};
  ```

A:ALL — Universal Instantiation
- From a schema `a(X)` and a term `t`, conclude `(![X]: a(X)) => a(t)`
- Usage:
  ```
  r ( (![X]: a(X)) => a(t) ) {A:ALL} {a(X)} {t, X};
  ```

A:EXT — Existential Introduction
- From a schema `a(X)` and a term `t`, conclude `a(t) => (?[X]: a(X))`
- Usage:
  ```
  r ( a(t) => (?[X]: a(X)) ) {A:EXT} {a(X)} {t, X};
  ```


### Rules `{R:NAME}`

Usage pattern:
```
r formula {R:NAME} { refs } { terms };
```
- `refs` are references of formulas, separated by semicolons.
- `terms` is a list of terms separated by commas (terms bracket can be skiped if a list of terms is empty). 

R:IDN — Identity
- From `a`, conclude `a`
- Usage:
  ```
  r a {R:IDN} {<ref of a>};
  ```

R:MOD — Modus Ponens
- From `(a => b)` and `a`, conclude `b`
- Usage:
  ```
  r b {R:MOD} {<ref of a => b>; <ref of a>};
  ```

R:GEN — Generalization
- From `a`, conclude `![X]: a`
- Constraint: `X` must not depend on open assumptions in the current context.
- Usage:
  ```
  r (![X]: a) {R:GEN} {<ref of a>} {X};
  ```

R:SKO — Skolemization
- From `?[X]: a(X)`, introduce a Skolem term and conclude `a(sk)`
- Constraints:
  - The Skolem’s anchor `<ref>` should match the current statement’s reference.
  - Variables in the Skolem term must fit the context (no hidden dependencies on open assumptions).
- Usage with a Skolem constant:
  ```
  ref a(sk.<ref>) {R:SKO} { <ref of ?[X]: a(X)> } { sk.<ref>, X };
  ```
- Usage with a Skolem function:
  ```
  ref a(sk.<ref>(t1,...)) {R:SKO} { <ref of ?[X]: a(X)> } { sk.<ref>(t1,...), X };
  ```

### Assumption {ASM}

Usage pattern:
```
r formula {ASM} { ref };
```

- `ref` is a reference of a context block.

Recalls an assumption `formula` from the current context block 
```
ref formula => a {
  ... inner statements ...
}
```


## Examples

### Example A: Identity p => p

```
. p => p
{
  0 p {ASM} {.};
}
```

Explanation:
- The goal is `p => p`.
- Inside the context for the implication, assume `p`. That immediately provides the consequent and closes the context.

### Example B: ( ?[X]: (![Y]: p(X, Y)) ) => ( ![Y]: (?[X]: p(X, Y)) )

```
. ( ?[X]: (![Y]: p(X, Y)) ) => ( ![Y]: (?[X]: p(X, Y)) )
{
    0 ?[X]: (![Y]: p(X, Y)) {ASM} {.};
    1 ![Y]: p(sk.1, Y) {R:SKO} {0} {sk.1, X};
    2 ( ![Y]: p(sk.1, Y) ) => p(sk.1, Y) {A:ALL} {p(sk.1, Y)} {Y, Y};
    3 p(sk.1, Y) {R:MOD} {2; 1};
    4 p(sk.1, Y) => ( ?[X]: p(X, Y) ) {A:EXT} {p(X, Y)} {sk.1, X};
    5 ?[X]: p(X, Y) {R:MOD} {4; 3};
    6 ![Y]: (?[X]: p(X, Y)) {R:GEN} {5} {Y};
}
```
Explanation:

- Goal
  - Prove: `(?[X]: (![Y]: p(X, Y))) => ( ![Y]: ( ?[X]: p(X, Y) ))`.
  - Strategy: assume the antecedent, derive the consequent inside the block, then close the implication.

- Assumption and context
  - Introduce the assumption `?[X]: (![Y]: p(X, Y))` at the start of the inner block.

- Skolemization `{R:SKO}`
  - From `?[X]: a(X)` with `a(X) := ![Y]: p(X, Y)`, introduce a Skolem constant `sk.<ref>` for `X`.
  - Conclude: `![Y]: p(sk.<ref>, Y)`.

- Universal instantiation `{A:ALL}`
  - From `![Y]: p(sk.<ref>, Y)` and variable `Y`, instantiate to get:
  - `(![Y]: p(sk.<ref>, Y)) => p(sk.<ref>, Y)`.

- Modus Ponens `{R:MOD}`
  - Apply `{R:MOD}` with the two lines above to conclude:
  - `p(sk.<ref>, Y)`.

- Existential introduction `{A:EXT}`
  - From `p(sk.<ref>, Y)` and variable `X`, introduce an existential:
  - `p(sk.<ref>, Y) => ( ?[X]: p(X, Y) )`.

- Modus Ponens `{R:MOD}`
  - Apply `{R:MOD}` again to obtain:
  - `?[X]: p(X, Y)`.

- Generalization `{R:GEN}`
  - Generalize over `Y` to conclude:
  - `![Y]: ( ?[X]: p(X, Y) )`.

- Discharging the implication
  - The inner block starts with the assumption `?[X]: (![Y]: p(X, Y))` and ends with `![Y]: ( ?[X]: p(X, Y) )`.
  - Therefore, the outer line concludes the implication:
  - `(?[X]: (![Y]: p(X, Y))) => ( ![Y]: ( ?[X]: p(X, Y) ))`.


