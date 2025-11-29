# CtxProof

## Overview

CtxProof is a minimal, trustworthy kernel for context-based first-order proofs. Its primary purpose is to be embedded in a vernacular (front‑end) that users write proofs in, while the kernel remains the final arbiter of correctness. The kernel focuses on a small set of core judgments (contexts, references, axioms, and rules) and enforces them strictly; higher-level syntax, tactics, and user-friendly notation are meant to live in the vernacular built on top of this kernel.


## Overview

CtxProof implements a context-based proof system that models the way assumptions and auxiliary objects are introduced, scoped, and closed in mathematical arguments. Instead of a flat, linear list of steps, proofs are organized hierarchically by context, closely reflecting how proofs are commonly written.

Key ideas:
- Hierarchical contexts: open and close contexts as you assume statements or introduce objects.
- Structured references: each statement has a structured reference (like 0.1.2) that determines which earlier statements it may depend on.
- First-order logic core: supports predicates, connectives, quantifiers, and Skolemization with checks that ensure proper scoping.

## Installation

Quick start (Linux, with opam):

1) System dependencies (example for Debian/Ubuntu):
    ```bash
    sudo apt-get update
    sudo apt-get install -y opam libgmp-dev
    ```

2) Install CtxProof:
    ```bash
    ./install.sh
    ```

4) Run tests (mirrors CI):
    ```bash
    cd CtxProof
    dune test
    ```

5) Run the checker:
    ```bash
    dune exec ctxproof < ../examples/trivial
    ```
    You should see the following output:
    ```
    QED
    ```




## Working Example

Trivial tautology p ⇒ p:
```
. p => p 
{
    0 p {ASM} {.} {};
}
```

Explanation:
- The top-level goal is p => p.
- Inside the context, we assume p (ASM), and immediately conclude p, closing the context to finish the implication.

A more involved example with quantifiers and Skolemization:
```
. ( ![X]: (a(X) => b) ) => ( ( ?[X]: a(X) ) => b )
{
    0 (?[X]: a(X)) => b
    {
        0.0 ![X]: a(X) => b {ASM} {.};
        0.1 ?[X]: a(X) {ASM} {0};
        0.2 a(sk.0.2) {R:SKO} {0.1} {sk.0.2, X};
        0.3 ( ![X]: (a(X) => b) ) => ( a(sk.0.2) => b ) {A:ALL} {a(X) => b} {sk.0.2, X};
        0.4 a(sk.0.2) => b {R:MOD} {0.3; 0.0};
        0.5 b {R:MOD} {0.4; 0.2};
    }
}
```

Highlights:
- Contexts are nested to represent assumptions and their scope.
- R:SKO introduces a Skolem term consistent with the current reference.
- A:ALL specializes a universally quantified statement.
- R:MOD applies modus ponens.

## Documentation

For instructions how to write proofs refer to [PROOF.md](PROOF.md).

Moreover, the repository includes a documentation paper describing the theory and the kernel specification:

- spec/kernel.pdf — the documentation paper
- spec/kernel.tex — LaTeX sources

The paper covers:
- Formal definition of the context proof system.
- Soundness and completeness proofs.
- Relationship to traditional proof systems.

## Acknowledgments

CtxProof is developed by Michał Stanisław Wójcik.
