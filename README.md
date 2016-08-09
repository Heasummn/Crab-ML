# Crab
A small programming language to experiment with compiler design in OCaml, as well as an educational tool to study Compiler Design.

## Running
To run, you need Ocaml installed, preferrably through opam. Install Menhir, Batteries, and LLVM from opam, by running the command `opam install menhir batteries llvm`. Afterwards, the build system does the rest, it is documented below. Please report any errors with the build system.

## What features does Crab have?
Right now? Nothing really. Crab has an okay build system, if `run` is executable, all you need to do is run `./run build` and then `./run test.cb` to see Crab in action. Crab currently only has simple Arithmetic expressions, and compiles down to LLVM. 

## Future goals
Crab has a long way to go, but I hope to add enough features to make Crab turing complete, and easy to use.

The next thing to work on is more tests and documentation. A book, or something of the sort to document the iterative creation of Crab would add to the goal of being educational. 
