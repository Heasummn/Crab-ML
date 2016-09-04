# Crab
A small programming language to experiment with compiler design in OCaml, as well as an educational tool to study Compiler Design.

## Running
To run, you need Ocaml installed, preferrably through opam. Install Menhir, Batteries, and LLVM from opam, by running the command `opam install oasis menhir batteries llvm`. Afterwards, the build system does the rest, it is documented below. Please report any errors with the build system.

## The Build System
The build system is a very simple Python script. It requires a working installation of Python on your machine. To use it, make sure the script is executable. This can be done by running `chmod +x crab`. Afterwards, run `crab --help` to see the options that you have. It is recommended to add this compiled folder into your PATH, making it possible to run crab from anywhere in your system.

## What features does Crab have?
Quite a bit. Crab has operator overloading, and the ability to define and call functions. Crab has a decent type checker that prevents most errors from getting to compile time. To see Crab in action, use the build system and build Crab. Then run `launch -o executable test.cb`. 

## Future goals
Crab has a long way to go, but I hope to add enough features to make Crab turing complete, and easy to use.

The next step is to either create lambdas and currying, or implement types and pattern matching.

The next thing to work on is more tests and documentation. A book, or something of the sort to document the iterative creation of Crab would add to the goal of being educational. 

A GCC backend is also planned for the future.
