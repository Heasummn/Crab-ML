# Crab
A small programming language to experiment with compiler design in OCaml, as well as an educational tool to study Compiler Design.

## Running
To run, you need Ocaml installed, preferrably through opam. Install Menhir, Batteries, and LLVM from opam, by running the command `opam install oasis menhir batteries llvm`. Afterwards, the build system does the rest, it is documented below. Please report any errors with the build system.

## The Build System
The build system is a very simple Python script. It requires a working installation of Python on your machine. To use it, make sure the script is executable. This can be done by running `chmod +x crab`. Afterwards, run `crab --help` to see the options that you have. It is recommended to add this compiled folder into your PATH, making it possible to run crab from anywhere in your system.

## The Development system
The development system is a similar Python script with some neat features. It also requires a working installation of Python on your machine. The same stuff applies, make sure the script is executable, or deal with writing the annoying `python3` prefix. Run `crab_build --help` to see what it can do. You need oUnit2 to run the tests, which can easily be installed using `opam install ounit`.

## What features does Crab have?
Quite a bit. Crab has operator overloading, and the ability to define and call functions. Crab has a decent type checker that prevents most errors from getting to compile time. To see Crab in action, use the build system and build Crab. Then run `crab -o executable test.cb`. 

## Feature requests
I doubt anyone will want to request a feature, but I'm trying to make Crab official looking. To request a feature, make an issue detailing the feature, and a plan to implement it. If you want to implement that feature yourself, first make the issue, and then make a pull request implementing it. 

## Dealing with strange errors
If you get a strange error, such as an `assertion failure`, please report this. I'm working on making Internal Compiler errors more comfortable to look at. 

## Future goals
Crab has a long way to go, but I hope to add enough features to make Crab turing complete, and easy to use.

The next step is to either create lambdas and currying, or implement types and pattern matching.

Another thing to work on is more tests and documentation. A book, or something of the sort to document the iterative creation of Crab would add to the goal of being educational. 

A GCC backend is also planned for the future.
