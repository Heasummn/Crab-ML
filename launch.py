#!/usr/bin/env python3

import argparse
from subprocess import call
import sys
import os.path

parser = argparse.ArgumentParser(
    description="Build tool for Crab, a small functional Programming language")

parser.add_argument('--build', '-b',
                    help="Build the Crab Compiler",
                    action="store_true"
                    )

parser.add_argument('--clean',
                    help="""Clean the generated files.
                    Run if a strange error occurs in building""",
                    action="store_true"
                    )

parser.add_argument('--test', '-t',
                    help="Run tests for Crab Compiler",
                    action="store_true")

parser.add_argument('filename',
                    nargs="?",
                    help="Run Crab program specified")

args = parser.parse_args()


if len(sys.argv) == 1:
    parser.print_help()
    sys.exit(1)

call(["clear"])

if args.clean:
    call(["make", "clean"])
    call(["make", "distclean"])
    print()

if args.build:
    call(['oasis', 'setup'])
    call(["make", "build"])
    call(["mv", "crab.native", "crab"])
    print()

if args.test:
    call(["./configure", "--enable-tests"])
    call(["clear"])
    call(['make', 'test'])
    print()

if args.filename is not None:
    fname = args.filename

    if os.path.isfile(fname):
        # If this doesn't exist, the user hasn't built yet
        try:
            call(["./crab", fname])
        except FileNotFoundError:
            print("Please build before running")
            sys.exit(1)
    else:
        print("Argument " + fname + " is not a file.")
        sys.exit(1)
