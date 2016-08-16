#!/usr/bin/env python3

import argparse

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

parser.add_argument('--run', '-r',
                    metavar="file",
                    help="Run Crab program specified")

args = parser.parse_args()
print(args)
