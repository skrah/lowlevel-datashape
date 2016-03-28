About
=====

This is an implementation of Datashape++ in OCaml.  Datashape++ is a data
description language for modern array computing.

Datashape originated as part of the Blaze Project and is geared towards
high level languages like Python.  Datashape++ contains a large subset
of Datashape together with some additional extensions. It targets low-level
languages.

Libdyndt is the reference implementation of Datashape++. Libdyndt is part of
the Libdynd suite, the other parts of Libdynd are an array library and an
array function library.

The raison d'etre for this package is to specify Datashape++ in BNF form,
using the traditional YACC syntax.  Datashape++ input is converted into
an abstract syntax tree.  The "indent" program reads input from a file
and pretty-prints the output to stdout.


Build (Debian/Ubuntu example)
=============================

sudo apt-get install ocaml-nox

git clone https://github.com/skrah/lowlevel-datashape.git

cd ocaml-datashape++
make


Indent a file
=============

# pretty-print:
./indent single-line-example.txt

# Dump the abstract syntax tree:
./dump_ast.sh single-line-example.txt


Contact: Stefan Krah <skrah@bytereef.org>



