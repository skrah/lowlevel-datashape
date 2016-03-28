#!/bin/sh

# DEBUGGING: Dump the abstract syntax tree to stdout.

if [ X"$@" = X"" ]; then
   echo "./usage: ./dump_ast file"
   exit 1
fi


OBJS="ast.cmo parser.cmo lexer.cmo lib.cmo"

cmd="ocaml -noinit $OBJS"

$cmd << EOF
#print_length 100000;;
open Ast;;
let ast = Lib.ast_from_file "$@";;
EOF


