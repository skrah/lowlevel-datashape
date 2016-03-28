(* Copyright (c) 2016 Stefan Krah.  BSD 2-Clause License *)

open Printf

exception CompileError


let print_err pos msg =
  let open Lexing in
    fprintf stderr "%s:%d:%d: %s\n"
      pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg;
    flush stderr;
    raise CompileError

let lexbuf_from_string s =
  let open Lexing in
  let buf = from_string s in
    buf.lex_start_p <- { buf.lex_start_p with pos_fname = "<string>" };
    buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = "<string>" };
    buf

let lexbuf_from_file filename ic =
  let open Lexing in
  let buf = from_channel ic in
    buf.lex_start_p <- { buf.lex_start_p with pos_fname = filename };
    buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename };
    buf

let ast_from_string s =
  let open Lexing in
  let buf = lexbuf_from_string s in
    try
      Parser.input Lexer.token buf
    with Lexer.LexingError msg -> print_err buf.lex_curr_p msg
       | Ast.ParseError msg -> print_err buf.lex_curr_p msg

let ast_from_file filename =
  let open Lexing in
  let ic = open_in filename in
  let buf = lexbuf_from_file filename ic in
    try
      let ast = Parser.input Lexer.token buf in
      close_in ic; ast
    with Lexer.LexingError msg -> close_in ic; print_err buf.lex_curr_p msg
       | Ast.ParseError msg -> close_in ic; print_err buf.lex_curr_p msg




