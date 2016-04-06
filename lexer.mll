(* Copyright (c) 2016 Stefan Krah.  BSD 2-Clause License *)

{
module P = Parser
exception LexingError of string

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keywords = create_hashtable 32 [
  "void", P.VOID;
  "bool", P.BOOL;
  "int8", P.INT8;
  "int16", P.INT16;
  "int32", P.INT32;
  "int64", P.INT64;
  "int128", P.INT128;
  "uint8", P.UINT8;
  "uint16", P.UINT16;
  "uint32", P.UINT32;
  "uint64", P.UINT64;
  "uint128", P.UINT128;
  "float16", P.FLOAT16;
  "float32", P.FLOAT32;
  "float64", P.FLOAT64;
  "float128", P.FLOAT128;
  "complex64", P.COMPLEX64;
  "complex128", P.COMPLEX128;
  "intptr", P.INTPTR;
  "uintptr", P.UINTPTR;
  "size", P.SIZE;
  "real", P.REAL;
  "complex", P.COMPLEX;
  "int", P.INT;
  "char", P.CHAR;
  "string", P.STRING;
  "fixed_string", P.FIXED_STRING;
  "bytes", P.BYTES;
  "fixed_bytes", P.FIXED_BYTES;
  "pointer", P.POINTER;
  "option", P.OPTION;
  "cuda_host", P.CUDA_HOST;
  "cuda_device", P.CUDA_DEVICE;
  "fixed", P.FIXED;
  "var", P.VAR;
  "align", P.ALIGN;
  "Any", P.ANY_KIND;
  "Scalar", P.SCALAR_KIND;
  "Categorical", P.CATEGORICAL_KIND;
  "FixedBytes", P.FIXED_BYTES_KIND;
  "FixedString", P.FIXED_STRING_KIND;
  "Fixed", P.FIXED_DIM_KIND
]

}

let name_lower = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let name_upper = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let name_other = '_' ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let nat = '0' | ['1' - '9']['0' - '9']*

let stringlit = "'" name_lower "'" | "'" name_upper "'"
let comment = '#' [^ '\r' '\n']*

rule token = parse
    ","        { P.COMMA }
  | ":"        { P.COLON }
  | "("        { P.LPAREN }
  | ")"        { P.RPAREN }
  | "{"        { P.LBRACE }
  | "}"        { P.RBRACE }
  | "["        { P.LBRACK }
  | "]"        { P.RBRACK }
  | "*"        { P.STAR }
  | "**"       { P.DOUBLESTAR }
  | "..."      { P.ELLIPSIS }
  | "->"       { P.RARROW }
  | "="        { P.EQUAL }
  | "?"        { P.QUESTIONMARK }
  | stringlit  { let s = Lexing.lexeme lexbuf in P.STRINGLIT s }
  | name_lower { let s = Lexing.lexeme lexbuf in
                 try Hashtbl.find keywords s
                 with Not_found -> (P.NAME_LOWER s) }
  | name_upper { let s = Lexing.lexeme lexbuf in
                 try Hashtbl.find keywords s
                 with Not_found -> (P.NAME_UPPER s) }
  | name_other { let s = Lexing.lexeme lexbuf in (P.NAME_OTHER s) }
  | nat        { let s = Lexing.lexeme lexbuf in (P.INTEGER (int_of_string s)) }

  | comment '\n'     { Lexing.new_line lexbuf; token lexbuf }
  | '\n'             { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\r' '\t']+ { token lexbuf }

  | eof      { P.EOF }

  | _        { let s = Lexing.lexeme lexbuf in
               let err = Printf.sprintf "invalid character: %s" s in
                 raise (LexingError err) }


