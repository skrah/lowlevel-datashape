exception CompileError
val print_err : Lexing.position -> string -> 'a
val lexbuf_from_string : string -> Lexing.lexbuf
val lexbuf_from_file : string -> in_channel -> Lexing.lexbuf
val ast_from_string : string -> Ast.datashape
val ast_from_file : string -> Ast.datashape
