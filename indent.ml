(* Copyright (c) 2016 Stefan Krah.  BSD 2-Clause License *)

open Printf

let source = ref None
let usage = "indent: usage: ./indent file"
let specs = []

let _ = Arg.parse specs (fun name -> source := Some name) usage

let main () =
    match !source with
    | None -> fprintf stderr "%s" (Arg.usage_string specs usage); exit(1)
    | Some filename ->
      let ast = Lib.ast_from_file filename in
        Visualize.AstPrinter.print ast

let () = main ()



