exception InternalError of string
module AstPrinter :
  sig
    val print : Ast.datashape -> unit
  end
