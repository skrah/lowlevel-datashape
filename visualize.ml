(* Copyright (c) 2016 Stefan Krah.  BSD 2-Clause License *)

open Printf

exception InternalError of string


(*** Convert AST back to re-indented source ***)
module AstPrinter =
struct
open Ast

let indent d ch () = fprintf ch "%*s" d ""

let variadic_flag flag lst =
  match flag, lst with
    Nonvariadic, _ -> ""
  | Variadic, [] -> "..."
  | Variadic, _ -> ", ..."

let rec list_single_comma (d : int) (ch : out_channel) f = function
    [a] -> f d ch a
  | a :: r -> f d ch a; fprintf ch  ", "; list_single_comma d ch f r
  | [] -> ()

let rec list_single_push_comma (d : int) (ch : out_channel) f = function
  | a :: r -> fprintf ch  ", "; f d ch a; list_single_push_comma d ch f r
  | [] -> ()

let rec list_multi_comma d ch f = function
    [a]    -> f d ch a
  | a :: r -> f d ch a; fprintf ch ",\n%a" (indent d) (); list_multi_comma d ch f r
  | []     -> ()

let rec datashape d ch t =
  match t with

    Void -> fprintf ch "void"
  | Bool -> fprintf ch "bool"

  | Int8 -> fprintf ch "int8"
  | Int16 -> fprintf ch "int16"
  | Int32 -> fprintf ch "int32"
  | Int64 -> fprintf ch "int64"
  | Int128 -> fprintf ch "int128"

  | Uint8 -> fprintf ch "Uint8"
  | Uint16 -> fprintf ch "Uint16"
  | Uint32 -> fprintf ch "Uint32"
  | Uint64 -> fprintf ch "Uint64"
  | Uint128 -> fprintf ch "Uint128"

  | Float16 -> fprintf ch "float16"
  | Float32 -> fprintf ch "float32"
  | Float64 -> fprintf ch "float64"
  | Float128 -> fprintf ch "float128"

  | Complex t -> fprintf ch "complex[%a]" (datashape d) t

  | Char enc -> fprintf ch "char[%s]" (string_of_encoding enc)

  | String -> fprintf ch "string"
  | FixedString (size, encoding) ->
     begin match encoding with
       None -> fprintf ch "fixed_string[%d]" size
     | Some enc -> fprintf ch "fixed_string[%d, %s]" size (string_of_encoding enc)
     end

  | Bytes align -> fprintf ch "bytes[align=%d]" align
  | FixedBytes (size, align) -> fprintf ch "fixed_bytes[%d, align=%d]" size align

  | Tuple (flag, lst) ->
     let lst' = match flag with
                  Nonvariadic -> lst
                | Variadic -> lst @ [Dtypevar "..."] (* Abuse Dtypevar for printing *)
     in fprintf ch "(%a)" (datashape_list d) lst'

  | Record (flag, lst) ->
     let lst' = match flag with
                  Nonvariadic -> lst
                | Variadic -> lst @ [("", Dtypevar "...")] (* Abuse Dtypevar for printing *)
     in fprintf ch "{\n%a%a\n%a}"
          (indent (d+2)) ()
          (field_declaration_list (d+2)) lst'
          (indent d) ()

  | Function { fun_ret; fun_pos=Tuple (tflag, tlst); fun_kwds=Record (rflag, rlst) } ->
      fprintf ch "(%a%s%a%s) -> %a"
        (datashape_list d) tlst
        (variadic_flag tflag tlst)
        (field_declaration_list_tail d) rlst
        (variadic_flag rflag rlst)
        (datashape d) fun_ret

  | Function _ -> raise (InternalError "unexpected function")

  | Dtypevar s -> fprintf ch "%s" s

  | Pointer t -> fprintf ch "pointer[%a]" (datashape d) t
  | Option t -> fprintf ch "?%a" (datashape d) t

  | CudaHost t -> fprintf ch "cuda_host[%a]" (datashape d) t
  | CudaDevice t -> fprintf ch "cuda_host[%a]" (datashape d) t

  (* dtype kinds *)
  | ScalarKind -> fprintf ch "Scalar"
  | CategoricalKind -> fprintf ch "Categorical"
  | FixedBytesKind -> fprintf ch "FixedBytes"
  | FixedStringKind -> fprintf ch "FixedString"

  (* general type constructor *)
  | Constr (sym, t) -> fprintf ch "%s[%a]" sym (datashape d) t

  (* arrays *)
  | FixedDim (size, t) -> fprintf ch "%d * %a" size (datashape d) t
  | FixedDimKind t -> fprintf ch "Fixed * %a" (datashape d) t
  | VarDim t -> fprintf ch "var * %a" (datashape d) t
  | SymbolicDim (sym, t) -> fprintf ch "%s * %a" sym (datashape d) t
  | EllipsisDim (sym, t) -> fprintf ch "%s... * %a" sym (datashape d) t

  (* type kinds *)
  | AnyKind -> fprintf ch "Any"

and datashape_list d ch lst = list_single_comma d ch datashape lst
and field_declaration_list d ch lst = list_multi_comma d ch field_declaration lst
and field_declaration_list_tail d ch lst = list_single_push_comma d ch field_declaration lst
and field_declaration d ch (s, t) =
      match s with
        "" ->  fprintf ch "%a" (datashape d) t (* for printing the variadic ellipsis *)
      | _ ->  fprintf ch "%s: %a" s (datashape d) t

let print t =
  datashape 0 stdout t;
  fprintf stdout "\n";

end


