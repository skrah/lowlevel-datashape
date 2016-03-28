(* Copyright (c) 2016 Stefan Krah.  BSD 2-Clause License *)


exception ParseError of string
let parse_error s = raise (ParseError s)


type size = int
type alignment = int

type variadic_flag
  = Nonvariadic
  | Variadic

type encoding
  = Ascii
  | Utf8
  | Utf16
  | Utf32
  | Ucs2

type datashape =
  (* dtypes *)
  | Void
  | Bool

  | Int8
  | Int16
  | Int32
  | Int64
  | Int128

  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Uint128

  | Float16
  | Float32
  | Float64
  | Float128

  | Complex of datashape (* argument restricted to floats *)

  | Char of encoding

  | String (* utf8 string *)
  | FixedString of size * encoding option

  | Bytes of alignment
  | FixedBytes of size * alignment

  | Pointer of datashape
  | Option of datashape

  | CudaHost of datashape
  | CudaDevice of datashape

  | Constr of string * datashape (* general type constructor *)

  (* symbolic dtypes *)
  | Typevar of string

  (* type kinds (denoting specific subsets of types) *)
  | Any
  | Scalar
  | Categorical
  | FixedBytesKind
  | FixedStringKind

  (* compound types *)
  | Tuple of variadic_flag * datashape list
  | Record of variadic_flag * field list
  | Function of parameters

  (* dimension types *)
  | FixedDim of size * datashape      (* equivalent to "array[size] of type" *)
  | VarDim of datashape               (* equivalent to "array of type" *)
  | SymbolicDim of string * datashape (* equivalent to "array[N] of type" *)
  | EllipsisDim of string * datashape (* any number of dimensions (... or Dim...) *)

and field = (string * datashape)

and parameters =
  { fun_ret: datashape;   (* any type *)
    fun_pos: datashape;   (* always a tuple *)
    fun_kwds: datashape } (* always a record *)

(* Type aliases, example for the Linux 64-bit data model *)
let translate_alias = function
  (* machine dependent type aliases *)
    "size" -> Uint64
  | "intptr" -> Int64
  | "uintptr" -> Uint64
  (* machine dependent, used for constructing complex64 and complex128 *)
  | "float" -> Float32
  | "double" -> Float64
  (* machine independent (per datashape definition) *)
  | "int" -> Int32
  | s -> parse_error("invalid type alias: " ^ s)

let encoding_of_string s =
  let len = String.length s in
  if len < 2 || s.[0] <> '\'' || s.[len-1] <> '\'' then
    parse_error("expected single quoted string")
  else match String.sub s 1 (len-2) with
    "A" | "ascii" | "us-ascii" -> Ascii
  | "U8" | "utf8" | "utf-8" -> Utf8
  | "U16" | "utf16" | "utf-16" -> Utf16
  | "U32" | "utf32" | "utf-32" -> Utf32
  | "ucs2" | "ucs-2" | "ucs_2" -> Ucs2
  | _ -> parse_error("invalid encoding: " ^ s)

let string_of_encoding = function
   Ascii -> "'ascii'"
 | Utf8 -> "'utf8'"
 | Utf16 -> "'utf16'"
 | Utf32 -> "'utf32'"
 | Ucs2 -> "'ucs2'"

let mk_fixed_power_dim ~size ~exponent ~datashape =
  let rec mk = function
    | 0 -> datashape
    | n -> FixedDim (size, mk (n-1))
  in if exponent < 0 then parse_error("negative dimension")
     else mk exponent

let mk_var_power_dim ~exponent ~datashape =
  let rec mk = function
    | 0 -> datashape
    | n -> VarDim (mk (n-1))
  in if exponent < 0 then parse_error("negative dimension")
     else mk exponent

let mk_symbolic_power_dim ~symbol ~exponent ~datashape =
  let rec mk = function
    | 0 -> datashape
    | n -> SymbolicDim (symbol, mk (n-1))
  in if exponent < 0 then parse_error("negative dimension")
     else mk exponent

let mk_bytes = function
    1 | 2 | 4 | 8 | 16 as align -> Bytes align
  | _ -> parse_error("alignment must be a power of 2 in [1, 16]")

let mk_fixed_bytes ~size ~align =
  match align with
    1 | 2 | 4 | 8 | 16 ->
     if size >= align && size mod align = 0 then FixedBytes (size, align)
     else parse_error("size must be divisible by align")
  | _ -> parse_error("alignment must be a power of 2 in [1, 16]")

let mk_function ~pos ~kwds ~ret =
  Function { fun_ret = ret; fun_pos = pos; fun_kwds = kwds }




