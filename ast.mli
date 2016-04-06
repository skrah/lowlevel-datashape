exception ParseError of string
val parse_error : string -> 'a
type size = int
type alignment = int
type variadic_flag = Nonvariadic | Variadic
type encoding = Ascii | Utf8 | Utf16 | Utf32 | Ucs2
type datashape =
    Void
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
  | Complex of datashape
  | Char of encoding
  | String
  | FixedString of size * encoding option
  | Bytes of alignment
  | FixedBytes of size * alignment
  | Pointer of datashape
  | Option of datashape
  | CudaHost of datashape
  | CudaDevice of datashape
  | Constr of string * datashape
  | Dtypevar of string
  | ScalarKind
  | CategoricalKind
  | FixedBytesKind
  | FixedStringKind
  | Tuple of variadic_flag * datashape list
  | Record of variadic_flag * field list
  | Function of parameters
  | FixedDim of size * datashape
  | VarDim of datashape
  | SymbolicDim of string * datashape
  | EllipsisDim of string * datashape
  | FixedDimKind of datashape
  | AnyKind
and field = string * datashape
and parameters = {
  fun_ret : datashape;
  fun_pos : datashape;
  fun_kwds : datashape;
}
val translate_alias : string -> datashape
val encoding_of_string : string -> encoding
val string_of_encoding : encoding -> string
val mk_fixed_power_dim :
  size:size -> exponent:int -> datashape:datashape -> datashape
val mk_var_power_dim : exponent:int -> datashape:datashape -> datashape
val mk_symbolic_power_dim :
  symbol:string -> exponent:int -> datashape:datashape -> datashape
val mk_bytes : alignment -> datashape
val mk_fixed_bytes : size:size -> align:size -> datashape
val mk_function :
  pos:datashape -> kwds:datashape -> ret:datashape -> datashape
