%{
(* Copyright (c) 2016 Stefan Krah.  BSD 2-Clause License *)
module A = Ast
%}
/* keywords */
%token VOID
%token BOOL
%token INT8
%token INT16
%token INT32
%token INT64
%token INT128
%token UINT8
%token UINT16
%token UINT32
%token UINT64
%token UINT128
%token FLOAT16
%token FLOAT32
%token FLOAT64
%token FLOAT128
%token COMPLEX64
%token COMPLEX128
%token INTPTR
%token UINTPTR
%token SIZE
%token REAL
%token COMPLEX
%token INT
%token CHAR
%token STRING
%token FIXED_STRING
%token BYTES
%token FIXED_BYTES
%token POINTER
%token OPTION
%token CUDA_HOST
%token CUDA_DEVICE
%token FIXED
%token VAR
%token ALIGN
%token ANY_KIND
%token SCALAR_KIND
%token CATEGORICAL_KIND
%token FIXED_BYTES_KIND
%token FIXED_STRING_KIND
%token FIXED_DIM_KIND

/* punctuation */
%token COMMA
%token COLON
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token STAR
%token DOUBLESTAR
%token ELLIPSIS
%token RARROW
%token EQUAL
%token QUESTIONMARK

/* values */
%token <int>INTEGER
%token <string>NAME_LOWER
%token <string>NAME_UPPER
%token <string>NAME_OTHER
%token <string>STRINGLIT

/* eof */
%token EOF

%start input
%type <Ast.datashape> input

%%

input:
  datashape EOF { $1 }

/* types */
datashape:
  datashape_nooption                      { $1 }
| QUESTIONMARK datashape_nooption         { A.Option($2) }
| OPTION LBRACK datashape_nooption RBRACK { A.Option($3) }

datashape_nooption:
  /* dimension types */
  INTEGER STAR datashape                     { A.FixedDim($1, $3) }
| FIXED LBRACK INTEGER RBRACK STAR datashape { A.FixedDim($3, $6) }
| FIXED_DIM_KIND STAR datashape              { A.FixedDimKind($3) }
| VAR STAR datashape                         { A.VarDim($3) }
| NAME_UPPER STAR datashape                  { A.SymbolicDim($1, $3) }
| ELLIPSIS STAR datashape                    { A.EllipsisDim("", $3) }
| NAME_UPPER ELLIPSIS STAR datashape         { A.EllipsisDim($1, $4) }

  /* power dimension syntax sugar */
| INTEGER DOUBLESTAR INTEGER STAR datashape    { A.mk_fixed_power_dim ~size:$1 ~exponent:$3 ~datashape:$5 }
| VAR DOUBLESTAR INTEGER STAR datashape        { A.mk_var_power_dim ~exponent:$3 ~datashape:$5 }
| NAME_UPPER DOUBLESTAR INTEGER STAR datashape { A.mk_symbolic_power_dim ~symbol:$1 ~exponent:$3 ~datashape:$5 }

| ANY_KIND { A.AnyKind }

| dtype { $1 }

dtype:
  /***** Scalars *****/

  /*** fixed-size ***/
  VOID { A.Void }
| BOOL { A.Bool }

| INT8  { A.Int8 }
| INT16 { A.Int16 }
| INT32 { A.Int32 }
| INT64 { A.Int64 }
| INT128 { A.Int128 }

| UINT8  { A.Uint8 }
| UINT16 { A.Uint16 }
| UINT32 { A.Uint32 }
| UINT64 { A.Uint64 }
| UINT128 { A.Uint128 }

  /* binary floating point (IEEE 754-2008) */
| FLOAT16 { A.Float16 }
| FLOAT32 { A.Float32 }
| FLOAT64 { A.Float64 }
| FLOAT128 { A.Float128 }

  /* complex numbers (IEEE 754-2008) */
| COMPLEX64 { A.Complex(A.Float32) }
| COMPLEX128 { A.Complex(A.Float64) }

  /*** aliases ***/
  /* machine independent */
| INT { A.Int32 }
| REAL { A.Float64 }
| COMPLEX { A.Complex(A.Float64) }

  /*** machine dependent ***/
| INTPTR { A.translate_alias "intptr_t" }
| UINTPTR { A.translate_alias "uintptr_t" }
| SIZE { A.translate_alias "size_t" }

  /*** complex constructor (scalars internally) ***/
  /* complex[float32] */
| COMPLEX LBRACK FLOAT32 RBRACK { A.Complex(A.Float32) }
  /* complex[float64] */
| COMPLEX LBRACK FLOAT64 RBRACK { A.Complex(A.Float64) }
  /* complex[real] */
| COMPLEX LBRACK REAL RBRACK { A.Complex(A.Float64) }

  /***** Chars, strings, bytes *****/
  /* char[encoding] */
| CHAR LBRACK STRINGLIT RBRACK { A.Char(A.encoding_of_string $3) }

  /* alias: unicode character (utf32) */
| CHAR { A.Char(A.Utf32) }

  /* unicode string (utf8) */
| STRING { A.String }

  /* fixed_string[size] */
| FIXED_STRING LBRACK INTEGER RBRACK { A.FixedString($3, None) }
  /* fixed_string[size, encoding] */
| FIXED_STRING LBRACK INTEGER COMMA STRINGLIT RBRACK { A.FixedString($3, Some (A.encoding_of_string $5)) }

  /* bytes[alignment] */
| BYTES LBRACK ALIGN EQUAL INTEGER RBRACK { A.mk_bytes $5 }

  /* fixed_bytes[size, alignment] */
| FIXED_BYTES LBRACK INTEGER COMMA ALIGN EQUAL INTEGER RBRACK { A.mk_fixed_bytes ~size:$3 ~align:$7 }

  /* pointer[datashape] */
| POINTER LBRACK datashape RBRACK { A.Pointer($3) }

  /* cuda_host[datashape] */
| CUDA_HOST LBRACK datashape RBRACK { A.CudaHost($3) }

  /* cuda_device[datashape] */
| CUDA_DEVICE LBRACK datashape RBRACK { A.CudaDevice($3) }

 /* dtype variable */
| NAME_UPPER { A.Dtypevar $1 }

 /* dtype kinds */
| SCALAR_KIND { A.ScalarKind }
| CATEGORICAL_KIND { A.CategoricalKind }
| FIXED_BYTES_KIND { A.FixedBytesKind }
| FIXED_STRING_KIND { A.FixedStringKind }

| NAME_UPPER LBRACK datashape RBRACK { A.Constr ($1, $3) }

| tuple_type    { $1 }
| struct_type   { $1 }
| function_type { $1 }

variadic_flag:
  /* empty */    { A.Nonvariadic }
| ELLIPSIS { A.Variadic }

comma_variadic_flag:
  /* empty */    { A.Nonvariadic }
| COMMA          { A.Nonvariadic }
| COMMA ELLIPSIS { A.Variadic }

tuple_type:
  LPAREN variadic_flag RPAREN                       { A.Tuple ($2, []) }
| LPAREN tuple_item_list comma_variadic_flag RPAREN { A.Tuple ($3, List.rev $2) }

tuple_item_list:
  datashape                       { [$1] }
| tuple_item_list COMMA datashape { $3 :: $1 }

struct_type:
  LBRACE variadic_flag RBRACE                         { A.Record ($2, []) } 
| LBRACE struct_field_list comma_variadic_flag RBRACE { A.Record ($3, List.rev $2) }

struct_field_list:
  struct_field                          { [$1] }
| struct_field_list COMMA struct_field  { $3 :: $1 }

struct_field:
  struct_field_name COLON datashape { ($1, $3) }

struct_field_name:
  NAME_LOWER { $1 }
| NAME_UPPER { $1 }
| NAME_OTHER { $1 }
| keyword { $1 }

function_type:
  tuple_type RARROW datashape
    { A.mk_function ~pos:$1 ~kwds:(A.Record(A.Nonvariadic, [])) ~ret:$3 }
| LPAREN struct_field_list comma_variadic_flag RPAREN RARROW datashape
    { A.mk_function ~pos:(A.Tuple(A.Nonvariadic, [])) ~kwds:(A.Record($3, List.rev $2)) ~ret:$6 }
| LPAREN tuple_item_list COMMA struct_field_list comma_variadic_flag RPAREN RARROW datashape
    { A.mk_function ~pos:(A.Tuple(A.Nonvariadic, $2)) ~kwds:(A.Record($5, List.rev $4)) ~ret:$8 }
| LPAREN tuple_item_list COMMA ELLIPSIS COMMA struct_field_list comma_variadic_flag RPAREN RARROW datashape
    { A.mk_function ~pos:(A.Tuple(A.Variadic, $2)) ~kwds:(A.Record($7, List.rev $6)) ~ret:$10 }


/* record fields may have keyword names */
keyword:
  VOID { "void" }
| BOOL { "bool" }
| INT8 { "int8" }
| INT16 { "int16" }
| INT32 { "int32" }
| INT64 { "int64" }
| INT128 { "int128" }
| UINT8  { "uint8" }
| UINT16 { "uint16" }
| UINT32 { "uint32" }
| UINT64 { "uint64" }
| UINT128 { "uint128" }
| FLOAT16 { "float16" }
| FLOAT32 { "float32" }
| FLOAT64 { "float64" }
| FLOAT128 { "float128" }
| COMPLEX64 { "complex64" }
| COMPLEX128 { "complex128" }
| INTPTR { "intptr" }
| UINTPTR { "uintptr" }
| SIZE { "size" }
| REAL { "real" }
| COMPLEX { "complex" }
| INT { "int" }
| CHAR { "char" }
| STRING { "string" }
| FIXED_STRING { "fixed_string" }
| BYTES { "bytes" }
| FIXED_BYTES { "fixed_bytes" }
| POINTER { "pointer" }
| OPTION { "option" }
| CUDA_HOST { "cuda_host" }
| CUDA_DEVICE { "cuda_device" }
| FIXED { "fixed" }
| VAR { "var" }
| ALIGN { "align" }
| ANY_KIND { "Any" }
| SCALAR_KIND { "Scalar" }
| CATEGORICAL_KIND { "Categorical" }
| FIXED_BYTES_KIND { "FixedBytes" }
| FIXED_STRING_KIND { "FixedString" }
| FIXED_DIM_KIND { "Fixed" }



