/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* A simple parser for C-- */

%{
[@@@ocaml.warning "-w-27-33"]

open Cmm
open Parsecmmaux

let rec make_letdef def body =
  match def with
    [] -> body
  | (id, def) :: rem ->
      unbind_ident id;
      Clet(id, def, make_letdef rem body)

let make_switch n selector caselist =
  let index = Array.make n 0 in
  let casev = Array.of_list caselist in
  let dbg = Debuginfo.none in
  let actv = Array.make (Array.length casev) (Cexit(Cmm.Lbl 0,[],[]), dbg) in
  for i = 0 to Array.length casev - 1 do
    let (posl, e) = casev.(i) in
    List.iter (fun pos -> index.(pos) <- i) posl;
    actv.(i) <- (e, dbg)
  done;
  Cswitch(selector, index, actv, dbg)

let access_array base numelt size =
  match numelt with
    Cconst_int (0, _) -> base
  | Cconst_int (n, _) ->
      let dbg = Debuginfo.none in
      Cop(Cadda, [base; Cconst_int(n * size, dbg)], dbg)
  | _ ->
      let dbg = Debuginfo.none in
      Cop(Cadda, [base;
                  Cop(Clsl, [numelt; Cconst_int(Misc.log2 size, dbg)],
                  dbg)],
          dbg)

%}

%token ABSF
%token ADDA
%token ADDF
%token ADDI
%token ADDV
%token ADDR
%token ALIGN
%token ALLOC
%token AND
%token APPLY
%token ASR
%token ASSIGN
%token BYTE
%token CASE
%token CATCH
%token COLON
%token DATA
%token DIVF
%token DIVI
%token EOF
%token EQA
%token EQF
%token EQI
%token EXIT
%token EXTCALL
%token FLOAT
%token FLOAT32
%token FLOAT64
%token <string> FLOATCONST
%token FLOATOFINT
%token FUNCTION
%token GEA
%token GEF
%token GEI
%token GLOBAL
%token GTA
%token GTF
%token GTI
%token HALF
%token <string> IDENT
%token IF
%token INT
%token INT32
%token <int> INTCONST
%token INTOFFLOAT
%token INTOFVALUE
%token KSTRING
%token LBRACKET
%token LEA
%token LEF
%token LEI
%token LET
%token LETMUT
%token LOAD
%token <Location.t> LOCATION
%token LPAREN
%token LSL
%token LSR
%token LTA
%token LTF
%token LTI
%token MODI
%token MULF
%token MULH
%token MULI
%token NEA
%token NEF
%token NEI
%token NGEF
%token NGTF
%token NLEF
%token NLTF
%token OR
%token PROJ
%token <Lambda.raise_kind> RAISE
%token RBRACKET
%token RPAREN
%token SEQ
%token SIGNED
%token SKIP
%token STAR
%token STORE
%token <string> STRING
%token SUBF
%token SUBI
%token SWITCH
%token TRY
%token UNIT
%token UNSIGNED
%token VAL
%token VALUEOFINT
%token WHILE
%token WITH
%token XOR
%token ADDRAREF
%token INTAREF
%token FLOATAREF
%token ADDRASET
%token INTASET
%token FLOATASET

%start phrase
%type <Cmm.phrase> phrase

%%

phrase:
    fundecl     { Cfunction $1 }
  | datadecl    { Cdata $1 }
  | EOF         { raise End_of_file }
;
fundecl:
    LPAREN FUNCTION fun_name LPAREN params RPAREN sequence RPAREN
      { List.iter (fun (id, ty) -> unbind_ident id) $5;
        {fun_name = $3; fun_args = $5; fun_body = $7;
         fun_codegen_options =
           if Config.flambda then [
             Reduce_code_size;
             No_CSE;
           ]
           else [ Reduce_code_size ];
         fun_poll = Lambda.Default_poll;
         fun_dbg = debuginfo ();
         (* CR yusumez: Adding return types to the parser might require a lot
            of changes, so we assume this for now. *)
         fun_ret_type = Cmm.typ_val } }
;
fun_name:
    STRING              { Cmm.global_symbol $1 }
  | IDENT               { Cmm.global_symbol $1 }
params:
    oneparam params     { $1 :: $2 }
  | /**/                { [] }
;
oneparam:
    IDENT COLON machtype { (bind_ident $1, $3) }
;
machtype:
    UNIT                        { [||] }
  | componentlist               { Array.of_list(List.rev $1) }
;
component:
    VAL                         { Val }
  | ADDR                        { Addr }
  | INT                         { Int }
  | FLOAT                       { Float }
;
componentlist:
    component                    { [$1] }
  | componentlist STAR component { $3 :: $1 }
;
traps:
    LPAREN INTCONST RPAREN       { List.init $2 (fun i -> Pop i) }
  | /**/                         { [] }
expr:
    INTCONST    { Cconst_int ($1, debuginfo ()) }
  | FLOATCONST  { Cconst_float (float_of_string $1, debuginfo ()) }
  | STRING      { Cconst_symbol (Cmm.global_symbol $1, debuginfo ()) }
  | IDENT       { Cvar(find_ident $1) }
  | LBRACKET RBRACKET { Ctuple [] }
  | LPAREN LET letdef sequence RPAREN { make_letdef $3 $4 }
  | LPAREN APPLY location expr exprlist machtype RPAREN
                { Cop(Capply ($6, Lambda.Rc_normal),
                      $4 :: List.rev $5, debuginfo ?loc:$3 ()) }
  | LPAREN EXTCALL STRING exprlist machtype RPAREN
               {Cop(Cextcall {func=$3; ty=$5; alloc=false;
                              builtin=false;
                              returns=true;
                              effects=Arbitrary_effects;
                              coeffects=Has_coeffects;
                              ty_args=[];},
                     List.rev $4, debuginfo ())}
  | LPAREN ALLOC exprlist RPAREN { Cop(Calloc (Cmm.Alloc_mode.Heap,
                                               Cmm.Alloc_block_kind_other),
                                       List.rev $3, debuginfo ()) }
  | LPAREN SUBF expr RPAREN { Cop(Cnegf Float64, [$3], debuginfo ()) }
  | LPAREN SUBF expr expr RPAREN { Cop(Csubf Float64, [$3; $4], debuginfo ()) }
  | LPAREN unaryop expr RPAREN { Cop($2, [$3], debuginfo ()) }
  | LPAREN binaryop expr expr RPAREN { Cop($2, [$3; $4], debuginfo ()) }
  | LPAREN SEQ sequence RPAREN { $3 }
  | LPAREN IF expr expr expr RPAREN
      { Cifthenelse($3, debuginfo (), $4, debuginfo (), $5, debuginfo ()) }
  | LPAREN SWITCH INTCONST expr caselist RPAREN { make_switch $3 $4 $5 }
  | LPAREN WHILE expr sequence RPAREN
      {
        let lbl0 = Lambda.next_raise_count () in
        let lbl1 = Lambda.next_raise_count () in
        let body =
          match $3 with
            Cconst_int (x, _) when x <> 0 -> $4
          | _ -> Cifthenelse($3, debuginfo (), $4, debuginfo (),
                             (Cexit(Cmm.Lbl lbl0,[],[])),
                             debuginfo ()) in
        Ccatch(Normal, [lbl0, [], Ctuple [], debuginfo (), false],
          Ccatch(Recursive,
            [lbl1, [], Csequence(body, Cexit(Cmm.Lbl lbl1, [], [])), debuginfo (), false],
            Cexit(Cmm.Lbl lbl1, [], []))) }
  | LPAREN EXIT traps IDENT exprlist RPAREN
    { Cexit(Cmm.Lbl (find_label $4), List.rev $5, $3) }
  | LPAREN CATCH sequence WITH catch_handlers RPAREN
    { let handlers = $5 in
      List.iter (fun (_, l, _, _, _) ->
        List.iter (fun (x, _) -> unbind_ident x) l) handlers;
      Ccatch(Recursive, handlers, $3) }
  | EXIT        { Cexit(Cmm.Lbl 0,[],[]) }
  | LPAREN TRY machtype sequence WITH bind_ident sequence RPAREN
      { let after_push_k = Lambda.next_raise_count () in
        let after_pop_k = Lambda.next_raise_count () in
        let exn_k = Lambda.next_raise_count () in
        let result = Backend_var.create_local "result" in
        let result' = Backend_var.With_provenance.create result in
        unbind_ident $6;
        ctrywith (
          Ccatch (Normal,
            [after_push_k, [],
             Ccatch (Normal,
               [after_pop_k, [result', $3],
                Cvar result, debuginfo (), false],
               Cexit (Cmm.Lbl after_pop_k,
                 [$4], (* original try body *)
                 [Pop exn_k])),
             debuginfo (), false],
            Cexit (Cmm.Lbl after_push_k, [], [Push exn_k])),
          exn_k,
          $6, (* exception parameter *)
          [],
          $7, (* exception handler *)
          debuginfo ()) }
  | LPAREN VAL expr expr RPAREN
      { let open Asttypes in
        Cop(Cload {memory_chunk=Word_val;
                   mutability=Mutable;
                   is_atomic=false}, [access_array $3 $4 Arch.size_addr],
          debuginfo ()) }
  | LPAREN ADDRAREF expr expr RPAREN
      { let open Asttypes in
        Cop(Cload {memory_chunk=Word_val;
                   mutability=Mutable;
                   is_atomic=false}, [access_array $3 $4 Arch.size_addr],
          Debuginfo.none) }
  | LPAREN INTAREF expr expr RPAREN
      { let open Asttypes in
        Cop(Cload {memory_chunk=Word_int;
                   mutability=Mutable;
                   is_atomic=false}, [access_array $3 $4 Arch.size_int],
          Debuginfo.none) }
  | LPAREN FLOATAREF expr expr RPAREN
      { let open Asttypes in
        Cop(Cload {memory_chunk=Double;
                   mutability=Mutable;
                   is_atomic=false}, [access_array $3 $4 Arch.size_float],
          Debuginfo.none) }
  | LPAREN ADDRASET expr expr expr RPAREN
      { let open Lambda in
        Cop(Cstore (Word_val, Assignment),
            [access_array $3 $4 Arch.size_addr; $5], Debuginfo.none) }
  | LPAREN INTASET expr expr expr RPAREN
      { let open Lambda in
        Cop(Cstore (Word_int, Assignment),
            [access_array $3 $4 Arch.size_int; $5], Debuginfo.none) }
  | LPAREN FLOATASET expr expr expr RPAREN
      { let open Lambda in
        Cop(Cstore (Double, Assignment),
            [access_array $3 $4 Arch.size_float; $5], Debuginfo.none) }
;
exprlist:
    exprlist expr               { $2 :: $1 }
  | /**/                        { [] }
;
letdef:
    oneletdef                   { [$1] }
  | LPAREN letdefmult RPAREN    { $2 }
;
letdefmult:
    /**/                        { [] }
  | oneletdef letdefmult        { $1 :: $2 }
;
oneletdef:
    IDENT expr                  { (bind_ident $1, $2) }
;
letmutdef:
    oneletmutdef                { [$1] }
  | LPAREN letmutdefmult RPAREN { $2 }
;
letmutdefmult:
    /**/                        { [] }
  | oneletmutdef letmutdefmult  { $1 :: $2 }
;
oneletmutdef:
    IDENT machtype expr         { (bind_ident $1, $2, $3) }
;
chunk:
    UNSIGNED BYTE               { Byte_unsigned }
  | SIGNED BYTE                 { Byte_signed }
  | UNSIGNED HALF               { Sixteen_unsigned }
  | SIGNED HALF                 { Sixteen_signed }
  | UNSIGNED INT32              { Thirtytwo_unsigned }
  | SIGNED INT32                { Thirtytwo_signed }
  | INT                         { Word_int }
  | ADDR                        { Word_val }
  | FLOAT32                     { Single { reg = Float64 } }
  | FLOAT64                     { Double }
  | FLOAT                       { Double }
  | VAL                         { Word_val }
;
unaryop:
    LOAD chunk                  { Cload {memory_chunk=$2;
                                         mutability=Asttypes.Mutable;
                                         is_atomic=false} }
  | FLOATOFINT                  { Cstatic_cast (Float_of_int Float64) }
  | INTOFFLOAT                  { Cstatic_cast (Int_of_float Float64) }
  | VALUEOFINT                  { Creinterpret_cast Value_of_int }
  | INTOFVALUE                  { Creinterpret_cast Int_of_value }
  | RAISE                       { Craise $1 }
  | ABSF                        { Cabsf Float64 }
;
binaryop:
    STORE chunk                 { Cstore ($2, Assignment) }
  | ADDI                        { Caddi }
  | SUBI                        { Csubi }
  | STAR                        { Cmuli }
  | DIVI                        { Cdivi }
  | MODI                        { Cmodi }
  | AND                         { Cand }
  | OR                          { Cor }
  | XOR                         { Cxor }
  | LSL                         { Clsl }
  | LSR                         { Clsr }
  | ASR                         { Casr }
  | EQI                         { Ccmpi Ceq }
  | NEI                         { Ccmpi Cne }
  | LTI                         { Ccmpi Clt }
  | LEI                         { Ccmpi Cle }
  | GTI                         { Ccmpi Cgt }
  | GEI                         { Ccmpi Cge }
  | ADDA                        { Cadda }
  | ADDV                        { Caddv }
  | EQA                         { Ccmpa Ceq }
  | NEA                         { Ccmpa Cne }
  | LTA                         { Ccmpa Clt }
  | LEA                         { Ccmpa Cle }
  | GTA                         { Ccmpa Cgt }
  | GEA                         { Ccmpa Cge }
  | ADDF                        { Caddf Float64 }
  | MULF                        { Cmulf Float64 }
  | DIVF                        { Cdivf Float64 }
  | EQF                         { Ccmpf (Float64, CFeq) }
  | NEF                         { Ccmpf (Float64, CFneq) }
  | LTF                         { Ccmpf (Float64, CFlt) }
  | NLTF                        { Ccmpf (Float64, CFnlt) }
  | LEF                         { Ccmpf (Float64, CFle) }
  | NLEF                        { Ccmpf (Float64, CFnle) }
  | GTF                         { Ccmpf (Float64, CFgt) }
  | NGTF                        { Ccmpf (Float64, CFngt) }
  | GEF                         { Ccmpf (Float64, CFge) }
  | NGEF                        { Ccmpf (Float64, CFnge) }
  | MULH                        { (Cmulhi {signed = true}) }
  | MULH UNSIGNED               { (Cmulhi {signed = false}) }
;
sequence:
    expr sequence               { Csequence($1, $2) }
  | expr                        { $1 }
;
caselist:
    onecase sequence caselist   { ($1, $2) :: $3 }
  | /**/                        { [] }
;
onecase:
    CASE INTCONST COLON onecase { $2 :: $4 }
  | CASE INTCONST COLON         { [$2] }
;
bind_ident:
    IDENT                       { bind_ident $1 }
;
datadecl:
    LPAREN datalist RPAREN      { List.rev $2 }
  | LPAREN DATA datalist RPAREN { List.rev $3 }
;
datalist:
    datalist dataitem           { $2 :: $1 }
  | /**/                        { [] }
;
dataitem:
    STRING COLON                { Cdefine_symbol (Cmm.global_symbol $1) }
  | BYTE INTCONST               { Cint8 $2 }
  | HALF INTCONST               { Cint16 $2 }
  | INT INTCONST                { Cint(Nativeint.of_int $2) }
  | FLOAT FLOATCONST            { Cdouble (float_of_string $2) }
  | ADDR STRING                 { Csymbol_address (Cmm.global_symbol $2) }
  | VAL STRING                 { Csymbol_address (Cmm.global_symbol $2) }
  | KSTRING STRING              { Cstring $2 }
  | SKIP INTCONST               { Cskip $2 }
  | ALIGN INTCONST              { Calign $2 }
;
catch_handlers:
  | catch_handler
    { [$1] }
  | catch_handler AND catch_handlers
    { $1 :: $3 }

catch_handler:
  | sequence
    { 0, [], $1, debuginfo (), false }
  | LPAREN IDENT params RPAREN sequence
    { find_label $2, $3, $5, debuginfo (), false }

location:
    /**/                        { None }
  | LOCATION                    { Some $1 }
