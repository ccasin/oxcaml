(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

let simplify_array_set (array_kind : P.Array_kind.t)
    (array_set_kind : P.Array_set_kind.t) dacc ~original_term dbg ~arg1:array
    ~arg1_ty:array_ty ~arg2:index ~arg2_ty:_ ~arg3:new_value ~arg3_ty:_
    ~result_var =
  let orig_array_kind = array_kind in
  let array_kind =
    Simplify_common.specialise_array_kind dacc array_kind ~array_ty
  in
  match array_kind with
  | Bottom -> SPR.create_invalid dacc
  | Ok array_kind ->
    let () =
      match array_kind with
      | Immediates -> ()
      | Values -> (
        match array_set_kind with
        | Values _ -> ()
        | Immediates
        (* We don't expect specialisation regressions from Immediates to
           Values. *)
        | Naked_floats | Naked_float32s | Naked_int32s | Naked_int64s
        | Naked_nativeints | Naked_vec128s | Naked_vec256s | Naked_vec512s ->
          Misc.fatal_errorf
            "Didn't expect array specialisation to yield array kind %a from \
             array set kind %a (original array kind %a):@ %a"
            P.Array_kind.print array_kind P.Array_set_kind.print array_set_kind
            P.Array_kind.print orig_array_kind Named.print original_term)
      | Naked_floats | Naked_float32s | Naked_int32s | Naked_int64s
      | Naked_nativeints | Naked_vec128s | Naked_vec256s | Naked_vec512s
      | Unboxed_product _ ->
        ()
    in
    let named =
      Named.create_prim
        (Ternary
           (Array_set (array_kind, array_set_kind), array, index, new_value))
        dbg
    in
    let unit_ty = Flambda2_types.this_tagged_immediate Targetint_31_63.zero in
    let dacc = DA.add_variable dacc result_var unit_ty in
    SPR.create named ~try_reify:false dacc

let simplify_bytes_or_bigstring_set _bytes_like_value _string_accessor_width
    dacc ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~arg3:_
    ~arg3_ty:_ ~result_var =
  SPR.create_unit dacc ~result_var ~original_term

let simplify_bigarray_set ~num_dimensions:_ _bigarray_kind _bigarray_layout dacc
    ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~arg3:_ ~arg3_ty:_
    ~result_var =
  SPR.create_unit dacc ~result_var ~original_term

let simplify_atomic_field_int_arith (_op : P.int_atomic_op) ~original_prim dacc
    ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~arg3:_ ~arg3_ty:_
    ~result_var =
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

let simplify_atomic_set_field ~original_prim dacc ~original_term _dbg ~arg1:_
    ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~arg3:_ ~arg3_ty:_ ~result_var =
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

let simplify_atomic_exchange_field ~original_prim dacc ~original_term _dbg
    ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~arg3:_ ~arg3_ty:_ ~result_var =
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

let simplify_ternary_primitive dacc original_prim (prim : P.ternary_primitive)
    ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3 ~arg3_ty dbg ~result_var =
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Array_set (array_kind, width) -> simplify_array_set array_kind width
    | Bytes_or_bigstring_set (bytes_like_value, string_accessor_width) ->
      simplify_bytes_or_bigstring_set bytes_like_value string_accessor_width
    | Bigarray_set (num_dimensions, bigarray_kind, bigarray_layout) ->
      simplify_bigarray_set ~num_dimensions bigarray_kind bigarray_layout
    | Atomic_field_int_arith op ->
      simplify_atomic_field_int_arith op ~original_prim
    | Atomic_set_field _ -> simplify_atomic_set_field ~original_prim
    | Atomic_exchange_field _ -> simplify_atomic_exchange_field ~original_prim
  in
  simplifier dacc ~original_term dbg ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3
    ~arg3_ty ~result_var
