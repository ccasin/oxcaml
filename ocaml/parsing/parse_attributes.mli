(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module has code for parsing and interpreting attributes that can appear
    on functions or applications.
*)

open Parsetree

type specialise_attribute =
  | Always_specialise (* [@specialise] or [@specialise always] *)
  | Never_specialise (* [@specialise never] *)
  | Default_specialise (* no [@specialise] attribute *)

val equal_specialise_attribute
   : specialise_attribute
  -> specialise_attribute
  -> bool

type local_attribute =
  | Always_local (* [@local] or [@local always] *)
  | Never_local (* [@local never] *)
  | Default_local (* [@local maybe] or no [@local] attribute *)

type property =
  | Zero_alloc

type poll_attribute =
  | Error_poll (* [@poll error] *)
  | Default_poll (* no [@poll] attribute *)

type check_attribute =
  | Default_check
  | Ignore_assert_all of property
  | Check of { property: property;
               strict: bool;
               (* [strict=true] property holds on all paths.
                  [strict=false] if the function returns normally,
                  then the property holds (but property violations on
                  exceptional returns or divering loops are ignored).
                  This definition may not be applicable to new properties. *)
               opt: bool;
               loc: Location.t;
             }
  | Assume of { property: property;
                strict: bool;
                loc: Location.t;
                never_returns_normally: bool;
              }

type loop_attribute =
  | Always_loop (* [@loop] or [@loop always] *)
  | Never_loop (* [@loop never] *)
  | Default_loop (* no [@loop] attribute *)

type tailcall_attribute =
  | Tailcall_expectation of bool
    (* [@tailcall] and [@tailcall true] have [true],
       [@tailcall false] has [false] *)
  | Default_tailcall (* no [@tailcall] attribute *)

(* Function declaration inlining annotations *)
type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Available_inline (* [@inline available] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inline (* no [@inline] attribute *)

(* Call site inlining annotations *)
type inlined_attribute =
  | Always_inlined (* [@inlined] or [@inlined always] *)
  | Never_inlined (* [@inlined never] *)
  | Hint_inlined (* [@inlined hint] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inlined (* no [@inlined] attribute *)


val equal_inline_attribute : inline_attribute -> inline_attribute -> bool
val equal_inlined_attribute : inlined_attribute -> inlined_attribute -> bool

val get_inline_attribute : attributes -> inline_attribute
val get_specialise_attribute : attributes -> specialise_attribute
val get_local_attribute : attributes -> local_attribute
val get_check_attribute : attributes -> check_attribute
val get_poll_attribute : attributes -> poll_attribute
val get_loop_attribute : attributes -> loop_attribute
val get_opaque_attribute : attributes -> bool

val get_property_attribute : attributes -> property -> check_attribute

val get_tmc_attribute : attributes -> attribute option

val get_tailcall_attribute : attributes -> tailcall_attribute
val get_inlined_attribute : attributes -> inlined_attribute
val get_specialised_attribute : attributes -> inlined_attribute

val assume_zero_alloc : Parsetree.attributes -> bool

