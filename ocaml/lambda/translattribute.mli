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

val add_inline_attribute
   : Lambda.lambda
  -> Location.t
  -> Parsetree.attributes
  -> Lambda.lambda

val add_specialise_attribute
   : Lambda.lambda
  -> Location.t
  -> Parsetree.attributes
  -> Lambda.lambda

val add_local_attribute
   : Lambda.lambda
  -> Location.t
  -> Parsetree.attributes
  -> Lambda.lambda

val add_function_attributes
  : Lambda.lambda
  -> Location.t
  -> Parsetree.attributes
  -> Lambda.lambda
