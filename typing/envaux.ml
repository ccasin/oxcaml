(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Env

type error =
    Module_not_found of Path.t

exception Error of error

let env_cache =
  (Hashtbl.create 59 : ((Env.summary * Subst.t), Env.t) Hashtbl.t)

let reset_cache ~preserve_persistent_env =
  Hashtbl.clear env_cache;
  Env.reset_cache ~preserve_persistent_env

let rec env_from_summary ~allow_missing_modules sum subst =
  try
    Hashtbl.find env_cache (sum, subst)
  with Not_found ->
    let env =
      match sum with
        Env_empty ->
          Env.empty
      | Env_value(s, id, desc, mode) ->
          let desc =
            Subst.Lazy.of_value_description desc
            |> Subst.Lazy.value_description subst
          in
          Env.add_value_lazy ~mode id desc (env_from_summary ~allow_missing_modules s subst)
      | Env_type(s, id, desc) ->
          Env.add_type ~check:false id
            (Subst.type_declaration subst desc)
            (env_from_summary ~allow_missing_modules s subst)
      | Env_extension(s, id, desc) ->
          Env.add_extension ~check:false ~rebind:false id
            (Subst.extension_constructor subst desc)
            (env_from_summary ~allow_missing_modules s subst)
      | Env_module(s, id, pres, desc, mode, locks) ->
          let desc =
            Subst.Lazy.module_decl Keep subst (Subst.Lazy.of_module_decl desc)
          in
          Env.add_module_declaration_lazy ~update_summary:true id pres desc
            ~mode ~locks (env_from_summary ~allow_missing_modules s subst)
      | Env_modtype(s, id, desc) ->
          let desc =
            Subst.Lazy.modtype_decl Keep subst (Subst.Lazy.of_modtype_decl desc)
          in
          Env.add_modtype_lazy ~update_summary:true id desc
            (env_from_summary ~allow_missing_modules s subst)
      | Env_class(s, id, desc) ->
          Env.add_class id (Subst.class_declaration subst desc)
                        (env_from_summary ~allow_missing_modules s subst)
      | Env_cltype (s, id, desc) ->
          Env.add_cltype id (Subst.cltype_declaration subst desc)
                         (env_from_summary ~allow_missing_modules s subst)
      | Env_open(s, path) ->
          let env = env_from_summary ~allow_missing_modules s subst in
          let path' = Subst.module_path subst path in
          if allow_missing_modules then
            (try Env.open_signature_by_path path' env with
            | Not_found -> env)
          else Env.open_signature_by_path path' env
      | Env_functor_arg(Env_module(s, id, pres, desc, mode, locks), id')
            when Ident.same id id' ->
          let desc =
            Subst.Lazy.module_decl Keep subst (Subst.Lazy.of_module_decl desc)
          in
          Env.add_module_declaration_lazy ~update_summary:true id pres desc
            ~mode ~locks
            ~arg:true (env_from_summary ~allow_missing_modules s subst)
      | Env_functor_arg _ -> assert false
      | Env_constraints(s, map) ->
          Path.Map.fold
            (fun path info ->
              Env.add_local_constraint (Subst.type_path subst path)
                (Subst.type_declaration subst info))
            map (env_from_summary ~allow_missing_modules s subst)
      | Env_copy_types s ->
          let env = env_from_summary ~allow_missing_modules s subst in
          Env.make_copy_of_types env env
      | Env_persistent (s, id) ->
          let env = env_from_summary ~allow_missing_modules s subst in
          Env.add_persistent_structure id env
      | Env_value_unbound (s, str, reason) ->
          let env = env_from_summary ~allow_missing_modules s subst in
          Env.enter_unbound_value str reason env
      | Env_module_unbound (s, str, reason) ->
          let env = env_from_summary ~allow_missing_modules s subst in
          Env.enter_unbound_module str reason env
    in
      Hashtbl.add env_cache (sum, subst) env;
      env

let env_of_only_summary ?(allow_missing_modules = false) env =
  Env.env_of_only_summary (env_from_summary ~allow_missing_modules) env

(* Error report *)

open Format
module Style = Misc.Style

let report_error ppf = function
  | Module_not_found p ->
      fprintf ppf "@[Cannot find module %a@].@."
        (Style.as_inline_code Printtyp.path) p

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
