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

open Lambda
open Parse_attributes

let check_local_inline loc attr =
  match attr.local, attr.inline with
  | Always_local, (Always_inline | Available_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Duplicated_attribute "local/inline")
  | _ ->
      ()

let check_poll_inline loc attr =
  match attr.poll, attr.inline with
  | Error_poll, (Always_inline | Available_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
           "[@poll error] is incompatible with inlining")
  | _ ->
      ()

let check_poll_local loc attr =
  match attr.poll, attr.local with
  | Error_poll, Always_local ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
          "[@poll error] is incompatible with local function optimization")
  | _ ->
      ()

let check_opaque_inline loc attr =
  match attr.is_opaque, attr.inline with
  | true, (Always_inline | Available_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
           "[@opaque] is incompatible with inlining")
  | _ ->
      ()

let check_opaque_local loc attr =
  match attr.is_opaque, attr.local with
  | true, Always_local ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
           "[@opaque] is incompatible with local function optimization")
  | _ ->
      ()


let lfunction_with_attr ~attr
  { kind; params; return; body; attr=_; loc; mode; ret_mode; region } =
  lfunction ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode ~region

let add_inline_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_inline_attribute attributes with
      | Default_inline -> expr
      | (Always_inline | Available_inline | Never_inline | Unroll _)
          as inline ->
        begin match attr.inline with
          | Default_inline -> ()
          | Always_inline | Available_inline | Never_inline | Unroll _ ->
            Location.prerr_warning loc
              (Warnings.Duplicated_attribute "inline")
        end;
        let attr = { attr with inline } in
        check_local_inline loc attr;
        check_poll_inline loc attr;
        lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_specialise_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_specialise_attribute attributes with
    | Default_specialise -> expr
    | (Always_specialise | Never_specialise) as specialise ->
      begin match attr.specialise with
      | Default_specialise -> ()
      | Always_specialise | Never_specialise ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "specialise")
      end;
      let attr = { attr with specialise } in
      lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_local_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_local_attribute attributes with
    | Default_local -> expr
    | (Always_local | Never_local) as local ->
      begin match attr.local with
      | Default_local -> ()
      | Always_local | Never_local ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "local")
      end;
      let attr = { attr with local } in
      check_local_inline loc attr;
      check_poll_local loc attr;
      lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_check_attribute expr loc attributes =
  let to_string = function
    | Zero_alloc -> "zero_alloc"
  in
  let to_string = function
    | Check { property; strict; loc = _} ->
      Printf.sprintf "assert %s%s"
        (to_string property)
        (if strict then " strict" else "")
    | Assume { property; strict; loc = _} ->
      Printf.sprintf "assume %s%s"
        (to_string property)
        (if strict then " strict" else "")
    | Ignore_assert_all property ->
      Printf.sprintf "ignore %s" (to_string property)
    | Default_check -> assert false
  in
  match expr with
  | Lfunction({ attr = { stub = false } as attr; } as funct) ->
    begin match get_property_attribute attributes Zero_alloc with
    | Default_check -> expr
    | (Ignore_assert_all p | Check { property = p; _ } | Assume { property = p; _ })
      as check ->
      begin match attr.check with
      | Default_check -> ()
      | Ignore_assert_all p'
      | Assume { property = p'; strict = _; loc = _; }
      | Check { property = p'; strict = _; loc = _; } ->
        if p = p' then
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute (to_string check));
      end;
      let attr = { attr with check } in
      lfunction_with_attr ~attr funct
    end
  | expr -> expr

let add_loop_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_loop_attribute attributes with
    | Default_loop -> expr
    | (Always_loop | Never_loop) as loop ->
      begin match attr.loop with
      | Default_loop -> ()
      | Always_loop | Never_loop ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "loop")
      end;
      let attr = { attr with loop } in
      lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_tmc_attribute expr loc attributes =
  match expr with
  | Lfunction funct ->
     begin match get_tmc_attribute attributes with
     | None -> expr
     | Some _ ->
        if funct.attr.tmc_candidate then
            Location.prerr_warning loc
              (Warnings.Duplicated_attribute "tail_mod_cons");
        let attr = { funct.attr with tmc_candidate = true } in
        lfunction_with_attr ~attr funct
     end
  | _ -> expr

let add_poll_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_poll_attribute attributes with
    | Default_poll -> expr
    | Error_poll as poll ->
      begin match attr.poll with
      | Default_poll -> ()
      | Error_poll ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "poll error")
      end;
      let attr = { attr with poll } in
      check_poll_inline loc attr;
      check_poll_local loc attr;
      let attr = { attr with inline = Never_inline; local = Never_local } in
      lfunction_with_attr ~attr funct
    end
  | expr -> expr

let add_opaque_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr } as funct) ->
      if not (get_opaque_attribute attributes) then
        expr
      else begin
        if attr.is_opaque then
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "opaque");
        let attr = { attr with is_opaque = true } in
        check_opaque_inline loc attr;
        check_opaque_local loc attr;
        let attr = { attr with inline = Never_inline; local = Never_local } in
        lfunction_with_attr ~attr funct
      end
  | _ -> expr

let add_function_attributes lam loc attr =
  let lam =
    add_inline_attribute lam loc attr
  in
  let lam =
    add_specialise_attribute lam loc attr
  in
  let lam =
    add_local_attribute lam loc attr
  in
  let lam =
    add_check_attribute lam loc attr
  in
  let lam =
    add_loop_attribute lam loc attr
  in
  let lam =
    add_tmc_attribute lam loc attr
  in
  (* last because poll and opaque overrides inline and local *)
  let lam =
    add_poll_attribute lam loc attr
  in
  let lam =
    add_opaque_attribute lam loc attr
  in
  lam
