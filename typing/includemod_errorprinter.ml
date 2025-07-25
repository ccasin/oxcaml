(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Style = Misc.Style

module Context = struct
  type pos =
    | Module of Ident.t
    | Modtype of Ident.t
    | Arg of Types.functor_parameter
    | Body of Types.functor_parameter

  let path_of_context = function
      Module id :: rem ->
        let rec subm path = function
          | [] -> path
          | Module id :: rem -> subm (Path.Pdot (path, Ident.name id)) rem
          | _ -> assert false
        in subm (Path.Pident id) rem
    | _ -> assert false


  let rec context ppf = function
      Module id :: rem ->
        Format.fprintf ppf "@[<2>module %a%a@]" Printtyp.ident id args rem
    | Modtype id :: rem ->
        Format.fprintf ppf "@[<2>module type %a =@ %a@]"
          Printtyp.ident id context_mty rem
    | Body x :: rem ->
        Format.fprintf ppf "functor (%s) ->@ %a" (argname x) context_mty rem
    | Arg x :: rem ->
        Format.fprintf ppf "functor (%s : %a) -> ..."
          (argname x) context_mty rem
    | [] ->
        Format.fprintf ppf "<here>"
  and context_mty ppf = function
      (Module _ | Modtype _) :: _ as rem ->
        Format.fprintf ppf "@[<2>sig@ %a@;<1 -2>end@]" context rem
    | cxt -> context ppf cxt
  and args ppf = function
      Body x :: rem ->
        Format.fprintf ppf "(%s)%a" (argname x) args rem
    | Arg x :: rem ->
        Format.fprintf ppf "(%s :@ %a) : ..." (argname  x) context_mty rem
    | cxt ->
        Format.fprintf ppf " :@ %a" context_mty cxt
  and argname = function
    | Types.Unit -> ""
    | Types.Named (None, _) -> "_"
    | Types.Named (Some id, _) -> Ident.name id

  let alt_pp ppf cxt =
    if cxt = [] then () else
    if List.for_all (function Module _ -> true | _ -> false) cxt then
      Format.fprintf ppf "in module %a,"
        (Style.as_inline_code Printtyp.path) (path_of_context cxt)
    else
      Format.fprintf ppf "@[<hv 2>at position@ %a,@]"
        (Style.as_inline_code context) cxt

  let pp ppf cxt =
    if cxt = [] then () else
    if List.for_all (function Module _ -> true | _ -> false) cxt then
      Format.fprintf ppf "In module %a:@ "
        (Style.as_inline_code Printtyp.path) (path_of_context cxt)
    else
      Format.fprintf ppf "@[<hv 2>At position@ %a@]@ "
        (Style.as_inline_code context) cxt
end

module Illegal_permutation = struct
  (** Extraction of information in case of illegal permutation
      in a module type *)

  (** When examining coercions, we only have runtime component indices,
      we use thus a limited version of {!pos}. *)
  type coerce_pos =
    | Item of int
    | InArg
    | InBody

  let either f x g y = match f x with
    | None -> g y
    | Some _ as v -> v

  (** We extract a lone transposition from a full tree of permutations. *)
  let rec transposition_under path (coerc:Typedtree.module_coercion) =
    match coerc with
    | Tcoerce_structure(c,_) ->
        either
          (not_fixpoint path 0) c
          (first_non_id path 0) c
    | Tcoerce_functor(arg,res) ->
        either
          (transposition_under (InArg::path)) arg
          (transposition_under (InBody::path)) res
    | Tcoerce_none -> None
    | Tcoerce_alias _ | Tcoerce_primitive _ ->
        (* these coercions are not inversible, and raise an error earlier when
           checking for module type equivalence *)
        assert false
  (* we search the first point which is not invariant at the current level *)
  and not_fixpoint path pos = function
    | [] -> None
    | (n, _) :: q ->
        if n = pos then
          not_fixpoint path (pos+1) q
        else
          Some(List.rev path, pos, n)
  (* we search the first item with a non-identity inner coercion *)
  and first_non_id path pos = function
    | [] -> None
    | (_, Typedtree.Tcoerce_none) :: q -> first_non_id path (pos + 1) q
    | (_,c) :: q ->
        either
          (transposition_under (Item pos :: path)) c
          (first_non_id path (pos + 1)) q

  let transposition c =
    match transposition_under [] c with
    | None -> raise Not_found
    | Some x -> x

  let rec runtime_item k = function
    | [] -> raise Not_found
    | item :: q ->
        if not(Includemod.is_runtime_component item) then
          runtime_item k q
        else if k = 0 then
          item
        else
          runtime_item (k-1) q

  (* Find module type at position [path] and convert the [coerce_pos] path to
     a [pos] path *)
  let rec find env ctx path (mt:Types.module_type) =
    match Mtype.scrape_alias env mt, path with
    | Mty_signature s , [] -> List.rev ctx, s
    | Mty_signature s, Item k :: q ->
        begin match runtime_item k s with
        | Sig_module (id, _, md,_,_) ->
            find env (Context.Module id :: ctx) q md.md_type
        | _ -> raise Not_found
        end
    | Mty_functor(Named (_,mt) as arg,_), InArg :: q ->
        find env (Context.Arg arg :: ctx) q mt
    | Mty_functor(arg, mt), InBody :: q ->
        find env (Context.Body arg :: ctx) q mt
    | _ -> raise Not_found

  let find env path mt = find env [] path mt
  let item mt k = Includemod.item_ident_name (runtime_item k mt)

  let pp_item ppf (id,_,kind) =
    Format.fprintf ppf "%s %a"
      (Includemod.kind_of_field_desc kind)
      Style.inline_code (Ident.name id)

  let pp ctx_printer env ppf (mty,c) =
    try
      let p, k, l = transposition c in
      let ctx, mt = find env p mty in
      Format.fprintf ppf
        "@[<hv 2>Illegal permutation of runtime components in a module type.@ \
         @[For example,@ %a@]@ @[the %a@ and the %a are not in the same order@ \
         in the expected and actual module types.@]@]"
        ctx_printer ctx pp_item (item mt k) pp_item (item mt l)
    with Not_found -> (* this should not happen *)
      Format.fprintf ppf
        "Illegal permutation of runtime components in a module type."

end



module Err = Includemod.Error

let is_big p =
  let size = !Clflags.error_size in
  size > 0 && Misc.is_print_longer_than size p

let show_loc msg ppf loc =
  let pos = loc.Location.loc_start in
  if List.mem pos.Lexing.pos_fname [""; "_none_"; "//toplevel//"] then ()
  else Format.fprintf ppf "@\n@[<2>%a:@ %s@]" Location.print_loc loc msg

let show_locs ppf (loc1, loc2) =
  show_loc "Expected declaration" ppf loc2;
  show_loc "Actual declaration" ppf loc1


let dmodtype mty =
  let tmty = Printtyp.tree_of_modtype ~abbrev:true mty in
  Format.dprintf "%a" !Oprint.out_module_type tmty

let space ppf () = Format.fprintf ppf "@ "


(** Checks if the error is a mode error at the leaf node, and returns the
  offending axis. *)
module Is_modal = struct
  open Err
  let rec module_type_symptom = function
    | Mode (Error (ax, _)) -> Some (Mode.Value.Axis.P ax)
    | Signature s -> signature_symptom s
    | Functor _ | Invalid_module_alias _ | After_alias_expansion _ | Mt_core _
      -> None

  and signature_symptom {incompatibles} =
    List.find_map (fun (_, s) -> sigitem_symptom s)
      incompatibles

  and sigitem_symptom = function
    | Core s -> core_sigitem_symptom s
    | Module_type d -> module_type_symptom d.symptom
    | Module_type_declaration _ -> None

  and core_sigitem_symptom = function
    | Value_descriptions d -> value_mismatch d.symptom
    | Class_declarations d -> class_declaration_symptom d.symptom
    | Type_declarations _ | Extension_constructors _ | Class_type_declarations _
    | Modalities _ -> None

  and class_declaration_symptom = function
    | Class_mode (Error (ax, _)) -> Some (Mode.Value.Axis.P ax)
    | Class_type _ -> None

  and value_mismatch : Includecore.value_mismatch -> _ = function
    | Mode (Error (ax, _)) -> Some (Mode.Value.Axis.P ax)
    | _ -> None
end

(* CR zqian: refactor to remove the following two functions *)
let zap_axis_to_floor
  : type a d0 d1. (a, d0, d1) Mode.Value.Axis.t -> Mode.Value.l -> a
  = fun ax m ->
  match ax with
  | Comonadic Areality ->
      Mode.Regionality.zap_to_floor (Mode.Value.proj (Comonadic Areality) m)
  | Comonadic Linearity ->
      Mode.Linearity.zap_to_floor (Mode.Value.proj (Comonadic Linearity) m)
  | Comonadic Portability ->
      Mode.Portability.zap_to_floor (Mode.Value.proj (Comonadic Portability)  m)
  | Comonadic Yielding ->
      Mode.Yielding.zap_to_floor (Mode.Value.proj (Comonadic Yielding) m)
  | Comonadic Statefulness ->
      Mode.Statefulness.zap_to_floor
        (Mode.Value.proj (Comonadic Statefulness) m)
  | Monadic Uniqueness ->
      Mode.Uniqueness.zap_to_floor (Mode.Value.proj (Monadic Uniqueness) m)
  | Monadic Contention ->
      Mode.Contention.zap_to_floor (Mode.Value.proj (Monadic Contention) m)
  | Monadic Visibility ->
      Mode.Visibility.zap_to_floor (Mode.Value.proj (Monadic Visibility) m)

let zap_axis_to_ceil
  : type a d0 d1. (a, d0, d1) Mode.Value.Axis.t -> Mode.Value.r -> a
  = fun ax m ->
  match ax with
  | Comonadic Areality ->
      Mode.Regionality.zap_to_ceil (Mode.Value.proj (Comonadic Areality) m)
  | Comonadic Linearity ->
      Mode.Linearity.zap_to_ceil (Mode.Value.proj (Comonadic Linearity) m)
  | Comonadic Portability ->
      Mode.Portability.zap_to_ceil (Mode.Value.proj (Comonadic Portability) m)
  | Comonadic Yielding ->
      Mode.Yielding.zap_to_ceil (Mode.Value.proj (Comonadic Yielding) m)
  | Comonadic Statefulness ->
      Mode.Statefulness.zap_to_ceil (Mode.Value.proj (Comonadic Statefulness) m)
  | Monadic Uniqueness ->
      Mode.Uniqueness.zap_to_ceil (Mode.Value.proj (Monadic Uniqueness) m)
  | Monadic Contention ->
      Mode.Contention.zap_to_ceil (Mode.Value.proj (Monadic Contention) m)
  | Monadic Visibility ->
      Mode.Visibility.zap_to_ceil (Mode.Value.proj (Monadic Visibility) m)

let print_out_mode
: type a d0 d1. ?in_structure:_ -> (a, d0, d1) Mode.Value.Axis.t -> a -> _
= fun ?(in_structure=true) ax mode ->
  let (module L) = Mode.Value.Const.lattice_of_axis ax in
  if in_structure then
    Format.dprintf " (* in a structure at %a *)" L.print mode
  else
    Format.dprintf " (* at %a *)" L.print mode

let maybe_print_mode_l ~is_modal (mode : Mode.Value.l) =
  match is_modal with
  | None -> fun _ppf -> ()
  | Some ax ->
      let (P ax) : Mode.Value.Axis.packed = ax in
      let mode =
        mode
        (* error printing, so mutation doesn't need to be backtracked *)
        |> zap_axis_to_floor ax
      in
      print_out_mode ax mode

let print_modes ?in_structure ax (modes : Includemod.modes) =
  let (P ax) : Mode.Value.Axis.packed = ax in
  let mode1, mode2 =
    match modes with
    | All -> assert false
    | Specific (mode1, mode2, _) -> mode1, mode2
  in
  let mode1 =
    mode1
    (* error printing, so mutation doesn't need to be backtracked *)
    |> zap_axis_to_floor ax
  in
  let mode2 =
    mode2
    |> zap_axis_to_ceil ax
  in
  print_out_mode ?in_structure ax mode1, print_out_mode ?in_structure ax mode2

let maybe_print_modes ?in_structure ~is_modal (modes : Includemod.modes) =
  match is_modal with
  | None -> (fun _ppf -> ()), (fun _ppf -> ())
  | Some ax -> print_modes ?in_structure ax modes

(**
   In order to display a list of functor arguments in a compact format,
   we introduce a notion of shorthand for functor arguments.
   The aim is to first present the lists of actual and expected types with
   shorthands:

     (X: $S1) (Y: $S2) (Z: An_existing_module_type) ...
   does not match
     (X: $T1) (Y: A_real_path) (Z: $T3) ...

   and delay the full display of the module types corresponding to $S1, $S2,
   $T1, and $T3 to the suberror message.

*)
module With_shorthand = struct

  (** A item with a potential shorthand name *)
  type 'a named = {
    item: 'a;
    name : string;
  }

  type 'a t =
    | Original of 'a (** The shorthand has been discarded *)
    | Synthetic of 'a named
    (** The shorthand is potentially useful *)

  type functor_param =
    | Unit
    | Named of (Ident.t option * Types.module_type t)

  (** Shorthand generation *)
  type kind =
    | Got
    | Expected
    | Unneeded

  type variant =
    | App
    | Inclusion

  let elide_if_app ctx s = match ctx with
    | App -> Unneeded
    | Inclusion -> s

  let make side pos =
    match side with
    | Got -> Format.sprintf "$S%d" pos
    | Expected -> Format.sprintf "$T%d" pos
    | Unneeded -> "..."

  (** Add shorthands to a patch *)
  open Diffing
  let patch ctx p =
    let add_shorthand side pos mty =
      {name = (make side pos); item = mty }
    in
    let aux i d =
      let pos = i + 1 in
      let d = match d with
        | Insert mty ->
            Insert (add_shorthand Expected pos mty)
        | Delete mty ->
            Delete (add_shorthand (elide_if_app ctx Got) pos mty)
        | Change (g, e, p) ->
            Change
              (add_shorthand Got pos g,
               add_shorthand Expected pos e, p)
        | Keep (g, e, p) ->
            Keep (add_shorthand Got pos g,
                          add_shorthand (elide_if_app ctx Expected) pos e, p)
      in
      pos, d
    in
    List.mapi aux p

  (** Shorthand computation from named item *)
  let modtype (r : _ named) = match r.item with
    | Types.Mty_ident _
    | Types.Mty_alias _
    | Types.Mty_signature []
    | Types.Mty_strengthen _
      -> Original r.item
    | Types.Mty_signature _ | Types.Mty_functor _
      -> Synthetic r

  let functor_param (ua : _ named) = match ua.item with
    | Types.Unit -> Unit
    | Types.Named (from, mty) ->
        Named (from, modtype { ua with item = mty })

  (** Printing of arguments with shorthands *)
  let pp ppx = function
    | Original x -> ppx x
    | Synthetic s -> Format.dprintf "%s" s.name

  let pp_orig ppx = function
    | Original x | Synthetic { item=x; _ } -> ppx x

  let definition_aux x = match functor_param x with
    | Unit -> Format.dprintf "()"
    | Named(_,short_mty) ->
        match short_mty with
        | Original mty -> dmodtype mty
        | Synthetic {name; item = mty} ->
            Format.dprintf
              "%s@ =@ %t" name (dmodtype mty)

  let definition ~is_modal x =
    let mode =
      Types.functor_param_mode
      |> Mode.alloc_as_value
      |> Mode.Value.disallow_right
    in
    let mode = maybe_print_mode_l ~is_modal mode in
    Format.dprintf "%t%t" (definition_aux x) mode

  let param x = match functor_param x with
    | Unit -> Format.dprintf "()"
    | Named (_, short_mty) ->
        pp dmodtype short_mty

  let qualified_param x = match functor_param x with
    | Unit -> Format.dprintf "()"
    | Named (None, Original (Mty_signature []) ) ->
        Format.dprintf "(sig end)"
    | Named (None, short_mty) ->
        pp dmodtype short_mty
    | Named (Some p, short_mty) ->
        Format.dprintf "(%s : %t)"
          (Ident.name p) (pp dmodtype short_mty)

  let definition_of_argument_aux ua =
    let arg, mty, _ = ua.item in
    match (arg: Err.functor_arg_descr) with
    | Unit -> Format.dprintf "()"
    | Empty_struct -> Format.dprintf "(struct end)"
    | Named p ->
        let mty = match mty with
          | Types.Mty_strengthen (mty,q,_) when Path.same p q -> mty
          | _ -> mty
        in
        let mty = modtype { ua with item = mty } in
        Format.dprintf
          "%a@ :@ %t"
          Printtyp.path p
          (pp_orig dmodtype mty)
    | Anonymous ->
        let short_mty = modtype { ua with item = mty } in
        begin match short_mty with
        | Original mty -> dmodtype mty
        | Synthetic {name; item=mty} ->
            Format.dprintf "%s@ :@ %t" name (dmodtype mty)
        end

  let definition_of_argument ~is_modal ua =
    let _, _, (mode, _locks) = ua.item in
    let mode = maybe_print_mode_l ~is_modal mode in
    Format.dprintf "%t%t" (definition_of_argument_aux ua) mode

  let arg_aux ua =
    let arg, mty, _ = ua.item in
    match (arg: Err.functor_arg_descr) with
    | Unit -> Format.dprintf "()"
    | Empty_struct -> Format.dprintf "(struct end)"
    | Named p -> fun ppf -> Printtyp.path ppf p
    | Anonymous ->
        let short_mty = modtype { ua with item=mty } in
        pp dmodtype short_mty

  let arg ~is_modal ua =
    let _, _, (mode, _locks) = ua.item in
    let mode = maybe_print_mode_l ~is_modal mode in
    Format.dprintf "%t%t" (arg_aux ua) mode

end


module Functor_suberror = struct
  open Err

  let param_id x = match x.With_shorthand.item with
    | Types.Named (Some _ as x,_) -> x
    | Types.(Unit | Named(None,_)) -> None

  (** Print the list of params with style *)
  let pretty_params sep proj printer patch =
    let elt (x,param) =
      let sty = Diffing.(style @@ classify x) in
      Format.dprintf "%a%t%a"
        Format.pp_open_stag (Style.Style sty)
        (printer param)
        Format.pp_close_stag ()
    in
    let params = List.filter_map proj @@ List.map snd patch in
    Printtyp.functor_parameters ~sep elt params

  let expected d =
    let extract: _ Diffing.change -> _ = function
      | Insert mty
      | Keep(_,mty,_)
      | Change (_,mty,_) as x ->
          Some (param_id mty,(x, mty))
      | Delete _ -> None
    in
    pretty_params space extract With_shorthand.qualified_param d

  let drop_inserted_suffix patch =
    let rec drop = function
      | Diffing.Insert _ :: q -> drop q
      | rest -> List.rev rest in
    drop (List.rev patch)

  let prepare_patch ~drop ~ctx patch =
    let drop_suffix x = if drop then drop_inserted_suffix x else x in
    patch |> drop_suffix |> With_shorthand.patch ctx


  module Inclusion = struct

    let got d =
      let extract: _ Diffing.change -> _ = function
      | Delete mty
      | Keep (mty,_,_)
      | Change (mty,_,_) as x ->
          Some (param_id mty,(x,mty))
      | Insert _ -> None
      in
      pretty_params space extract With_shorthand.qualified_param d

    let insert mty =
      Format.dprintf
        "An argument appears to be missing with module type@;<1 2>@[%t@]"
        (With_shorthand.definition ~is_modal:None mty)

    let delete mty =
      Format.dprintf
        "An extra argument is provided of module type@;<1 2>@[%t@]"
        (With_shorthand.definition ~is_modal:None mty)

      let ok x y =
        Format.dprintf
          "Module types %t and %t match"
          (With_shorthand.param x)
          (With_shorthand.param y)

      let diff g e more =
        let g = With_shorthand.definition ~is_modal:None g in
        let e = With_shorthand.definition ~is_modal:None e in
        Format.dprintf
          "Module types do not match:@ @[%t@]@;<1 -2>does not include@ \
           @[%t@]%t"
          g e (more ())

      let incompatible = function
        | Types.Unit ->
            Format.dprintf
              "The functor was expected to be applicative at this position"
        | Types.Named _ ->
            Format.dprintf
              "The functor was expected to be generative at this position"

      let patch env got expected =
        Includemod.Functor_inclusion_diff.diff env got expected
        |> prepare_patch ~drop:false ~ctx:Inclusion

    end

  module App = struct

    let patch env ~f ~args =
      Includemod.Functor_app_diff.diff env ~f ~args
      |> prepare_patch ~drop:true ~ctx:App

    let got ~is_modal d =
      let extract: _ Diffing.change -> _ = function
        | Delete mty
        | Keep (mty,_,_)
        | Change (mty,_,_) as x ->
            Some (None,(x,mty))
        | Insert _ -> None
      in
      pretty_params space extract (With_shorthand.arg ~is_modal) d

    let delete mty =
      Format.dprintf
        "The following extra argument is provided@;<1 2>@[%t@]"
        (With_shorthand.definition_of_argument ~is_modal:None mty)

    let insert = Inclusion.insert

    let ok x y =
      let pp_orig_name = match With_shorthand.functor_param y with
        | With_shorthand.Named (_, Original mty) ->
            Format.dprintf " %t" (dmodtype mty)
        | _ -> ignore
      in
      Format.dprintf
        "Module %t matches the expected module type%t"
        (With_shorthand.arg ~is_modal:None x)
        pp_orig_name

    let diff ~is_modal g e more =
      let g = With_shorthand.definition_of_argument ~is_modal g in
      let e = With_shorthand.definition ~is_modal e in
      Format.dprintf
        "Modules do not match:@ @[%t@]@;<1 -2>\
         is not included in@ @[%t@]%t"
        g e (more ())

    (** Specialized to avoid introducing shorthand names
        for single change difference
    *)
    let single_diff ~is_modal g e more =
      let _arg, mty, (mode1, locks) = g.With_shorthand.item in
      let mode2 =
        Types.functor_param_mode
        |> Mode.alloc_as_value
        |> Mode.Value.disallow_left
      in
      let modes : Includemod.modes = Specific (mode1, mode2, locks) in
      let mode1, mode2 = maybe_print_modes ~is_modal modes in
      let e = match e.With_shorthand.item with
        | Types.Unit -> Format.dprintf "()"
        | Types.Named(_, mty) -> dmodtype mty
      in
      Format.dprintf
        "Modules do not match:@ @[%t%t@]@;<1 -2>\
         is not included in@ @[%t%t@]%t"
        (dmodtype mty) mode1 e mode2 (more ())


    let incompatible = function
      | Unit ->
          Format.dprintf
            "The functor was expected to be applicative at this position"
      | Named _ | Anonymous ->
          Format.dprintf
            "The functor was expected to be generative at this position"
      | Empty_struct ->
          (* an empty structure can be used in both applicative and generative
             context *)
          assert false
  end

  let subcase sub ~expansion_token env (pos, diff) =
    Location.msg "%a%a%a%a@[<hv 2>%t@]%a"
      Format.pp_print_tab ()
      Format.pp_open_tbox ()
      Diffing.prefix (pos, Diffing.classify diff)
      Format.pp_set_tab ()
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )
     Format.pp_close_tbox ()

  let onlycase sub ~expansion_token env (_, diff) =
    Location.msg "%a@[<hv 2>%t@]"
      Format.pp_print_tab ()
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )

  let params sub ~expansion_token env l =
    let rec aux subcases = function
      | [] -> subcases
      | (_, Diffing.Keep _) as a :: q ->
          aux (subcase sub ~expansion_token env a :: subcases) q
      | a :: q ->
          List.fold_left (fun acc x ->
            (subcase sub ~expansion_token:false env x) :: acc
            )
            (subcase sub ~expansion_token env a :: subcases)
            q
    in
    match l with
    | [a] -> [onlycase sub ~expansion_token env a]
    | l -> aux [] l
end


(** Construct a linear presentation of the error tree *)

open Err

(* Context helper functions *)
let with_context ?loc ctx printer diff =
  Location.msg ?loc "%a%a" Context.pp (List.rev ctx)
    printer diff

let dwith_context ?loc ctx printer =
  Location.msg ?loc "%a%t" Context.pp (List.rev ctx) printer

let dwith_context_and_elision ?loc ctx print_diff =
  if is_big print_diff then
    Location.msg ?loc "..."
  else
    dwith_context ?loc ctx print_diff

(* Merge sub msgs into one printer *)
let coalesce msgs =
  match List.rev msgs with
  | [] -> ignore
  | before ->
      let ctx ppf =
        Format.pp_print_list ~pp_sep:space
          (fun ppf x -> x.Location.txt ppf)
          ppf before in
      ctx

let subcase_list l ppf = match l with
  | [] -> ()
  | _ :: _ ->
      Format.fprintf ppf "@;<1 -2>@[%a@]"
        (Format.pp_print_list ~pp_sep:space
           (fun ppf f -> f.Location.txt ppf)
        )
        (List.rev l)

(* Printers for leaves *)
let core env id x =
  match x with
  | Err.Value_descriptions {symptom = Modality e} ->
      Format.dprintf "@[<hv>%s:@;%a@]"
        ("Modalities on " ^ (Ident.name id) ^ " do not match")
        (Includecore.report_modality_sub_error "the first" "the second") e
  | Err.Value_descriptions diff ->
      let is_modal = Is_modal.value_mismatch diff.symptom in
      let mode1, mode2 = maybe_print_modes ~is_modal diff.modes in
      Format.dprintf "@[<v>@[<hv>%s:@;<1 2>%a%t@ %s@;<1 2>%a%t@]%a%a%t@]"
        "Values do not match"
        !Oprint.out_sig_item
        (Printtyp.tree_of_value_description id diff.got)
        mode1
        "is not included in"
        !Oprint.out_sig_item
        (Printtyp.tree_of_value_description id diff.expected)
        mode2
        (Includecore.report_value_mismatch
           "the first" "the second" env) diff.symptom
        show_locs (diff.got.val_loc, diff.expected.val_loc)
        Printtyp.Conflicts.print_explanations
  | Err.Modalities e ->
      Format.dprintf "@[<hv>%s:@;%a@]"
        ("Modalities on " ^ (Ident.name id) ^ " do not match")
        (Includecore.report_modality_sub_error "the first" "the second") e
  | Err.Type_declarations diff ->
      Format.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a%a%t@]"
        "Type declarations do not match"
        !Oprint.out_sig_item
        (Printtyp.tree_of_type_declaration id diff.got Trec_first)
        "is not included in"
        !Oprint.out_sig_item
        (Printtyp.tree_of_type_declaration id diff.expected Trec_first)
        (Includecore.report_type_mismatch
           "the first" "the second" "declaration" env) diff.symptom
        show_locs (diff.got.type_loc, diff.expected.type_loc)
        Printtyp.Conflicts.print_explanations
  | Err.Extension_constructors diff ->
      Format.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]@ %a%a%t@]"
        "Extension declarations do not match"
        !Oprint.out_sig_item
        (Printtyp.tree_of_extension_constructor id diff.got Text_first)
        "is not included in"
        !Oprint.out_sig_item
        (Printtyp.tree_of_extension_constructor id diff.expected Text_first)
        (Includecore.report_extension_constructor_mismatch
           "the first" "the second" "declaration" env) diff.symptom
        show_locs (diff.got.ext_loc, diff.expected.ext_loc)
        Printtyp.Conflicts.print_explanations
  | Err.Class_type_declarations diff ->
      Format.dprintf
        "@[<hv 2>Class type declarations do not match:@ \
         %a@;<1 -2>does not match@ %a@]@ %a%t"
        !Oprint.out_sig_item
        (Printtyp.tree_of_cltype_declaration id diff.got Trec_first)
        !Oprint.out_sig_item
        (Printtyp.tree_of_cltype_declaration id diff.expected Trec_first)
        (Includeclass.report_error Type_scheme) diff.symptom
        Printtyp.Conflicts.print_explanations
  | Err.Class_declarations {got;expected;symptom=Class_type reason} ->
      let t1 = Printtyp.tree_of_class_declaration id got Trec_first in
      let t2 = Printtyp.tree_of_class_declaration id expected Trec_first in
      Format.dprintf
        "@[<hv 2>Class declarations do not match:@ \
         %a@;<1 -2>does not match@ %a@]@ %a%t"
        !Oprint.out_sig_item t1
        !Oprint.out_sig_item t2
        (Includeclass.report_error Type_scheme) reason
        Printtyp.Conflicts.print_explanations
  | Err.Class_declarations {symptom=Class_mode e} ->
      Format.dprintf
        "@[<hv 2>Class declarations %s do not match:@ @]@ %a%t"
        (Ident.name id)
        (Includecore.report_mode_sub_error "first is" "second is") e
        Printtyp.Conflicts.print_explanations

let missing_field ppf item =
  let id, loc, kind =  Includemod.item_ident_name item in
  Format.fprintf ppf "The %s %a is required but not provided%a"
    (Includemod.kind_of_field_desc kind)
    (Style.as_inline_code Printtyp.ident) id
    (show_loc "Expected declaration") loc

let module_types {Err.got=mty1; expected=mty2; modes; symptom}=
  let is_modal = Is_modal.module_type_symptom symptom in
  let mode1, mode2 = maybe_print_modes ~in_structure:false ~is_modal modes in
  Format.dprintf
    "@[<hv 2>Modules do not match:@ \
     %a%t@;<1 -2>is not included in@ %a%t@]"
    !Oprint.out_module_type (Printtyp.tree_of_modtype ~abbrev:true mty1)
    mode1
    !Oprint.out_module_type (Printtyp.tree_of_modtype ~abbrev:true mty2)
    mode2

let eq_module_types ({Err.got=mty1; expected=mty2} : _ mdiff) =
  Format.dprintf
    "@[<hv 2>Module types do not match:@ \
     %a@;<1 -2>is not equal to@ %a@]"
    !Oprint.out_module_type (Printtyp.tree_of_modtype ~abbrev:true mty1)
    !Oprint.out_module_type (Printtyp.tree_of_modtype ~abbrev:true mty2)

let module_type_declarations id {Err.got=d1 ; expected=d2} =
  Format.dprintf
    "@[<hv 2>Module type declarations do not match:@ \
     %a@;<1 -2>does not match@ %a@]"
    !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration ~abbrev:true id d1)
    !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration ~abbrev:true id d2)

let interface_mismatch ppf (diff: _ Err.diff) =
  Format.fprintf ppf
    "The implementation %a@ does not match the interface %a:@ "
    Style.inline_code diff.got Style.inline_code diff.expected

let parameter_mismatch ppf (diff: _ Err.diff) =
  Format.fprintf ppf
    "The argument module %s@ does not match the parameter signature %s:@ "
    diff.got diff.expected

let compilation_unit_mismatch comparison ppf diff =
  match (comparison : Err.compilation_unit_comparison) with
  | Implementation_vs_interface -> interface_mismatch ppf diff
  | Argument_vs_parameter -> parameter_mismatch ppf diff

let core_module_type_symptom (x:Err.core_module_type_symptom)  =
  match x with
  | Not_an_alias | Not_an_identifier | Abstract_module_type
  | Incompatible_aliases ->
      if Printtyp.Conflicts.exists () then
        Some Printtyp.Conflicts.print_explanations
      else None
  | Unbound_module_path path ->
      Some(Format.dprintf "Unbound module %a"
             (Style.as_inline_code Printtyp.path) path
          )

(* Construct a linearized error message from the error tree *)

let rec module_type ~expansion_token ~eqmode ~env ~before ~ctx
  (diff : _ mdiff) =
  match diff.symptom with
  | Invalid_module_alias _ (* the difference is non-informative here *)
  | After_alias_expansion _ (* we print only the expanded module types *) ->
      module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx
        diff.symptom
  | Functor Params d -> (* We jump directly to the functor param error *)
      functor_params ~expansion_token ~env ~before ~ctx d
  | Mode _ -> (* jump directly to the module mode mismatch, without types *)
      module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx
        diff.symptom
  | _ ->
      let inner = if eqmode then eq_module_types else module_types in
      let next =
        match diff.symptom with
        | Mt_core _ ->
            (* In those cases, the refined error messages for the current error
               will at most add some minor comments on the current error.
               It is thus better to avoid eliding the current error message.
            *)
            dwith_context ctx (inner diff)
        | _ -> dwith_context_and_elision ctx (inner diff)
      in
      let before = next :: before in
      module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx
        diff.symptom

and module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx = function
  | Mt_core core ->
      begin match core_module_type_symptom core with
      | None -> before
      | Some msg -> Location.msg "%t" msg :: before
      end
  | Signature s -> signature ~expansion_token ~env ~before ~ctx s
  | Functor f -> functor_symptom ~expansion_token ~env ~before ~ctx f
  | After_alias_expansion diff ->
      module_type ~eqmode ~expansion_token ~env ~before ~ctx diff
  | Invalid_module_alias path ->
      let printer =
        Format.dprintf "Module %a cannot be aliased"
          (Style.as_inline_code Printtyp.path) path
      in
      dwith_context ctx printer :: before
  | Mode e ->
      let printer ppf =
        Includecore.report_mode_sub_error "Got" "expected" ppf e
      in
      dwith_context ctx printer :: before

and functor_params ~expansion_token ~env ~before ~ctx {got;expected;_} =
  let d = Functor_suberror.Inclusion.patch env got expected in
  let actual = Functor_suberror.Inclusion.got d in
  let expected = Functor_suberror.expected d in
  let main =
    Format.dprintf
      "@[<hv 2>Modules do not match:@ \
       @[functor@ %t@ -> ...@]@;<1 -2>is not included in@ \
       @[functor@ %t@ -> ...@]@]"
      actual expected
  in
  let msgs = dwith_context ctx main :: before in
  let functor_suberrors =
    if expansion_token then
      Functor_suberror.params functor_arg_diff ~expansion_token env d
    else []
  in
  functor_suberrors @ msgs

and functor_symptom ~expansion_token ~env ~before ~ctx = function
  | Result res ->
      module_type ~expansion_token ~eqmode:false ~env ~before ~ctx res
  | Params d -> functor_params ~expansion_token ~env ~before ~ctx d

and signature ~expansion_token ~env:_ ~before ~ctx sgs =
  Printtyp.wrap_printing_env ~error:true sgs.env (fun () ->
      match sgs.missings, sgs.incompatibles with
      | _ :: _ as missings, _ ->
          if expansion_token then
            let init_missings, last_missing = Misc.split_last missings in
            List.map (Location.msg "%a" missing_field) init_missings
            @ [ with_context ctx missing_field last_missing ]
            @ before
          else
            before
      | [], a :: _ -> sigitem ~expansion_token ~env:sgs.env ~before ~ctx a
      | [], [] -> assert false
    )
and sigitem ~expansion_token ~env ~before ~ctx (name,s) = match s with
  | Core c ->
      dwith_context ctx (core env name c) :: before
  | Module_type diff ->
      module_type ~expansion_token ~eqmode:false ~env ~before
        ~ctx:(Context.Module name :: ctx) diff
  | Module_type_declaration diff ->
      module_type_decl ~expansion_token ~env ~before ~ctx name diff
and module_type_decl ~expansion_token ~env ~before ~ctx id diff =
  let next =
    dwith_context_and_elision ctx (module_type_declarations id diff) in
  let before = next :: before in
  match diff.symptom with
  | Not_less_than mts ->
      let before =
        Location.msg "The first module type is not included in the second"
        :: before
      in
      module_type ~expansion_token ~eqmode:true ~before ~env
        ~ctx:(Context.Modtype id :: ctx) mts
  | Not_greater_than mts ->
      let before =
        Location.msg "The second module type is not included in the first"
        :: before in
      module_type ~expansion_token ~eqmode:true ~before ~env
        ~ctx:(Context.Modtype id :: ctx) mts
  | Incomparable mts ->
      module_type ~expansion_token ~eqmode:true ~env ~before
        ~ctx:(Context.Modtype id :: ctx) mts.less_than
  | Illegal_permutation c ->
      begin match diff.got.Types.mtd_type with
      | None -> assert false
      | Some mty ->
          with_context (Modtype id::ctx)
            (Illegal_permutation.pp Context.alt_pp env) (mty,c)
          :: before
      end

and functor_arg_diff ~expansion_token env (patch: _ Diffing.change) =
  match patch with
  | Insert mty -> Functor_suberror.Inclusion.insert mty
  | Delete mty -> Functor_suberror.Inclusion.delete mty
  | Keep (x, y, _) ->  Functor_suberror.Inclusion.ok x y
  | Change (_, _, Err.Incompatible_params (i,_)) ->
      Functor_suberror.Inclusion.incompatible i
  | Change (g, e,  Err.Mismatch mty_diff) ->
      let more () =
        subcase_list @@
        module_type_symptom ~eqmode:false ~expansion_token ~env ~before:[]
          ~ctx:[] mty_diff.symptom
      in
      Functor_suberror.Inclusion.diff g e more

let functor_app_diff ~expansion_token env  (patch: _ Diffing.change) =
  match patch with
  | Insert mty ->  Functor_suberror.App.insert mty
  | Delete mty ->  Functor_suberror.App.delete mty
  | Keep (x, y, _) ->  Functor_suberror.App.ok x y
  | Change (_, _, Err.Incompatible_params (i,_)) ->
      Functor_suberror.App.incompatible i
  | Change (g, e,  Err.Mismatch mty_diff) ->
      let more () =
        subcase_list @@
        module_type_symptom ~eqmode:false ~expansion_token ~env ~before:[]
          ~ctx:[] mty_diff.symptom
      in
      let is_modal = Is_modal.module_type_symptom mty_diff.symptom in
      Functor_suberror.App.diff ~is_modal g e more

let module_type_subst ~env id diff =
  match diff.symptom with
  | Not_less_than mts ->
      module_type ~expansion_token:true ~eqmode:true ~before:[] ~env
        ~ctx:[Modtype id] mts
  | Not_greater_than mts ->
      module_type ~expansion_token:true ~eqmode:true ~before:[] ~env
        ~ctx:[Modtype id] mts
  | Incomparable mts ->
      module_type ~expansion_token:true ~eqmode:true ~env ~before:[]
        ~ctx:[Modtype id] mts.less_than
  | Illegal_permutation c ->
      let mty = diff.got in
      let main =
        with_context [Modtype id]
          (Illegal_permutation.pp Context.alt_pp env) (mty,c) in
      [main]

let all env = function
  | In_Compilation_unit (comparison, diff) ->
      let first =
        Location.msg "%a" (compilation_unit_mismatch comparison) diff
      in
      signature ~expansion_token:true ~env ~before:[first] ~ctx:[] diff.symptom
  | In_Type_declaration (id,reason) ->
      [Location.msg "%t" (core env id reason)]
  | In_Module_type diff ->
      module_type ~expansion_token:true ~eqmode:false ~before:[] ~env ~ctx:[]
        diff
  | In_Module_type_substitution (id,diff) ->
      module_type_subst ~env id diff
  | In_Signature diff ->
      signature ~expansion_token:true ~before:[] ~env ~ctx:[] diff
  | In_Include_functor_signature diff ->
      signature ~expansion_token:true ~before:[] ~env ~ctx:[] diff
  | In_Expansion cmts ->
      match core_module_type_symptom cmts with
      | None -> assert false
      | Some main -> [Location.msg "%t" main]

(* General error reporting *)

let err_msgs (env, err) =
  Printtyp.Conflicts.reset();
  Printtyp.wrap_printing_env ~error:true env
    (fun () -> coalesce @@ all env err)

let report_error err =
  let main = err_msgs err in
  Location.errorf ~loc:Location.(in_file !input_name) "%t" main

let report_apply_error ~loc env (app_name, mty_f, args) =
  let d = Functor_suberror.App.patch env ~f:mty_f ~args in
  match d with
  (* We specialize the one change and one argument case to remove the
     presentation of the functor arguments *)
  | [ _,  Change (_, _, Err.Incompatible_params (i,_)) ] ->
      Location.errorf ~loc "%t" (Functor_suberror.App.incompatible i)
  | [ _, Change (g, e,  Err.Mismatch mty_diff) ] ->
      let more () =
        subcase_list @@
        module_type_symptom ~eqmode:false ~expansion_token:true ~env ~before:[]
          ~ctx:[] mty_diff.symptom
      in
      let is_modal = Is_modal.module_type_symptom mty_diff.symptom in
      Location.errorf ~loc "%t"
        (Functor_suberror.App.single_diff ~is_modal g e more)
  | _ ->
      let not_functor =
        List.for_all (function _, Diffing.Delete _ -> true | _ -> false) d
      in
      if not_functor then
        match app_name with
        | Includemod.Named_leftmost_functor lid ->
            Location.errorf ~loc
              "@[The module %a is not a functor, it cannot be applied.@]"
               (Style.as_inline_code Printtyp.longident)  lid
        | Includemod.Anonymous_functor
        | Includemod.Full_application_path _
          (* The "non-functor application in term" case is directly handled in
             [Env] and it is the only case where we have a full application
             path at hand. Thus this case of the or-pattern is currently
             unreachable and we don't try to specialize the corresponding error
             message. *) ->
            Location.errorf ~loc
              "@[This module is not a functor, it cannot be applied.@]"
      else
        let intro ppf =
          match app_name with
          | Includemod.Anonymous_functor ->
              Format.fprintf ppf "This functor application is ill-typed."
          | Includemod.Full_application_path lid ->
              Format.fprintf ppf "The functor application %a is ill-typed."
                (Style.as_inline_code Printtyp.longident) lid
          |  Includemod.Named_leftmost_functor lid ->
              Format.fprintf ppf
                "This application of the functor %a is ill-typed."
                 (Style.as_inline_code Printtyp.longident) lid
        in
        let actual = Functor_suberror.App.got ~is_modal:None d in
        let expected = Functor_suberror.expected d in
        let sub =
          List.rev @@
          Functor_suberror.params functor_app_diff env ~expansion_token:true d
        in
        Location.errorf ~loc ~sub
          "@[<hv>%t@ \
           These arguments:@;<1 2>@[%t@]@ \
           do not match these parameters:@;<1 2>@[functor@ %t@ -> ...@]@]"
          intro
          actual expected

let register () =
  Location.register_error_of_exn
    (function
      | Includemod.Error err -> Some (report_error err)
      | Includemod.Apply_error {loc; env; app_name; mty_f; args} ->
          Some (Printtyp.wrap_printing_env env ~error:true (fun () ->
              report_apply_error ~loc env (app_name, mty_f, args))
            )
      | _ -> None
    )
