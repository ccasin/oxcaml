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

let is_inline_attribute =
  [ ["inline"; "ocaml.inline"],true ]

let is_inlined_attribute =
  [ ["inlined"; "ocaml.inlined"], true
  ; ["unrolled"; "ocaml.unrolled"], (Config.flambda || Config.flambda2)
  ]

let is_specialise_attribute =
  [ ["specialise"; "ocaml.specialise"], Config.flambda ]

let is_specialised_attribute =
  [ ["specialised"; "ocaml.specialised"], Config.flambda ]

let is_local_attribute =
  [ ["local"; "ocaml.local"], true ]

let is_tailcall_attribute =
  [ ["tailcall"; "ocaml.tailcall"], true ]

let is_property_attribute = function
  | Zero_alloc -> [ ["zero_alloc"; "ocaml.zero_alloc"], true ]

let is_tmc_attribute =
  [ ["tail_mod_cons"; "ocaml.tail_mod_cons"], true ]

let is_poll_attribute =
  [ ["poll"; "ocaml.poll"], true ]

let is_loop_attribute =
  [ ["loop"; "ocaml.loop"], true ]

let is_opaque_attribute =
  [ ["opaque"; "ocaml.opaque"], true ]


let find_attribute p attributes =
  let inline_attribute =
    Builtin_attributes.filter_attributes
      (Builtin_attributes.Attributes_filter.create p)
      attributes
  in
  let attr =
    match inline_attribute with
    | [] -> None
    | [attr] -> Some attr
    | attr :: {Parsetree.attr_name = {txt;loc}; _} :: _ ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
      Some attr
  in
  attr

let is_unrolled = function
  | {txt="unrolled"|"ocaml.unrolled"} -> true
  | {txt="inline"|"ocaml.inline"|"inlined"|"ocaml.inlined"} -> false
  | _ -> assert false

let get_payload get_from_exp =
  let open Parsetree in
  function
  | PStr [{pstr_desc = Pstr_eval (exp, [])}] -> get_from_exp exp
  | _ -> Result.Error ()

let get_optional_payload get_from_exp =
  let open Parsetree in
  function
  | PStr [] -> Result.Ok None
  | other -> Result.map Option.some (get_payload get_from_exp other)

let get_id_from_exp =
  let open Parsetree in
  function
  | { pexp_desc = Pexp_ident { txt = Longident.Lident id } } -> Result.Ok id
  | _ -> Result.Error ()

let get_int_from_exp =
  let open Parsetree in
  function
    | { pexp_desc = Pexp_constant (Pconst_integer(s, None)) } ->
        begin match Misc.Int_literal_converter.int s with
        | n -> Result.Ok n
        | exception (Failure _) -> Result.Error ()
        end
    | _ -> Result.Error ()

let get_construct_from_exp =
  let open Parsetree in
  function
    | { pexp_desc =
          Pexp_construct ({ txt = Longident.Lident constr }, None) } ->
        Result.Ok constr
    | _ -> Result.Error ()

let get_bool_from_exp exp =
  Result.bind (get_construct_from_exp exp)
    (function
      | "true" -> Result.Ok true
      | "false" -> Result.Ok false
      | _ -> Result.Error ())

let get_ids_from_exp exp =
  let open Parsetree in
  (match exp with
   | { pexp_desc = Pexp_apply (exp, args) } ->
     get_id_from_exp exp ::
     List.map (function
       | (Asttypes.Nolabel, arg) -> get_id_from_exp arg
       | (_, _) -> Result.Error ())
       args
   | _ -> [get_id_from_exp exp])
  |> List.fold_left (fun acc r ->
    match acc, r with
    | Result.Ok ids, Ok id -> Result.Ok (id::ids)
    | (Result.Error _ | Ok _), _ -> Result.Error ())
    (Ok [])
  |> Result.map List.rev


let parse_ids_payload txt loc ~default ~empty cases payload =
  let[@local] warn () =
    let ( %> ) f g x = g (f x) in
    let msg =
      cases
      |> List.map (fst %> String.concat " " %> Printf.sprintf "'%s'")
      |> String.concat ", "
      |> Printf.sprintf "It must be either %s or empty"
    in
    Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
    default
  in
  match get_optional_payload get_ids_from_exp payload with
  | Error () -> warn ()
  | Ok None -> empty
  | Ok (Some ids) ->
      match List.assoc_opt (List.sort String.compare ids) cases with
      | Some r -> r
      | None -> warn ()

let parse_id_payload txt loc ~default ~empty cases payload =
  let[@local] warn () =
    let ( %> ) f g x = g (f x) in
    let msg =
      cases
      |> List.map (fst %> Printf.sprintf "'%s'")
      |> String.concat ", "
      |> Printf.sprintf "It must be either %s or empty"
    in
    Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
    default
  in
  match get_optional_payload get_id_from_exp payload with
  | Error () -> warn ()
  | Ok None -> empty
  | Ok (Some id) ->
      match List.assoc_opt id cases with
      | Some r -> r
      | None -> warn ()

let parse_inline_attribute attr : inline_attribute =
  match attr with
  | None -> Default_inline
  | Some {Parsetree.attr_name = {txt;loc} as id; attr_payload = payload} ->
    if is_unrolled id then begin
      (* the 'unrolled' attributes must be used as [@unrolled n]. *)
      let warning txt = Warnings.Attribute_payload
          (txt, "It must be an integer literal")
      in
      match get_payload get_int_from_exp payload with
      | Ok n -> Unroll n
      | Error () ->
        Location.prerr_warning loc (warning txt);
        Default_inline
    end else
      parse_id_payload txt loc
        ~default:Default_inline
        ~empty:Always_inline
        [
          "never", Never_inline;
          "always", Always_inline;
          "available", Available_inline;
        ]
        payload

let parse_inlined_attribute attr : inlined_attribute =
  match attr with
  | None -> Default_inlined
  | Some {Parsetree.attr_name = {txt;loc} as id; attr_payload = payload} ->
    if is_unrolled id then begin
      (* the 'unrolled' attributes must be used as [@unrolled n]. *)
      let warning txt = Warnings.Attribute_payload
          (txt, "It must be an integer literal")
      in
      match get_payload get_int_from_exp payload with
      | Ok n -> Unroll n
      | Error () ->
        Location.prerr_warning loc (warning txt);
        Default_inlined
    end else
      parse_id_payload txt loc
        ~default:Default_inlined
        ~empty:Always_inlined
        [
          "never", Never_inlined;
          "always", Always_inlined;
          "hint", Hint_inlined;
        ]
        payload

let parse_specialise_attribute attr =
  match attr with
  | None -> Default_specialise
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_specialise
        ~empty:Always_specialise
        [
          "never", Never_specialise;
          "always", Always_specialise;
        ]
        payload

let parse_local_attribute attr =
  match attr with
  | None -> Default_local
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_local
        ~empty:Always_local
        [
          "never", Never_local;
          "always", Always_local;
          "maybe", Default_local;
        ]
        payload

let parse_property_attribute attr property =
  match attr with
  | None -> Default_check
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload}->
      parse_ids_payload txt loc
        ~default:Default_check
        ~empty:(Check { property; strict = false; opt = false; loc; } )
        [
          ["assume"],
          Assume { property; strict = false; never_returns_normally = false; loc; };
          ["strict"], Check { property; strict = true; opt = false; loc; };
          ["opt"], Check { property; strict = false; opt = true; loc; };
          ["opt"; "strict"; ], Check { property; strict = true; opt = true; loc; };
          ["assume"; "strict"],
          Assume { property; strict = true; never_returns_normally = false; loc; };
          ["assume"; "never_returns_normally"],
          Assume { property; strict = false; never_returns_normally = true; loc; };
          ["assume"; "strict"; "never_returns_normally"],
          Assume { property; strict = true; never_returns_normally = true; loc; };
          ["ignore"], Ignore_assert_all property
        ]
        payload

let parse_poll_attribute attr =
  match attr with
  | None -> Default_poll
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_poll
        ~empty:Default_poll
        [
          "error", Error_poll;
        ]
        payload

let parse_loop_attribute attr =
  match attr with
  | None -> Default_loop
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_loop
        ~empty:Always_loop
        [
          "never", Never_loop;
          "always", Always_loop;
        ]
        payload

let parse_opaque_attribute attr =
  match attr with
  | None -> false
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:false
        ~empty:true
        []
        payload

let get_inline_attribute l =
  let attr = find_attribute is_inline_attribute l in
  parse_inline_attribute attr

let get_specialise_attribute l =
  let attr = find_attribute is_specialise_attribute l in
  parse_specialise_attribute attr

let get_local_attribute l =
  let attr = find_attribute is_local_attribute l in
  parse_local_attribute attr

let get_opaque_attribute l =
  let attr = find_attribute is_opaque_attribute l in
  parse_opaque_attribute attr


let get_property_attribute l p =
  let attr = find_attribute (is_property_attribute p) l in
  let res = parse_property_attribute attr p in
  (match attr, res with
   | None, Default_check -> ()
   | _, Default_check -> ()
   | None, (Check _ | Assume _ | Ignore_assert_all _) -> assert false
   | Some _, Ignore_assert_all _ -> ()
   | Some _, Assume _ -> ()
   | Some attr, Check { opt; _ } ->
     if Lambda.is_check_enabled ~opt p && !Clflags.native_code then
       (* The warning for unchecked functions will not trigger if the check is requested
          through the [@@@zero_alloc all] top-level annotation rather than through the
          function annotation [@zero_alloc]. *)
       Builtin_attributes.register_property attr.attr_name);
   res

let get_poll_attribute l =
  let attr = find_attribute is_poll_attribute l in
  parse_poll_attribute attr

let get_loop_attribute l =
  let attr = find_attribute is_loop_attribute l in
  parse_loop_attribute attr

let get_inlined_attribute_on_module e =
  let rec get mod_expr =
    let attr = find_attribute is_inlined_attribute mod_expr.mod_attributes in
    let attr = parse_inlined_attribute attr in
    let attr =
      match mod_expr.Typedtree.mod_desc with
      | Tmod_constraint (me, _, _, _) ->
        let inner_attr = get me in
        begin match attr with
        | Always_inlined | Hint_inlined | Never_inlined | Unroll _ -> attr
        | Default_inlined -> inner_attr
        end
      | _ -> attr
    in
    attr
  in
  get e

let get_specialised_attribute e =
  let attr = find_attribute is_specialised_attribute e.exp_attributes in
  parse_specialise_attribute attr

let get_tailcall_attribute e =
  let attr = find_attribute is_tailcall_attribute e.exp_attributes in
  match attr with
  | None -> Default_tailcall
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
    match get_optional_payload get_bool_from_exp payload with
    | Ok (None | Some true) -> Tailcall_expectation true
    | Ok (Some false) -> Tailcall_expectation false
    | Error () ->
        let msg = "Only an optional boolean literal is supported." in
        Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
        Default_tailcall

let get_tmc_attribute attributes =
  find_attribute is_tmc_attribute attributes

let assume_zero_alloc attributes =
  let p = Zero_alloc in
  let attr = find_attribute (is_property_attribute p) attributes in
  match parse_property_attribute attr p with
  | Default_check -> false
  | Ignore_assert_all _ -> false
  | Assume { property = Zero_alloc; _ } -> true
  | Check { property = Zero_alloc; _ } -> false

let assume_zero_alloc attributes =
  (* This function is used for "look-ahead" to find attributes
     that affect [Scoped_location] settings before translation
     of expressions in that scope.
     Warnings will be produced by [add_check_attribute]. *)
  Warnings.without_warnings (fun () -> assume_zero_alloc attributes)
(* Get the [@inlined] attribute payload (or default if not present). *)
let get_inlined_attribute e =
  let attr = find_attribute is_inlined_attribute e.exp_attributes in
  parse_inlined_attribute attr

