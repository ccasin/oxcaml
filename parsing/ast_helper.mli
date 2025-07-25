(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers to produce Parsetree fragments

  {b Warning} This module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Asttypes
open Docstrings
open Parsetree

type 'a with_loc = 'a Location.loc
type loc = Location.t

type lid = Longident.t with_loc
type str = string with_loc
type str_opt = string option with_loc
type attrs = attribute list

(** {1 Default locations} *)

val default_loc: loc ref
    (** Default value for all optional location arguments. *)

val with_default_loc: loc -> (unit -> 'a) -> 'a
    (** Set the [default_loc] within the scope of the execution
        of the provided function. *)

(** {1 Constants} *)

module Const : sig
  val char : char -> constant
  val string :
    ?quotation_delimiter:string -> ?loc:Location.t -> string -> constant
  val integer : ?suffix:char -> string -> constant
  val int : ?suffix:char -> int -> constant
  val int32 : ?suffix:char -> int32 -> constant
  val int64 : ?suffix:char -> int64 -> constant
  val nativeint : ?suffix:char -> nativeint -> constant
  val float : ?suffix:char -> string -> constant
end

(** {1 Attributes} *)
module Attr : sig
  val mk: ?loc:loc -> str -> payload -> attribute
end

(** {1 Core language} *)

(** Type expressions *)
module Typ :
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
    val attr: core_type -> attribute -> core_type

    val any: ?loc:loc -> ?attrs:attrs -> jkind_annotation option -> core_type
    val var: ?loc:loc -> ?attrs:attrs -> string -> jkind_annotation option
      -> core_type
    val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type -> core_type ->
      mode with_loc list -> mode with_loc list -> core_type
    val tuple: ?loc:loc -> ?attrs:attrs -> (string option * core_type) list -> core_type
    val unboxed_tuple: ?loc:loc -> ?attrs:attrs
                       -> (string option * core_type) list -> core_type
    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val object_: ?loc:loc -> ?attrs:attrs -> object_field list
                   -> closed_flag -> core_type
    val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string with_loc option
               -> jkind_annotation option -> core_type
    (* Invariant: One of the options must be [Some]. *)
    val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
                 -> label list option -> core_type
    val poly: ?loc:loc -> ?attrs:attrs ->
      (str * jkind_annotation option) list -> core_type -> core_type
    val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
                 -> core_type
    val open_ : ?loc:loc -> ?attrs:attrs -> lid -> core_type -> core_type
    val of_kind : ?loc:loc -> ?attrs:attrs -> jkind_annotation -> core_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

    val force_poly: core_type -> core_type

    val varify_constructors: str list -> core_type -> core_type
    (** [varify_constructors newtypes te] is type expression [te], of which
        any of nullary type constructor [tc] is replaced by type variable of
        the same name, if [tc]'s name appears in [newtypes].
        Raise [Syntaxerr.Variable_in_scope] if any type variable inside [te]
        appears in [newtypes]. Used to translate [type a. a -> a] to
        ['a. 'a -> 'a] during parsing.
        @since 4.05
     *)
  end

(** Patterns *)
module Pat:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
    val attr:pattern -> attribute -> pattern

    val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
    val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
    val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
    val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
    val tuple: ?loc:loc -> ?attrs:attrs -> (string option * pattern) list ->
      closed_flag -> pattern
    val unboxed_tuple: ?loc:loc -> ?attrs:attrs
                       -> (string option * pattern) list -> closed_flag
                       -> pattern
    val construct: ?loc:loc -> ?attrs:attrs ->
      lid -> ((str * jkind_annotation option) list * pattern) option -> pattern
    val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
    val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
                -> pattern
    val record_unboxed_product: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list
                -> closed_flag -> pattern
    val array: ?loc:loc -> ?attrs:attrs -> mutable_flag -> pattern list ->
      pattern
    val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
    val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type option
                     -> mode with_loc list -> pattern
    val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
    val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val unpack: ?loc:loc -> ?attrs:attrs -> str_opt -> pattern
    val open_: ?loc:loc -> ?attrs:attrs  -> lid -> pattern -> pattern
    val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
  end

(** Expressions *)
module Exp:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
    val attr: expression -> attribute -> expression

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
    val let_: ?loc:loc -> ?attrs:attrs -> mutable_flag -> rec_flag ->
              value_binding list -> expression -> expression
    val function_ : ?loc:loc -> ?attrs:attrs -> function_param list
                   -> function_constraint -> function_body
                   -> expression
    val apply: ?loc:loc -> ?attrs:attrs -> expression
               -> (arg_label * expression) list -> expression
    val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
                -> expression
    val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
    val tuple: ?loc:loc -> ?attrs:attrs -> (string option * expression) list -> expression
    val unboxed_tuple: ?loc:loc -> ?attrs:attrs
                       -> (string option * expression) list -> expression
    val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
                   -> expression
    val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
                 -> expression
    val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
                -> expression option -> expression
    val record_unboxed_product: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
                -> expression option -> expression
    val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
    val unboxed_field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
    val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
                  -> expression
    val array: ?loc:loc -> ?attrs:attrs -> mutable_flag -> expression list ->
      expression
    val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
                    -> expression option -> expression
    val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
                  -> expression
    val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
                -> expression
    val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
              -> direction_flag -> expression -> expression
    val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                -> core_type -> expression
    val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                     -> mode with_loc list -> expression
    val send: ?loc:loc -> ?attrs:attrs -> expression -> str -> expression
    val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
    val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
                  -> expression
    val letmodule: ?loc:loc -> ?attrs:attrs -> str_opt -> module_expr
                   -> expression -> expression
    val letexception:
      ?loc:loc -> ?attrs:attrs -> extension_constructor -> expression
      -> expression
    val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
              -> expression
    val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
    val newtype: ?loc:loc -> ?attrs:attrs -> str -> jkind_annotation option ->
      expression  -> expression
    val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
    val open_: ?loc:loc -> ?attrs:attrs -> open_declaration -> expression
               -> expression
    val letop: ?loc:loc -> ?attrs:attrs -> binding_op
               -> binding_op list -> expression -> expression
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression
    val unreachable: ?loc:loc -> ?attrs:attrs -> unit -> expression
    val stack : ?loc:loc -> ?attrs:attrs -> expression -> expression
    val comprehension :
      ?loc:loc -> ?attrs:attrs -> comprehension_expression -> expression
    val overwrite : ?loc:loc -> ?attrs:attrs -> expression -> expression -> expression
    val hole : ?loc:loc -> ?attrs:attrs -> unit -> expression

    val case: pattern -> ?guard:expression -> expression -> case
    val binding_op: str -> pattern -> expression -> loc -> binding_op
  end

(** Value declarations *)
module Val:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?prim:string list ->
      ?modalities:modality with_loc list -> str -> core_type -> value_description
  end

(** Type declarations *)
module Type:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?params:(core_type * (variance * injectivity)) list ->
      ?cstrs:(core_type * core_type * loc) list ->
      ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type ->
      ?jkind_annotation:jkind_annotation ->
      str ->
      type_declaration

    val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?vars:(str * jkind_annotation option) list ->
      ?args:constructor_arguments -> ?res:core_type ->
      str ->
      constructor_declaration

    val constructor_arg: ?loc:loc -> ?modalities:modality with_loc list -> core_type ->
      constructor_argument

    val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?mut:mutable_flag -> ?modalities:modality with_loc list -> str -> core_type ->
      label_declaration
  end

(** Type extensions *)
module Te:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?params:(core_type * (variance * injectivity)) list ->
      ?priv:private_flag -> lid -> extension_constructor list -> type_extension

    val mk_exception: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      extension_constructor -> type_exception

    val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> extension_constructor_kind -> extension_constructor

    val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      ?vars:(str * jkind_annotation option) list ->
      ?args:constructor_arguments -> ?res:core_type ->
      str ->
      extension_constructor
    val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> lid -> extension_constructor
  end

(** {1 Module language} *)

(** Module type expressions *)
module Mty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
    val attr: module_type -> attribute -> module_type

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
    val functor_: ?loc:loc -> ?attrs:attrs -> ?ret_mode:modes ->
      functor_parameter -> module_type -> module_type
    val with_: ?loc:loc -> ?attrs:attrs -> module_type ->
      with_constraint list -> module_type
    val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
    val strengthen: ?loc:loc -> ?attrs:attrs -> module_type -> lid ->
      module_type
  end

(** Module expressions *)
module Mod:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
    val attr: module_expr -> attribute -> module_expr

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
    val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
    val functor_: ?loc:loc -> ?attrs:attrs ->
      functor_parameter -> module_expr -> module_expr
    val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr ->
      module_expr
    val apply_unit: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> module_type option -> modes ->
      module_expr -> module_expr
    val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
    val instance: ?loc:loc -> ?attrs:attrs -> module_instance -> module_expr
  end

(** Signature items *)
module Sig:
  sig
    val mk: ?loc:loc -> signature_item_desc -> signature_item

    val value: ?loc:loc -> value_description -> signature_item
    val type_: ?loc:loc -> rec_flag -> type_declaration list -> signature_item
    val type_subst: ?loc:loc -> type_declaration list -> signature_item
    val type_extension: ?loc:loc -> type_extension -> signature_item
    val exception_: ?loc:loc -> type_exception -> signature_item
    val module_: ?loc:loc -> module_declaration -> signature_item
    val mod_subst: ?loc:loc -> module_substitution -> signature_item
    val rec_module: ?loc:loc -> module_declaration list -> signature_item
    val modtype: ?loc:loc -> module_type_declaration -> signature_item
    val modtype_subst: ?loc:loc -> module_type_declaration -> signature_item
    val open_: ?loc:loc -> open_description -> signature_item
    val include_: ?loc:loc -> ?modalities:modalities -> include_description ->
      signature_item
    val class_: ?loc:loc -> class_description list -> signature_item
    val class_type: ?loc:loc -> class_type_declaration list -> signature_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
    val attribute: ?loc:loc -> attribute -> signature_item
    val kind_abbrev: ?loc:loc -> label with_loc -> jkind_annotation ->
      signature_item
    val text: text -> signature_item list
  end

module Sg:
  sig
    val mk : ?loc:loc -> ?modalities:modality with_loc list ->
      signature_item list -> signature
  end

(** Structure items *)
module Str:
  sig
    val mk: ?loc:loc -> structure_item_desc -> structure_item

    val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
    val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
    val primitive: ?loc:loc -> value_description -> structure_item
    val type_: ?loc:loc -> rec_flag -> type_declaration list -> structure_item
    val type_extension: ?loc:loc -> type_extension -> structure_item
    val exception_: ?loc:loc -> type_exception -> structure_item
    val module_: ?loc:loc -> module_binding -> structure_item
    val rec_module: ?loc:loc -> module_binding list -> structure_item
    val modtype: ?loc:loc -> module_type_declaration -> structure_item
    val open_: ?loc:loc -> open_declaration -> structure_item
    val class_: ?loc:loc -> class_declaration list -> structure_item
    val class_type: ?loc:loc -> class_type_declaration list -> structure_item
    val include_: ?loc:loc -> include_declaration -> structure_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
    val kind_abbrev: ?loc:loc -> label with_loc -> jkind_annotation ->
      structure_item
    val attribute: ?loc:loc -> attribute -> structure_item
    val text: text -> structure_item list
  end

(** Module declarations *)
module Md:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text -> ?modalities:modalities ->
      str_opt -> module_type -> module_declaration
  end

(** Module substitutions *)
module Ms:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str -> lid -> module_substitution
  end

(** Module type declarations *)
module Mtd:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?typ:module_type -> str -> module_type_declaration
  end

(** Module bindings *)
module Mb:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str_opt -> module_expr -> module_binding
  end

(** Opens *)
module Opn:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
      ?override:override_flag -> 'a -> 'a open_infos
  end

(** Includes *)
module Incl:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?kind:include_kind -> 'a
      -> 'a include_infos
  end

(** Value bindings *)
module Vb:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?value_constraint:value_constraint -> ?modes:mode with_loc list -> pattern ->
      expression -> value_binding
  end


(** {1 Class language} *)

(** Class type expressions *)
module Cty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
    val attr: class_type -> attribute -> class_type

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
    val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
    val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type ->
      class_type -> class_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
    val open_: ?loc:loc -> ?attrs:attrs -> open_description -> class_type
               -> class_type
  end

(** Class type fields *)
module Ctf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      class_type_field_desc -> class_type_field
    val attr: class_type_field -> attribute -> class_type_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
    val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
      virtual_flag -> core_type -> class_type_field
    val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
      virtual_flag -> core_type -> class_type_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
      class_type_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
    val attribute: ?loc:loc -> attribute -> class_type_field
    val text: text -> class_type_field list
  end

(** Class expressions *)
module Cl:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
    val attr: class_expr -> attribute -> class_expr

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
    val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
    val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option ->
      pattern -> class_expr -> class_expr
    val apply: ?loc:loc -> ?attrs:attrs -> class_expr ->
      (arg_label * expression) list -> class_expr
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list ->
      class_expr -> class_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type ->
      class_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
    val open_: ?loc:loc -> ?attrs:attrs -> open_description -> class_expr
               -> class_expr
  end

(** Class fields *)
module Cf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc ->
      class_field
    val attr: class_field -> attribute -> class_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr ->
      str option -> class_field
    val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
      class_field_kind -> class_field
    val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
      class_field_kind -> class_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
      class_field
    val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
    val attribute: ?loc:loc -> attribute -> class_field
    val text: text -> class_field list

    val virtual_: core_type -> class_field_kind
    val concrete: override_flag -> expression -> class_field_kind

  end

(** Classes *)
module Ci:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?virt:virtual_flag ->
      ?params:(core_type * (variance * injectivity)) list ->
      str -> 'a -> 'a class_infos
  end

(** Class signatures *)
module Csig:
  sig
    val mk: core_type -> class_type_field list -> class_signature
  end

(** Class structures *)
module Cstr:
  sig
    val mk: pattern -> class_field list -> class_structure
  end

(** Row fields *)
module Rf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> row_field_desc -> row_field
    val tag: ?loc:loc -> ?attrs:attrs ->
      label with_loc -> bool -> core_type list -> row_field
    val inherit_: ?loc:loc -> core_type -> row_field
  end

(** Object fields *)
module Of:
  sig
    val mk: ?loc:loc -> ?attrs:attrs ->
      object_field_desc -> object_field
    val tag: ?loc:loc -> ?attrs:attrs ->
      label with_loc -> core_type -> object_field
    val inherit_: ?loc:loc -> core_type -> object_field
  end
