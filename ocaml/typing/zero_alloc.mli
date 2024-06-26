type const = Builtin_attributes.zero_alloc_attribute =
  | Default_zero_alloc
  | Ignore_assert_all
  | Check of { strict: bool;
               opt: bool;
               arity: int;
               loc: Location.t;
             }
  | Assume of { strict: bool;
                never_returns_normally: bool;
                never_raises: bool;
                arity: int;
                loc: Location.t;
              }

type t

val default : t

val create : const -> t
val create_var : unit -> t

val get : t -> const option
val get_defaulting : t -> const

type error

exception Error of error

val print_error : Format.formatter -> error -> unit

type change = t
val set_change_log : (change -> unit) -> unit
val undo_change : change -> unit

(* [sub_exn t1 t2] checks whether the zero_alloc check t1 is stronger than the
   zero_alloc check t2. If not, it raises [Error]. *)
val sub_exn : t -> t -> unit
