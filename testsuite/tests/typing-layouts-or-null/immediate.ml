(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

(* Tests for [immediate_or_null]. *)

(* The [immediate_or_null] layout. *)
type t_immediate_or_null : immediate_or_null
[%%expect{|
type t_immediate_or_null : immediate_or_null
|}]

(* The subkind relation. *)

type ('a : immediate_or_null) accept_immediate_or_null
[%%expect{|
type ('a : immediate_or_null) accept_immediate_or_null
|}]

type should_work = t_immediate_or_null accept_immediate_or_null
[%%expect{|
type should_work = t_immediate_or_null accept_immediate_or_null
|}]

type should_work = int accept_immediate_or_null
[%%expect{|
type should_work = int accept_immediate_or_null
|}]

type ('a : immediate) accept_immediate
[%%expect{|
type ('a : immediate) accept_immediate
|}]

type should_fail = t_immediate_or_null accept_immediate
[%%expect{|
Line 1, characters 19-38:
1 | type should_fail = t_immediate_or_null accept_immediate
                       ^^^^^^^^^^^^^^^^^^^
Error: This type "t_immediate_or_null" should be an instance of type
         "('a : immediate)"
       The kind of t_immediate_or_null is immediate_or_null
         because of the definition of t_immediate_or_null at line 1, characters 0-44.
       But the kind of t_immediate_or_null must be a subkind of immediate
         because of the definition of accept_immediate at line 1, characters 0-38.
|}]

type ('a : value) accept_value
[%%expect{|
type 'a accept_value
|}]

type should_fail = t_immediate_or_null accept_value
[%%expect{|
Line 1, characters 19-38:
1 | type should_fail = t_immediate_or_null accept_value
                       ^^^^^^^^^^^^^^^^^^^
Error: This type "t_immediate_or_null" should be an instance of type
         "('a : value)"
       The kind of t_immediate_or_null is immediate_or_null
         because of the definition of t_immediate_or_null at line 1, characters 0-44.
       But the kind of t_immediate_or_null must be a subkind of value
         because of the definition of accept_value at line 1, characters 0-30.
|}]

type ('a : value_or_null) accept_value_or_null

type should_work = t_immediate_or_null accept_value_or_null
[%%expect{|
type 'a accept_value_or_null
type should_work = t_immediate_or_null accept_value_or_null
|}]

(* [int or_null] fits into [immediate_or_null]: *)


type int_or_null : immediate_or_null = int or_null
[%%expect{|
type int_or_null = int or_null
|}]

type should_work = int_or_null accept_immediate_or_null
[%%expect{|
type should_work = int_or_null accept_immediate_or_null
|}]

type should_work = int or_null accept_immediate_or_null
[%%expect{|
type should_work = int or_null accept_immediate_or_null
|}]

(* Values. *)

type ('a : immediate_or_null) myref = { mutable v : 'a }

external read_imm : ('a : immediate_or_null) . 'a myref -> 'a = "%field0"
external write_imm : ('a : immediate_or_null) . 'a myref -> 'a -> unit = "%setfield0"
external equal : ('a : immediate_or_null) . 'a -> 'a -> bool = "%equal"

[%%expect{|
type ('a : immediate_or_null) myref = { mutable v : 'a; }
external read_imm : ('a : immediate_or_null). 'a myref -> 'a = "%field0"
external write_imm : ('a : immediate_or_null). 'a myref -> 'a -> unit
  = "%setfield0"
external equal : ('a : immediate_or_null). 'a -> 'a -> bool = "%equal"
|}]

let () =
  let r = { v = (Null : int or_null) } in
  let x = read_imm r in
  assert (equal x Null);
  write_imm r (This 5);
  assert (equal r.v (This 5))
;;

[%%expect{|
|}]

type ('a : value_or_null mod non_float) accepts_nonfloat

type succeeds = t_immediate_or_null accepts_nonfloat

[%%expect{|
type ('a : value_or_null mod non_float) accepts_nonfloat
type succeeds = t_immediate_or_null accepts_nonfloat
|}]

(* Values of [int or_null] mode-cross: *)

let f (x : int or_null @ local) (g : int or_null -> unit) = g x [@nontail]

[%%expect{|
val f : local_ int or_null -> ((int or_null -> unit) -> unit) = <fun>
|}, Principal{|
val f : local_ int or_null -> (int or_null -> unit) -> unit = <fun>
|}]
