(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(* This file contains tests for the layout float64, not tests for the type float# *)

type t_float64 [@@float64]


(*********************************)
(* Test 1: The identity function *)

let f1 (x : t_float64) = x;;
[%%expect{|
type t_float64 [@@float64]
val f1 : t_float64 -> t_float64 = <fun>
|}];;

(*****************************************)
(* Test 2: You can let-bind them locally *)
let f2 (x : t_float64) =
  let y = x in
  y;;
[%%expect{|
val f2 : t_float64 -> t_float64 = <fun>
|}];;

(**************************************)
(* Test 3: No top-level bindings yet. *)

let x3 : t_float64 = assert false;;
[%%expect{|
Line 1, characters 4-6:
1 | let x3 : t_float64 = assert false;;
        ^^
Error: Top-level module bindings must have layout value, but x3 has layout
       float64.
|}];;

(*************************************)
(* Test 4: No putting them in tuples *)

let f4 (x : t_float64) = x, false;;
[%%expect{|
Line 1, characters 25-26:
1 | let f4 (x : t_float64) = x, false;;
                             ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       t_float64 has layout float64, which is not a sublayout of value.
|}];;

(****************************************************)
(* Test 5: Can't be put in structures in typedecls. *)

type t5_1 = { x : t_float64 };;
[%%expect{|
Line 1, characters 14-27:
1 | type t5_1 = { x : t_float64 };;
                  ^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_2 = { y : int; x : t_float64 };;
[%%expect{|
Line 1, characters 23-36:
1 | type t5_2 = { y : int; x : t_float64 };;
                           ^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_3 = { x : t_float64 } [@@unboxed];;
[%%expect{|
Line 1, characters 14-27:
1 | type t5_3 = { x : t_float64 } [@@unboxed];;
                  ^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_4 = A of t_float64;;
[%%expect{|
Line 1, characters 12-26:
1 | type t5_4 = A of t_float64;;
                ^^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_5 = A of int * t_float64;;
[%%expect{|
Line 1, characters 12-32:
1 | type t5_5 = A of int * t_float64;;
                ^^^^^^^^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_6 = A of t_float64 [@@unboxed];;
[%%expect{|
Line 1, characters 12-26:
1 | type t5_6 = A of t_float64 [@@unboxed];;
                ^^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

(****************************************************)
(* Test 6: Can't be put at top level of signatures. *)
module type S6 = sig val x : t_float64 end

let f6 (m : (module S6)) = let module M6 = (val m) in M6.x;;
[%%expect{|
Line 1, characters 29-38:
1 | module type S6 = sig val x : t_float64 end
                                 ^^^^^^^^^
Error: This type signature for x is not a value type.
       x has layout float64, which is not a sublayout of value.
|}];;

(*********************************************************)
(* Test 7: Can't be used as polymorphic variant argument *)
let f7 (x : t_float64) = `A x;;
[%%expect{|
Line 1, characters 28-29:
1 | let f7 (x : t_float64) = `A x;;
                                ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       t_float64 has layout float64, which is not a sublayout of value.
|}];;
