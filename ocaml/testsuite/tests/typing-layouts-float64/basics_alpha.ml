(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(* CR layouts v2: To the extent possible, there should be copies of every test
   working on float# that work instead on an abstract float64 type and/or type
   variables of that layout. *)

(*********************************)
(* Test 1: The identity function *)

let f1 (x : float#) = x;;
[%%expect{|
val f1 : float# -> float# = <fun>
|}];;

(*****************************************)
(* Test 2: You can let-bind them locally *)
let f2 (x : float#) =
  let y = x in
  y;;
[%%expect{|
val f2 : float# -> float# = <fun>
|}];;

(**************************************)
(* Test 3: No top-level bindings yet. *)

let x3 : float# = assert false;;
[%%expect{|
Line 1, characters 4-6:
1 | let x3 : float# = assert false;;
        ^^
Error: Top-level module bindings must have layout value, but x3 has layout
       float64.
|}];;

(*************************************)
(* Test 4: No putting them in tuples *)

let f4 (x : float#) = x, false;;
[%%expect{|
Line 1, characters 22-23:
1 | let f4 (x : float#) = x, false;;
                          ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       float# has layout float64, which is not a sublayout of value.
|}];;

(****************************************************)
(* Test 5: Can't be put in structures in typedecls. *)

type t5_1 = { x : float# };;
[%%expect{|
Line 1, characters 14-24:
1 | type t5_1 = { x : float# };;
                  ^^^^^^^^^^
Error: Type float# has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_2 = { y : int; x : float# };;
[%%expect{|
Line 1, characters 23-33:
1 | type t5_2 = { y : int; x : float# };;
                           ^^^^^^^^^^
Error: Type float# has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_3 = { x : float# } [@@unboxed];;
[%%expect{|
Line 1, characters 14-24:
1 | type t5_3 = { x : float# } [@@unboxed];;
                  ^^^^^^^^^^
Error: Type float# has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_4 = A of float#;;
[%%expect{|
Line 1, characters 12-23:
1 | type t5_4 = A of float#;;
                ^^^^^^^^^^^
Error: Type float# has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_5 = A of int * float#;;
[%%expect{|
Line 1, characters 12-29:
1 | type t5_5 = A of int * float#;;
                ^^^^^^^^^^^^^^^^^
Error: Type float# has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_6 = A of float# [@@unboxed];;
[%%expect{|
Line 1, characters 12-23:
1 | type t5_6 = A of float# [@@unboxed];;
                ^^^^^^^^^^^
Error: Type float# has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

(****************************************************)
(* Test 6: Can't be put at top level of signatures. *)
module type S6 = sig val x : float# end

let f6 (m : (module S6)) = let module M6 = (val m) in M6.x;;
[%%expect{|
Line 1, characters 29-35:
1 | module type S6 = sig val x : float# end
                                 ^^^^^^
Error: This type signature for x is not a value type.
       x has layout float64, which is not a sublayout of value.
|}];;

(*********************************************************)
(* Test 6: Can't be used as polymorphic variant argument *)
let f6 (x : float#) = `A x;;
[%%expect{|
Line 1, characters 25-26:
1 | let f6 (x : float#) = `A x;;
                             ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       float# has layout float64, which is not a sublayout of value.
|}];;
