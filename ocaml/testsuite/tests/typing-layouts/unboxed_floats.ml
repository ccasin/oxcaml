(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(* CR layouts v2: These tests will change when we actually typecheck
   unboxed literals.
 *)

let id : float# -> float# = fun x -> x;;

id #4.0;;
[%%expect {|
Line 1, characters 28-38:
1 | let id : float# -> float# = fun x -> x;;
                                ^^^^^^^^^^
Error: Non-value layout float64 detected in [Typeopt.layout] as sort for type
       float#. Please report this error to the Jane Street compilers team.
|}];;

(* CR layouts: We should actually add the numbers here when
   we support that.
*)
let add (x : float#) (y : float#) = x +. y;;

add #4.0 #5.0;;
[%%expect {|
Line 1, characters 36-37:
1 | let add (x : float#) (y : float#) = x +. y;;
                                        ^
Error: This expression has type float# but an expression was expected of type
         float
|}];;

let apply (f : float# -> float# -> float#) (x : float#) (y : float#) =
  f x y;;

apply add #4.0 #5.0;;
[%%expect {|
Lines 1-2, characters 43-7:
1 | ...........................................(x : float#) (y : float#) =
2 |   f x y..
Error: Non-value layout float64 detected in [Typeopt.layout] as sort for type
       float#. Please report this error to the Jane Street compilers team.
|}];;
