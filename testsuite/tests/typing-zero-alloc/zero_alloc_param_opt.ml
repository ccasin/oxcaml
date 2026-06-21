(* TEST
   flags = "-w +198 -zero-alloc-check opt";
   expect.opt;
*)

(** Tests for `[@zero_alloc opt]` on function arguments, run under
    `-zero-alloc-check opt`. Under this mode, `opt` annotations are active;
    compare with the analogous tests in zero_alloc_param.ml
    which run under the default mode where `opt` is inactive. *)

let[@zero_alloc] require_za_arity_1 (f [@zero_alloc arity 1]) =
  f 42;;
[%%expect {|
val require_za_arity_1 : ((int -> 'a) [@zero_alloc arity 1]) -> 'a
  [@@zero_alloc] = <fun>
|}];;

let f_requires_opt (g [@zero_alloc opt arity 1]) = g 0;;
[%%expect {|
val f_requires_opt : ((int -> 'a) [@zero_alloc opt arity 1]) -> 'a = <fun>
|}];;

let[@zero_alloc] za_fn x = x + 1;;
let non_za_fn x = x + 1;;
[%%expect {|
val za_fn : int -> int [@@zero_alloc] = <fun>
val non_za_fn : int -> int = <fun>
|}];;

(* Passing a [@zero_alloc] function to an opt parameter: succeeds. *)
let _ = f_requires_opt za_fn;;
[%%expect {|
- : int = 1
|}];;

(* Passing an unannotated function to an opt parameter succeeds in both modes.
   The function's zero_alloc is a unification Var that absorbs the constraint
   at the call site; the backend then verifies it; here non_za_fn is
   non-allocating, so the check passes. *)
let _ = f_requires_opt non_za_fn;;
[%%expect {|
- : int = 1
|}];;

(* Constraining an allocating function to be zero_alloc opt via such a parameter
   results in a back-end failure when [-zero-alloc-check opt] is passed. A
   corresponding test in [zero_alloc_param.ml] confirms the same function is not
   checked by the backend otherwise. *)
module M = struct
  let allocating_fn x = (x, x)
  let _ = f_requires_opt allocating_fn
end
[%%expect {|
Line 2, characters 20-30:
2 |   let allocating_fn x = (x, x)
                        ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP8.M.allocating_fn (camlTOP8__allocating_fn_8_9_code).
Line 2, characters 24-30:
2 |   let allocating_fn x = (x, x)
                            ^^^^^^
Error: allocation of 24 bytes
|}];;

(** When [-zero-alloc-check opt] is passed, we can make use of [@zero_alloc opt]
    info from parameters. A corresponding test in [zero_alloc_param.ml] confirms
    this does not pass the backend check otherwise. *)
(* XXX double check this is the known sadness. *)
let[@zero_alloc] f : ((int -> int) [@zero_alloc opt]) -> int =
  fun (g [@zero_alloc opt arity 1]) -> g 42;;
[%%expect {|
val f : ((int -> int) [@zero_alloc opt arity 1]) -> int [@@zero_alloc] =
  <fun>
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc opt]) -> int =
  fun (g [@zero_alloc arity 1]) -> g 42;;
[%%expect {|
val f : ((int -> int) [@zero_alloc opt arity 1]) -> int [@@zero_alloc] =
  <fun>
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc]) -> int =
  fun (g [@zero_alloc opt arity 1]) -> g 42;;
[%%expect {|
Line 2, characters 2-43:
2 |   fun (g [@zero_alloc opt arity 1]) -> g 42;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "zero_alloc" attribute on this function parameter conflicts
       with the one on its type.
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}];;
