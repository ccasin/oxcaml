(* TEST
 reference = "${test_source_directory}/unboxed_tuples.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }{
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compiler_reference = "${test_source_directory}/unboxed_tuples_stable.compilers.reference";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   flags = "-extension-universe upstream_compatible";
   compiler_reference = "${test_source_directory}/unboxed_tuples_stable.compilers.reference";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   flags = "-extension-universe no_extensions";
   compiler_reference = "${test_source_directory}/unboxed_tuples_disabled.compilers.reference";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)


open Stdlib_upstream_compatible

let print_floatu prefix x = Printf.printf "%s: %.2f\n" prefix (Float_u.to_float x)
let print_float prefix x = Printf.printf "%s: %.2f\n" prefix x
let print_int prefix x = Printf.printf "%s: %d\n" prefix x
let print_ints prefix #(x,y) = Printf.printf "%s: #(%d,%d)\n" prefix x y

(*******************************************************)
(* Test 1: Basic functions manipulating unboxed tuples *)

let mult_float_by_int #(f, i) = Float_u.(mul f (of_int i))

let div_mod m n = #(m/n, m mod n)

let test1 () =
  let pi = #3.14 in
  print_floatu "Test 1, twice pi" (mult_float_by_int #(pi,2));
  print_ints "Test 1, 14/3" (div_mod 14 3)

let _ = test1 ()
