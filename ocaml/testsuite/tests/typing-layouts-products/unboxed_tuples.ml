(* TEST
 reference = "${test_source_directory}/unboxed_tuples.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
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
 } {
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
   }
*)

open Stdlib_upstream_compatible

let print_floatu prefix x = Printf.printf "%s: %.2f\n" prefix (Float_u.to_float x)
let print_float prefix x = Printf.printf "%s: %.2f\n" prefix x
let print_int prefix x = Printf.printf "%s: %d\n" prefix x
let print_ints prefix #(x,y) = Printf.printf "%s: #(%d,%d)\n" prefix x y

(*******************************************************)
(* Test 1: Basic functions manipulating unboxed tuples *)

(* takes a tuple *)
let mult_float_by_int #(f, i) = Float_u.(mul f (of_int i))

(* returns a tuple *)
let div_mod m n = #(m/n, m mod n)

(* take multiple nested tuples, returns a nested tuple *)
let add_some_stuff x y =
  let #(a,b,#(c,d)) = x in
  let #(e,#(f,#(g,h,i))) = y in
  #(a+b+c, #(d+e+f, g+h+i))

let test1 () =
  let pi = #3.14 in
  print_floatu "Test 1, twice pi inlined"
    ((mult_float_by_int [@inlined]) #(pi,2));
  print_floatu "Test 1, twice pi not inlined"
    ((mult_float_by_int [@inlined never]) #(pi,2));
  print_ints "Test 1, 14/3 inlined" ((div_mod [@inlined]) 14 3);
  print_ints "Test 1, 14/3 not inlined" ((div_mod [@inlined never]) 14 3);
  let #(a,#(b,c)) =
    (add_some_stuff [@inlined]) #(1,2,#(3,4)) #(5,#(6,#(7,8,9)))
  in
  Printf.printf "Test 1, #(6,#(15,24)) inlined: #(%d,#(%d,%d))\n" a b c;
  let #(a,#(b,c)) =
    (add_some_stuff [@inlined never]) #(1,2,#(3,4)) #(5,#(6,#(7,8,9)))
  in
  Printf.printf "Test 1, #(6,#(15,24)) not inlined: #(%d,#(%d,%d))\n" a b c

let _ = test1 ()

(**********************************)
(* Test 2: higher-order functions *)

type t = #(int * #(float# * float))

let[@inline never] add_t #(i1,#(f1,f1')) #(i2,#(f2,f2')) =
  #(i1 + i2, #(Float_u.add f1 f2, f1' +. f2'))

let[@inline never] twice f (x : t) = f (f x)

let[@inline never] compose f g (x : t) = f (g x)

let print_t_sum prefix #(i,#(f1,f2)) =
  Printf.printf "%s: %.2f\n" prefix
    ((Float.of_int i) +. (Float_u.to_float f1 +. f2))

let[@inline never] twice_on_pi f =
  let pi = #(1, #(#2.0, 0.14)) in
  twice f pi

let times_four =
  twice (fun x -> add_t x x)

let _ =
  let pi = #(1, #(#2.0, 0.14)) in
  let one = #(0, #(#1.0, 0.0)) in
  let zero = #(0, #(#0.0, 0.0)) in

  print_t_sum "Test 2, add pi twice"
    (twice (fun x -> add_t x pi) zero);
  print_t_sum "Test 2, add pi four times"
    (twice (twice (fun x -> add_t x pi)) zero);
  print_t_sum "Test 2, increment pi twice"
    (twice_on_pi (fun x -> add_t one x));
  print_t_sum "Test 2, increment pi four times"
    (twice_on_pi (twice (fun x -> add_t one x)));
  print_t_sum "Test 2, e times four"
    (times_four #(1, #(#1.0, 2.72)));
  print_t_sum "Test 2, pi times sixteen"
    (twice_on_pi times_four);
  print_t_sum "Test 2, pi times sixteen again"
    (compose times_four times_four pi);
  print_t_sum "Test 2, pi minus four"
    (let two = twice (fun x -> add_t x one) zero in
     let add_two = add_t two in
     let add_two_after = compose add_two in
     let minus_four =
       add_two_after (twice (fun x -> add_t x #(-1,#(-#1.0, -1.0))))
     in
     minus_four pi)




