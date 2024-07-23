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

let[@inline never] sub_t #(i1,#(f1,f1')) #(i2,#(f2,f2')) =
  #(i1 - i2, #(Float_u.sub f1 f2, f1' -. f2'))

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

(**************************************)
(* Test 3: unboxed tuples in closures *)

(* [go]'s closure should have an unboxed tuple with an [int] (immediate), a [float#]
   (float64) and a [float array] (value). *)
let[@inline never] f3 bounds steps_init () =
  let[@inline never] rec go k =
    let #(n,m) = bounds in
    let #(steps, init) = steps_init in
    if k = n
    then init
    else begin
      let acc = go (k + 1) in
      steps.(k) <- Float_u.to_float acc;
      Float_u.add m acc
    end
  in
  go 0

(* many args - even args are floats, odd args are unboxed tuples *)
let[@inline_never] f3_manyargs x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 steps () =
  let #(start_k, end_k) = x0 in
  let[@inline never] rec go k =
    if k = end_k
    then 0.0
    else begin
      let #(x2_1, #(x2_2, x2_3)) = x2 in
      let #(x4_1, #(x4_2, x4_3)) = x4 in
      let #(x6_1, #(x6_2, x6_3)) = x6 in
      let #(x8_1, #(x8_2, x8_3)) = x8 in
      let sum =
        Float.of_int x2_1 +. Float_u.to_float x2_2 +. x2_3 +.
        Float.of_int x4_1 +. Float_u.to_float x4_2 +. x4_3 +.
        Float.of_int x6_1 +. Float_u.to_float x6_2 +. x6_3 +.
        Float.of_int x8_1 +. Float_u.to_float x8_2 +. x8_3
      in
      let acc = go (k + 1) in
      steps.(k) <- acc;
      acc +. ((x1 +. x3 +. x5 +. x7 +. x9) *. sum)
    end
  in
  go start_k

let test3 () =
  (* Test f3 *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let five_pi = f3 #(5, #3.14) #(steps, #0.0) in
  print_floatu "Test 3, 5 * pi: " (five_pi ());
  Array.iteri (Printf.printf "  Test 3, step %d: %.2f\n") steps;

  (* Test f3_manyargs

          (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 50.86
      3 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 152.58
      6 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 306.16
      9 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 457.74

    ( but we expect some floating point error )
  *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let x1 = 3.14 in
  let x3 = 2.72 in
  let x5 = 1.62 in
  let x7 = 1.41 in
  let x9 = 42.0 in

  (* these sum to 3 *)
  let x2 = #(7, #(#40.0, 2.0)) in
  let x4 = #(-23, #(#100.0, 9.0)) in
  let x6 = #(-242, #(#5.5, 84.5)) in
  let x8 = #(-2, #(#20.0, 2.0)) in

  let f3_manyargs = f3_manyargs #(4,8) x1 x2 x3 x4 x5 x6 x7 x8 x9 steps in
  print_float "Test 3, 610.68: " (f3_manyargs ());
  Array.iteri (Printf.printf "  Test 3, step %d: %.2f\n") steps

let _ = test3 ()

(*********************************************)
(* Test 4: Partial and indirect applications *)

let[@inline never] test4 () =
  let one = #(-1, #(#1.33, 0.67)) in
  let two = #(-5, #(#12.7, -5.7)) in

  (* Simple indirect call *)
  let[@inline never] go f = f one two in
  let #(x1, x2) = #(go add_t, go sub_t) in
  print_t_sum "Test 4, 1 + 2" x1;
  print_t_sum "Test 4, 1 - 2" x2;

  (* partial application to an unboxed tuple and with one remaining *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let f = Sys.opaque_identity (f3 #(5,#3.14)) in
  let five_pi = f #(steps,#0.0) in
  print_floatu "Test 4, 5 * pi: " (five_pi ());
  Array.iteri (Printf.printf "  Test 4, step %d: %.2f\n") steps;

  (* That test again, but making f3 also opaque to prevent expansion of the
     partial application. *)
  let f3 = Sys.opaque_identity f3 in

  let steps = Array.init 10 (fun _ -> 0.0) in
  let f = Sys.opaque_identity (f3 #(5,#3.14)) in
  let five_pi = f #(steps,#0.0) in
  print_floatu "Test 4, 5 * pi: " (five_pi ());
  Array.iteri (Printf.printf "  Test 4, step %d: %.2f\n") steps

let _ = test4 ()

(****************************)
(* Test 5: Over application *)

let[@inline never] f5 n m =
  let[@inline never] go f =
    f (add_t n m)
  in
  go

let test5 () =
  let one = #(-1, #(#1.33, 0.67)) in
  let pi = #(1, #(#2.0, 0.14)) in
  let e = #(1, #(#0.1, 1.62)) in
  let _ : unit =
    f5 pi e
      (fun n s m -> print_t_sum s (add_t n m)) "Test 5, pi+e+1"
      one
  in
  ()

let _ = test5 ()

