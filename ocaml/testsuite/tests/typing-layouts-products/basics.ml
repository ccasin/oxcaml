(* TEST
 flambda2;
 include stdlib_beta;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

(**************************************************)
(* Basic unboxed product layouts and tuple types. *)

type t1 : float64 & value
type t2 = #(string * float# * int)
[%%expect{|
type t1 : float64 & value
type t2 = #(string * float# * int)
|}]

(* You can put unboxed and normal products inside unboxed products *)
type t3 : value & (bits64 & (value & float32))
type t4 = #(string * #(int * (bool * int) * char option))
[%%expect{|
type t3 : value & bits64 & value & float32
type t4 = #(string * #(int * (bool * int) * char option))
|}]

(* But you can't put unboxed products into normal tuples (yet) *)
type t_nope = string * #(string * bool)
[%%expect{|
Line 1, characters 23-39:
1 | type t_nope = string * #(string * bool)
                           ^^^^^^^^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of #(string * bool) is value & value, because
         it is an unboxed tuple.
       But the layout of #(string * bool) must be a sublayout of value, because
         it's the type of a tuple element.
|}]

(************************************)
(* Simple kind annotations on types *)

type t1 : float64 & value = #(float# * bool)
type t2 : value & (float64 & value) = #(string option * t1)
[%%expect{|
type t1 = #(float# * bool)
type t2 = #(string option * t1)
|}]

type t2_wrong : value & float64 & value = #(string option * t1)
[%%expect{|
Line 1, characters 0-63:
1 | type t2_wrong : value & float64 & value = #(string option * t1)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type #(string option * t1) is value & (float64 & value), because
         it is an unboxed tuple.
       But the layout of type #(string option * t1) must be a sublayout of
         value & float64 & value, because
         of the definition of t2_wrong at line 1, characters 0-63.
|}]

type ('a : value & bits64) t3 = 'a
type t4 = #(int * int64#) t3
type t5 = t4 t3
[%%expect{|
type ('a : value * bits64) t3 = 'a
type t4 = #(int * int64#) t3
type t5 = t4 t3
|}]

type t4_wrong = #(int * int) t3
[%%expect{|
Line 1, characters 16-28:
1 | type t4_wrong = #(int * int) t3
                    ^^^^^^^^^^^^
Error: This type #(int * int) should be an instance of type
         ('a : value * bits64)
       The layout of #(int * int) is immediate & immediate, because
         it is an unboxed tuple.
       But the layout of #(int * int) must be a sublayout of value & bits64, because
         of the definition of t3 at line 1, characters 0-34.
|}]

(* some mutually recusive types *)
type ('a : value & bits64) t6 = 'a t7
and 'a t7 = { x : 'a t6 }
[%%expect{|
type ('a : value * bits64) t6 = 'a t7
and ('a : value * bits64) t7 = { x : 'a t6; }
|}]

type t9 = #(int * int64#) t7
type t10 = bool t6
[%%expect{|
type t9 = #(int * int64#) t7
Line 2, characters 11-15:
2 | type t10 = bool t6
               ^^^^
Error: This type bool should be an instance of type ('a : value * bits64)
       The layout of bool is immediate, because
         it's an enumeration variant type (all constructors are constant).
       But the layout of bool must be a sublayout of value & bits64, because
         of the definition of t6 at line 1, characters 0-37.
|}]

type ('a : value & bits64) t6_wrong = 'a t7_wrong
and 'a t7_wrong = { x : #(int * int64) t6_wrong }
[%%expect{|
Line 2, characters 24-38:
2 | and 'a t7_wrong = { x : #(int * int64) t6_wrong }
                            ^^^^^^^^^^^^^^
Error: This type #(int * int64) should be an instance of type
         ('a : value * bits64)
       The layout of #(int * int64) is value & value, because
         it is an unboxed tuple.
       But the layout of #(int * int64) must be a sublayout of value & bits64, because
         of the annotation on 'a in the declaration of the type t6_wrong.
|}]

(* Just like t6/t7, but with the annotation on the other (the order doesn't
   matter) *)
type 'a t11 = 'a t12
and ('a : value & bits64) t12 = { x : 'a t11 }
[%%expect{|
type ('a : value * bits64) t11 = 'a t12
and ('a : value * bits64) t12 = { x : 'a t11; }
|}]

(***********************************)
(* Nested expansion in kind checks *)

(* This test shows that the [check_coherence] check in Typedecl can look deeply
   into a product kind. That check is reached in this case because the
   algorithm in typedecl assumes the annotation is correct initially, and then
   it is checked in [check_coherence]. This relies on [type_jkind] doing
   deep expansion, as [check_coherence] calls it and then [Jkind.sub], rather
   than using [check_type_jkind]. *)
module type S_coherence_deep = sig
  type t1 : any
  type t2 = #(int * t1)
end

module type S_coherence_deep' = S_coherence_deep with type t1 = float#

module F(X : S_coherence_deep') = struct
  type r : value & float64 = X.t2
end
[%%expect{|
module type S_coherence_deep = sig type t1 : any type t2 = #(int * t1) end
module type S_coherence_deep' =
  sig type t1 = float# type t2 = #(int * t1) end
module F : functor (X : S_coherence_deep') -> sig type r = X.t2 end
|}]


(* Like the above, but hitting the nested expansion case in
   [constrain_type_jkind] *)
module type S_constrain_type_jkind_deep = sig
  type t1 : any
  type t2 = #(int * t1)
end

module type S_constrain_type_jkind_deep' =
  S_constrain_type_jkind_deep with type t1 = float#

type ('a : value & float64) t_constraint

module F(X : S_constrain_type_jkind_deep') = struct
  type r = X.t2 t_constraint
end
[%%expect{|
module type S_constrain_type_jkind_deep =
  sig type t1 : any type t2 = #(int * t1) end
module type S_constrain_type_jkind_deep' =
  sig type t1 = float# type t2 = #(int * t1) end
type ('a : value * float64) t_constraint
module F :
  functor (X : S_constrain_type_jkind_deep') ->
    sig type r = X.t2 t_constraint end
|}]

