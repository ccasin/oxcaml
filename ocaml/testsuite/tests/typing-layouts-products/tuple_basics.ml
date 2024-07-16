(* TEST
 flambda2;
 include stdlib_beta;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

(* This test shows that the jkind checking algorithm will expand a type inside
   an unboxed product type, when needed. In particular, to checking [r]'s jkind,
   it's not sufficient to use the jkind on [t2] from its declaration nor to just
   head expand [t2]. The algorithm used before we added unboxed tuples did not
   support this. *)
module type S_deep_expansion = sig
  type t1 : any
  type t2 = #(int * t1)
end

module type S_deep_expansion' = S_deep_expansion with type t1 = float#

module F(X : S_deep_expansion') = struct
  type r : value & float64 = X.t2
end
[%%expect{|
module type S_deep_expansion = sig type t1 : any type t2 = #(int * t1) end
module type S_deep_expansion' =
  sig type t1 = float# type t2 = #(int * t1) end
Line 9, characters 2-33:
9 |   type r : value & float64 = X.t2
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type X.t2 is value
         * any, because
         it is an unboxed tuple.
       But the layout of type X.t2 must be a sublayout of value
         * float64, because
         of the definition of r at line 9, characters 2-33.
|}]
(* CR ccasinghino: needs fixing. *)
