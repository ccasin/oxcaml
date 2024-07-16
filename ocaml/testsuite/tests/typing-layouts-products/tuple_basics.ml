(* TEST
 flambda2;
 include stdlib_beta;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

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
type 'a t_constraint
module F :
  functor (X : S_constrain_type_jkind_deep') ->
    sig type r = X.t2 t_constraint end
|}]

