(* TEST
 * expect
*)

(* You may constrain abstract types in packages. *)
module type S = sig
  type t
end

type m = (module S with type t = int);;
[%%expect{|
module type S = sig type t end
type m = (module S with type t = int)
|}];;

(* It works with non-trivial paths. *)
module type S = sig
  module M : sig
    type t
  end
end

type m = (module S with type M.t = int)
[%%expect{|
module type S = sig module M : sig type t end end
type m = (module S with type M.t = int)
|}];;

(* It respects immediacy. *)
module type S = sig
  type t [@@immediate]
end

type m1 = (module S with type t = int)
type m2 = (module S with type t = string);;
[%%expect{|
module type S = sig type t : immediate end
type m1 = (module S with type t = int)
Line 6, characters 10-41:
6 | type m2 = (module S with type t = string);;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = string
       is not included in
         type t : immediate
       The layout of the first is value, because
         it is the primitive value type string.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-22.
|}];;

(* You may not constrain types with a manifest in a package *)
module type S = sig
  type t = int
end

type m = (module S with type t = string);;
[%%expect{|
module type S = sig type t = int end
Line 5, characters 9-40:
5 | type m = (module S with type t = string);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type t is defined to be int.
       Package `with' constraints may only be used on abstract types.
|}];;

(* Even if your constraint would be satisfied. *)
module type S = sig
  type t = int
end

type m = (module S with type t = int);;
[%%expect{|
module type S = sig type t = int end
Line 5, characters 9-37:
5 | type m = (module S with type t = int);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type t is defined to be int.
       Package `with' constraints may only be used on abstract types.
|}];;

(* And even if the manifest is not obvious in the original definition. *)
module M = struct
  type t
end

module type S = sig
  module P = M
end

type m = (module S with type P.t = int);;
[%%expect{|
module M : sig type t end
module type S = sig module P = M end
Line 9, characters 9-39:
9 | type m = (module S with type P.t = int);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type P.t is defined to be M.t.
       Package `with' constraints may only be used on abstract types.
|}];;


