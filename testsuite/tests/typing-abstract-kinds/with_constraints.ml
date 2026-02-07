(* TEST
   expect;
*)

(***************************************************)
(* Test: Basic non-destructive constraint behavior *)
module type S = sig
  kind_ k
  type t : k
end

module M = struct
  kind_ k = value
  type t = int
end

module M1 : S = M
module M2 : S with kind_ k = value = M
[%%expect{|
module type S = sig kind_ k type t : k end
module M : sig kind_ k = value type t = int end
module M1 : S
module M2 : sig kind_ k = value type t end
|}]

module M3 : S with kind_ k = bits64 = M
[%%expect{|
Line 1, characters 38-39:
1 | module M3 : S with kind_ k = bits64 = M
                                          ^
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = value type t = int end
       is not included in
         sig kind_ k = bits64 type t : bits64 end
       Kind declarations do not match:
         kind_ k = value
       is not included in
         kind_ k = bits64
       Their definitions are not equal.
|}]

(*******************************************************)
(* Test: You can't change the manifest, if one exists. *)
module type S = sig
  kind_ k = value
end

module type S' = S with kind_ k = value
[%%expect{|
module type S = sig kind_ k = value end
module type S' = sig kind_ k = value end
|}]

module type S' = S with kind_ k = immediate
[%%expect{|
Line 1, characters 17-43:
1 | module type S' = S with kind_ k = immediate
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "k"
       does not match its original definition in the constrained signature:
       Kind declarations do not match:
         kind_ k = immediate
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]

module type S' = S with kind_ k = any
[%%expect{|
Line 1, characters 17-37:
1 | module type S' = S with kind_ k = any
                     ^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "k"
       does not match its original definition in the constrained signature:
       Kind declarations do not match:
         kind_ k = any
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]
