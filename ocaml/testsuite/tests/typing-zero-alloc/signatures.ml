(* TEST
   * expect
*)

(* This tests the typing behavior of `[@zero_alloc]` annotation in signatures.

   These tests are just about what is allowed and not allowed by the
   typechecker.  The implementation of the actual `[@zero_alloc]` backend checks
   (including how the annotations in signatures affect those checks) are tested
   in the `tests/backend/checkmach` directory at the root of the project.
*)

(*******************************************)
(* Test 1: Allowed and disallowed payloads *)
module type S1_1 = sig
  val[@zero_alloc] f : int -> int
end

module type S1_2 = sig
  val[@zero_alloc opt] f : int -> int
end

module type S1_3 = sig
  val[@zero_alloc strict] f : int -> int
end

module type S1_4 = sig
  val[@zero_alloc strict opt] f : int -> int
end;;
[%%expect{|
module type S1_1 = sig val f : int -> int [@@zero_alloc] end
module type S1_2 = sig val f : int -> int [@@zero_alloc opt] end
module type S1_3 = sig val f : int -> int [@@zero_alloc strict] end
module type S1_4 = sig val f : int -> int [@@zero_alloc strict opt] end
|}]

module type S1_5 = sig
  val[@zero_alloc assume] f : int -> int
end;;
[%%expect{|
Line 2, characters 2-40:
2 |   val[@zero_alloc assume] f : int -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: zero_alloc "assume" attributes are not supported in signatures
|}]

module type S1_6 = sig
  val[@zero_alloc ignore] f : int -> int
end;;
[%%expect{|
Line 2, characters 2-40:
2 |   val[@zero_alloc ignore] f : int -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: zero_alloc "ignore" attributes are not supported in signatures
|}]

(******************************)
(* Test 2: allowed inclusions *)
module type S2_1 = sig
  val[@zero_alloc] f : 'a -> 'a
end

module M2_1 : S2_1 = struct
  let[@zero_alloc] f x = x
end

module M2_2 : S2_1 = struct
  let[@zero_alloc assume] f x = x
end

module M2_3 : S2_1 = struct
  let[@zero_alloc assume never_returns_normally] f x = x
end

module M2_4 : S2_1 = struct
  let[@zero_alloc strict] f x = x
end

module M2_5 : S2_1 = struct
  let[@zero_alloc assume strict] f x = x
end

module M2_6 : S2_1 = struct
  let[@zero_alloc assume strict never_returns_normally] f x = x
end;;

[%%expect{|
module type S2_1 = sig val f : 'a -> 'a [@@zero_alloc] end
module M2_1 : S2_1
module M2_2 : S2_1
module M2_3 : S2_1
module M2_4 : S2_1
module M2_5 : S2_1
module M2_6 : S2_1
|}]

module type S2_2 = sig
  val[@zero_alloc opt] f : 'a -> 'a
end

module M2_7 : S2_2 = struct
  let[@zero_alloc] f x = x
end

module M2_8 : S2_2 = struct
  let[@zero_alloc opt] f x = x
end

module M2_9 : S2_2 = struct
  let[@zero_alloc assume] f x = x
end

module M2_10 : S2_2 = struct
  let[@zero_alloc assume never_returns_normally] f x = x
end

module M2_11 : S2_2 = struct
  let[@zero_alloc strict] f x = x
end

module M2_12 : S2_2 = struct
  let[@zero_alloc strict opt] f x = x
end

module M2_13 : S2_2 = struct
  let[@zero_alloc assume strict] f x = x
end

module M2_14 : S2_2 = struct
  let[@zero_alloc assume strict never_returns_normally] f x = x
end;;

[%%expect{|
module type S2_2 = sig val f : 'a -> 'a [@@zero_alloc opt] end
module M2_7 : S2_2
module M2_8 : S2_2
module M2_9 : S2_2
module M2_10 : S2_2
module M2_11 : S2_2
module M2_12 : S2_2
module M2_13 : S2_2
module M2_14 : S2_2
|}]

module type S2_3 = sig
  val[@zero_alloc strict] f : 'a -> 'a
end

module M2_15 : S2_3 = struct
  let[@zero_alloc strict] f x = x
end

module M2_16 : S2_3 = struct
  let[@zero_alloc assume strict] f x = x
end

module M2_17 : S2_3 = struct
  let[@zero_alloc assume strict never_returns_normally] f x = x
end;;

[%%expect{|
module type S2_3 = sig val f : 'a -> 'a [@@zero_alloc strict] end
module M2_15 : S2_3
module M2_16 : S2_3
module M2_17 : S2_3
|}]

module type S2_4 = sig
  val[@zero_alloc strict opt] f : 'a -> 'a
end

module M2_18 : S2_4 = struct
  let[@zero_alloc strict] f x = x
end

module M2_19 : S2_4 = struct
  let[@zero_alloc strict opt] f x = x
end

module M2_20 : S2_4 = struct
  let[@zero_alloc assume strict] f x = x
end

module M2_21 : S2_4 = struct
  let[@zero_alloc assume strict never_returns_normally] f x = x
end;;

[%%expect{|
module type S2_4 = sig val f : 'a -> 'a [@@zero_alloc strict opt] end
module M2_18 : S2_4
module M2_19 : S2_4
module M2_20 : S2_4
module M2_21 : S2_4
|}]

(*********************************)
(* Test 3: disallowed inclusions *)

module type S3_1 = sig
  val[@zero_alloc] f : 'a -> 'a
end

module M3_1 : S3_1 = struct
  let f x = x
end;;

[%%expect{|
module type S3_1 = sig val f : 'a -> 'a [@@zero_alloc] end
Lines 5-7, characters 21-3:
5 | .....................struct
6 |   let f x = x
7 | end..
Error: Signature mismatch:
       Modules do not match: sig val f : 'a -> 'a end is not included in S3_1
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : 'a -> 'a [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module M3_2 : S3_1 = struct
  let[@zero_alloc opt] f x = x
end;;

[%%expect{|
Lines 1-3, characters 21-3:
1 | .....................struct
2 |   let[@zero_alloc opt] f x = x
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc opt] end
       is not included in
         S3_1
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc opt]
       is not included in
         val f : 'a -> 'a [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module type S3_2 = sig
  val[@zero_alloc opt] f : 'a -> 'a
end

module M3_3 : S3_2 = struct
  let f x = x
end;;

[%%expect{|
module type S3_2 = sig val f : 'a -> 'a [@@zero_alloc opt] end
Lines 5-7, characters 21-3:
5 | .....................struct
6 |   let f x = x
7 | end..
Error: Signature mismatch:
       Modules do not match: sig val f : 'a -> 'a end is not included in S3_2
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : 'a -> 'a [@@zero_alloc opt]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module type S3_3 = sig
  val[@zero_alloc strict] f : 'a -> 'a
end

module M3_4 : S3_3= struct
  let f x = x
end;;

[%%expect{|
module type S3_3 = sig val f : 'a -> 'a [@@zero_alloc strict] end
Lines 5-7, characters 20-3:
5 | ....................struct
6 |   let f x = x
7 | end..
Error: Signature mismatch:
       Modules do not match: sig val f : 'a -> 'a end is not included in S3_3
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module M3_5 : S3_3 = struct
  let[@zero_alloc assume] f x = x
end;;

[%%expect{|
Lines 1-3, characters 21-3:
1 | .....................struct
2 |   let[@zero_alloc assume] f x = x
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc] end
       is not included in
         S3_3
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module M3_6 : S3_3 = struct
  let[@zero_alloc opt] f x = x
end;;

[%%expect{|
Lines 1-3, characters 21-3:
1 | .....................struct
2 |   let[@zero_alloc opt] f x = x
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc opt] end
       is not included in
         S3_3
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc opt]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module M3_7 : S3_3 = struct
  let[@zero_alloc strict opt] f x = x
end;;

[%%expect{|
Lines 1-3, characters 21-3:
1 | .....................struct
2 |   let[@zero_alloc strict opt] f x = x
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc strict opt] end
       is not included in
         S3_3
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc strict opt]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module M3_8 : S3_3 = struct
  let[@zero_alloc assume never_returns_normally] f x = x
end;;

[%%expect{|
Lines 1-3, characters 21-3:
1 | .....................struct
2 |   let[@zero_alloc assume never_returns_normally] f x = x
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc] end
       is not included in
         S3_3
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module type S3_4 = sig
  val[@zero_alloc strict opt] f : 'a -> 'a
end

module M3_9 : S3_4 = struct
  let f x = x
end;;

[%%expect{|
module type S3_4 = sig val f : 'a -> 'a [@@zero_alloc strict opt] end
Lines 5-7, characters 21-3:
5 | .....................struct
6 |   let f x = x
7 | end..
Error: Signature mismatch:
       Modules do not match: sig val f : 'a -> 'a end is not included in S3_4
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict opt]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module M3_10 : S3_4 = struct
  let[@zero_alloc assume] f x = x
end;;

[%%expect{|
Lines 1-3, characters 22-3:
1 | ......................struct
2 |   let[@zero_alloc assume] f x = x
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc] end
       is not included in
         S3_4
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict opt]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module M3_11 : S3_4 = struct
  let[@zero_alloc opt] f x = x
end;;

[%%expect{|
Lines 1-3, characters 22-3:
1 | ......................struct
2 |   let[@zero_alloc opt] f x = x
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc opt] end
       is not included in
         S3_4
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc opt]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict opt]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

module M3_12 : S3_4 = struct
  let[@zero_alloc assume never_returns_normally] f x = x
end;;

[%%expect{|
Lines 1-3, characters 22-3:
1 | ......................struct
2 |   let[@zero_alloc assume never_returns_normally] f x = x
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc] end
       is not included in
         S3_4
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict opt]
       The former provides a weaker "zero_alloc" guarantee than the latter
|}]

(***************************************************)
(* Test 4: requires function types, does expansion *)

module type S4_1 = sig
  val[@zero_alloc] x : int
end;;
[%%expect{|
Line 2, characters 2-26:
2 |   val[@zero_alloc] x : int
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: In signatures, zero_alloc is only supported on function declarations.
       Found no arrows in this declaration's type.
|}];;

module type S4_2 = sig
  type t = string
  val[@zero_alloc] x : t
end;;
[%%expect{|
Line 3, characters 2-24:
3 |   val[@zero_alloc] x : t
      ^^^^^^^^^^^^^^^^^^^^^^
Error: In signatures, zero_alloc is only supported on function declarations.
       Found no arrows in this declaration's type.
|}];;

module type S4_3 = sig
  type t = int -> int
  type s = t
  val[@zero_alloc] x : s
end;;
[%%expect{|
module type S4_3 =
  sig type t = int -> int type s = t val x : s [@@zero_alloc] end
|}];;

(***********************************************)
(* Test 5: arities must match, after expansion *)

type t5_two_args = int -> int -> int
type 'a t5_id = 'a

module type S5_1 = sig
  val[@zero_alloc] f : int -> int -> int
end

module M5_1 : S5_1 = struct
  let[@zero_alloc] f x y = x + y
end

module M5_2 : S5_1 = struct
  let[@zero_alloc] f x =
    function 0 -> x + x
           | n -> x + n
end

module M5_3 : S5_1 = struct
  let[@zero_alloc] f x = fun y -> x + y
end;;
[%%expect{|
type t5_two_args = int -> int -> int
type 'a t5_id = 'a
module type S5_1 = sig val f : int -> int -> int [@@zero_alloc] end
module M5_1 : S5_1
module M5_2 : S5_1
Lines 18-20, characters 21-3:
18 | .....................struct
19 |   let[@zero_alloc] f x = fun y -> x + y
20 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int -> int [@@zero_alloc] end
       is not included in
         S5_1
       Values do not match:
         val f : int -> int -> int [@@zero_alloc]
       is not included in
         val f : int -> int -> int [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 1 and the latter is 2.
|}];;

module type S5_2 = sig
  val[@zero_alloc] f : t5_two_args
end

module M5_4 : S5_2 = struct
  let[@zero_alloc] f x y = x + y
end

module M5_5 : S5_2 = struct
  let[@zero_alloc] f x = fun y -> x + y
end;;
[%%expect{|
module type S5_2 = sig val f : t5_two_args [@@zero_alloc] end
module M5_4 : S5_2
Lines 9-11, characters 21-3:
 9 | .....................struct
10 |   let[@zero_alloc] f x = fun y -> x + y
11 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int -> int [@@zero_alloc] end
       is not included in
         S5_2
       Values do not match:
         val f : int -> int -> int [@@zero_alloc]
       is not included in
         val f : t5_two_args [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 1 and the latter is 2.
|}];;

module type S5_3 = sig
  val[@zero_alloc] f : t5_two_args t5_id
end

module M5_6 : S5_3 = struct
  let[@zero_alloc] f x y = x + y
end

module M5_7 : S5_3 = struct
  let[@zero_alloc] f x = fun y -> x + y
end;;
[%%expect{|
module type S5_3 = sig val f : t5_two_args t5_id [@@zero_alloc] end
module M5_6 : S5_3
Lines 9-11, characters 21-3:
 9 | .....................struct
10 |   let[@zero_alloc] f x = fun y -> x + y
11 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int -> int [@@zero_alloc] end
       is not included in
         S5_3
       Values do not match:
         val f : int -> int -> int [@@zero_alloc]
       is not included in
         val f : t5_two_args t5_id [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 1 and the latter is 2.
|}];;

(* We do not update the arity due to substitutions. *)
module type S5_4 = sig
  type t
  val[@zero_alloc] f : t -> t
end

module M5_8 : S5_4 = struct
  type t = int
  let[@zero_alloc] f x = x
end

module type S5_4' = S5_4 with type t = int -> int

module M5_9 : S5_4'  = struct
  type t = int -> int
  let[@zero_alloc] f x = x
end

module M5_10 : S5_4'  = struct
  type t = int -> int
  let[@zero_alloc] f g x = g (x+1)
end
[%%expect{|
module type S5_4 = sig type t val f : t -> t [@@zero_alloc] end
module M5_8 : S5_4
module type S5_4' = sig type t = int -> int val f : t -> t [@@zero_alloc] end
module M5_9 : S5_4'
Lines 18-21, characters 24-3:
18 | ........................struct
19 |   type t = int -> int
20 |   let[@zero_alloc] f g x = g (x+1)
21 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = int -> int
           val f : (int -> 'a) -> int -> 'a [@@zero_alloc]
         end
       is not included in
         S5_4'
       Values do not match:
         val f : (int -> 'a) -> int -> 'a [@@zero_alloc]
       is not included in
         val f : t -> t [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 2 and the latter is 1.
|}]

