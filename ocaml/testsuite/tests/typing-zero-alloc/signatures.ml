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
(* Test 2: disallowed inclusions *)

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

(* Tests to add:
   - disallowed inclusions
   - arity
   - doesn't work on externals
   - various misplaced attribute cases (no arrow)
   - module type of
   ... *)

