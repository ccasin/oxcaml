open Stdlib_upstream_compatible

module I64 = struct
  type t = int64# array

  external[@layout_poly] reinterp_get :
    ('a : any). t -> int -> 'a = "%magic_reinterp_array_safe_get"

  external[@layout_poly] reinterp_set :
    ('a : any). t -> int -> 'a -> unit = "%magic_reinterp_array_safe_set"

  external create_uninitialized
    :  len:int -> t
    = "caml_make_unboxed_int64_vect_bytecode" "caml_make_unboxed_int64_vect"

  external unsafe_set : t -> int -> int64# -> unit =
    "%array_unsafe_set"

  let init len ~f =
    let r = create_uninitialized ~len in
    for i = 0 to len - 1 do
      unsafe_set r i (f i)
    done;
    r
  ;;

  let a_i64 = init 20 ~f:(fun i -> Int64_u.of_int i)

  let _ =
    let #(i4, i5, i6) : #(int64# * int64# * int64#) = reinterp_get a_i64 5 in
    Printf.printf "#(%d, %d, %d)"
      (Int64_u.to_int i4) (Int64_u.to_int i5) (Int64_u.to_int i6)
end

(* external[@layout_poly] reinterp_get_f64 :
 *   ('a : any). float# array -> int -> 'a = "%magic_reinterp_array_safe_get"
 *
 * external[@layout_poly] reinterp_set_f64 :
 *   ('a : any). float# array -> int -> 'a -> unit =
 *   "%magic_reinterp_array_safe_set"
 *
 * external[@layout_poly] reinterp_get_value :
 *   ('a : any). string array -> int -> 'a = "%magic_reinterp_array_safe_get"
 *
 * external[@layout_poly] reinterp_set_value :
 *   ('a : any). string array -> int -> 'a -> unit =
 *   "%magic_reinterp_array_safe_set" *)
