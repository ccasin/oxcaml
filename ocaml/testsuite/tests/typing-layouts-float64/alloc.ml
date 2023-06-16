(* TEST
   flags = "-extension layouts_alpha"
   * native
*)

(* A test comparing allocations with unboxed floats to allocations with boxed
   floats. *)

(* CR layouts v2: test shouldn't print exact allocation numbers. And I've only
   run it on closure - surely different on other middle ends. *)


module type Float_u = sig
  external to_float : float# -> (float[@local_opt]) = "%box_float"
  external of_float : (float[@local_opt]) -> float# = "%unbox_float"

  val ( + ) : float# -> float# -> float#
  val ( - ) : float# -> float# -> float#
  val ( * ) : float# -> float# -> float#
  val ( / ) : float# -> float# -> float#
  val ( ** ) : float# -> float# -> float#
  val ( > ) : float# -> float# -> bool
end

module Float_u : Float_u = struct
  external to_float : float# -> (float[@local_opt]) = "%box_float"
  external of_float : (float[@local_opt]) -> float# = "%unbox_float"

  let ( + ) x y = of_float ((to_float x) +. (to_float y))
  let ( - ) x y = of_float ((to_float x) -. (to_float y))
  let ( * ) x y = of_float ((to_float x) *. (to_float y))
  let ( / ) x y = of_float ((to_float x) /. (to_float y))
  let ( ** ) x y = of_float ((to_float x) ** (to_float y))
  let ( > ) x y = (to_float x) > (to_float y)
end

let alloc = ref 0.0

let measure_alloc f =
  (* NB: right-to-left evaluation order gets this right *)
  let baseline_allocation = Gc.allocated_bytes() -. Gc.allocated_bytes() in
  let before = Gc.allocated_bytes () in
  let result = f () in
  let after = Gc.allocated_bytes () in
  alloc := (after -. before) -. baseline_allocation;
  result

module Pi_unboxed =
struct
  open Float_u

  let step n estimate =
    let new_term =
      ((of_float (-1.)) ** n)
      / ((n * (of_float 2.)) + (of_float 1.))
    in
    estimate + new_term

  let rec go n est =
    if n > (of_float 10000.) then est else go (n+ (of_float 1.)) (step n est)

  let estimate () =
    let est =
      measure_alloc (fun () -> go (of_float 0.) (of_float 0.))
    in
    Printf.printf "Unboxed:\n  estimate: %f\n  bytes allocated: %f\n"
      ((to_float est) *. 4.) !alloc
end

module Pi_boxed =
struct
  let step n estimate =
    let new_term =
      (-1. ** n) /. ((n *. 2.) +. 1.)
    in
    estimate +. new_term

  let rec go n est =
    if n > 10000. then est else go (n+.1.) (step n est)

  let estimate () =
    let est =
      measure_alloc (fun () -> Float_u.of_float (go 0. 0.))
    in
    Printf.printf "Boxed:\n  estimate: %f\n  bytes allocated: %f\n"
      ((Float_u.to_float est) *. 4.) !alloc
end

let _ = Pi_unboxed.estimate ()
let _ = Pi_boxed.estimate ()
