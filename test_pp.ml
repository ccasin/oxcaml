type ('a : any) t = nativeint#

external read : ('a : any). 'a t -> 'a = "%peek"
  [@@layout_poly]

external write : ('a : any). 'a t -> 'a -> unit = "%poke"
  [@@layout_poly]

let test_read p
  : #(int * int32# * int64# * nativeint# * float# * float32#) =
  #(read p, read p, read p, read p, read p, read p)

let test_write p f =
  write p 0;
  write p #0l;
  write p #0L;
  write p #0n;
  write p #0.;
  write p #0.s
