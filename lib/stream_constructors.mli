(* Functional stream *)
type 'a stream_res = 
    | Done 
    | Cons of 'a * 'a stream 
    | Skip of 'a stream
  and 'a stream = unit -> 'a stream_res

type ('a,'b) either = Left of 'a | Right of 'b
