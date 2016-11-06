(* Public interface of the combinator library *)

type 'a stream

(* Producers *)
val of_arr : 'a array code -> 'a stream
val unfold : ('z code -> ('a * 'z) option code) -> 'z code -> 'a stream

(* Consumer *)
val fold : ('z code -> 'a code -> 'z code) -> 'z code -> 'a stream -> 'z code
val fold_tupled : ('z1 code -> 'a code -> 'z1 code) -> 'z1 code ->
                  ('z2 code -> 'a code -> 'z2 code) -> 'z2 code ->
                  'a stream -> ('z1 * 'z2) code

(* Transformers *)
val map      : ('a code -> 'b code) -> 'a stream -> 'b stream
val filter   : ('a code -> bool code) -> 'a stream -> 'a stream
val take     : int code -> 'a stream -> 'a stream
val flat_map : ('a code -> 'b stream) -> 'a stream -> 'b stream
val zip_with : ('a code -> 'b code -> 'c code) ->
               ('a stream -> 'b stream -> 'c stream)
