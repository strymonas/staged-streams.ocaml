open OUnit2
open Stream_combinators

(* Use it when OUnit is not installed.
let assert_equal x y = assert (x = y)
;;
*)

let test1 =
  of_arr .<[|1;2;3|]>.
  |> fold (fun z a -> .<.~z + .~a>.) .<0>.

let test2 =
  of_arr .<[|1.0;2.0;3.0|]>.
  |> map (fun x -> .<truncate .~x>.)
  |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

let test3 =
  of_arr .<[|1.0;2.0;3.0;4.0|]>.
  |> map (fun x -> .<truncate .~x>.)
  |> filter (fun x -> .<.~x mod 2 = 0>.)
  |> map (fun x -> .<.~x * .~x>.)
  |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

let test4 =
  of_arr .<[|1.0;2.0;3.0;4.0;5.0;6.0;7.0|]>.
  |> map (fun x -> .<truncate .~x>.)
  |> filter (fun x -> .<.~x mod 2 = 0>.)
  |> take .<2>.
  |> map (fun x -> .<.~x * .~x>.)
  |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

(*
let 20 = (Runcode.run test4);;
*)

let test5 =
  of_arr .<[|0;1;2;3;4;5|]>.
  |> filter (fun x -> .<.~x mod 2 = 0>.)
  |> flat_map (fun x -> of_arr .<[|.~x; .~x+1; .~x+2|]>.)
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
  |> (fun l -> .<List.rev .~l>.);;

let iota n = unfold (fun n -> .<Some (.~n,.~n+1)>.) n;;

let test7 =
  iota .<1>.
  |> flat_map (fun x -> iota x) (* Infinite inner stream *)
  |> filter (fun x -> .<.~x mod 2 = 0>.)
  |> take .<10>.
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
  |> (fun l -> .<List.rev .~l>.);;

(*
let [2; 4; 6; 8; 10; 12; 14; 16; 18; 20] =(Runcode.run test7) ;;
*)

let test8 =
  iota .<1>.
  |> flat_map (fun x -> iota .<.~x+1>. |> take .<3>.)
  |> filter (fun x -> .<.~x mod 2 = 0>.)
  |> take .<10>.
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
  |> (fun l -> .<List.rev .~l>.);;

(*
let [2; 3] = (Runcode.run test8') ;;
*)
let test8' =
    iota .<1>.
    |> flat_map (fun x -> iota .<.~x * 1>.
                          |> flat_map (fun x -> iota .<.~x * 2>.)
                          |> take .< 3 >. )
    |> take .< 2 >.
    |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
    |> (fun l -> .<List.rev .~l>.);;


(*
let [2; 4; 4; 4; 6; 6; 6; 8; 8; 8] = (Runcode.run test8) ;;
*)

let test9 =
  iota .<1>.
  |> flat_map (fun x -> iota .<.~x+10>.
			|> flat_map (fun x -> of_arr .<[|.~x+100;.~x+200|]>.)
			|> filter (fun x -> .<.~x mod 3 = 0>.)
			|> take .<3>.)
  |> filter (fun x -> .<.~x mod 2 = 0>.)
  |> take .<10>.
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
  |> (fun l -> .<List.rev .~l>.);;

(*
let [114; 114; 216; 114; 216; 114; 216; 216; 216; 120] = (Runcode.run test9) ;;
*)

let testz0 =
  zip_with (fun e1 e2 -> .<.~e1 * .~e2>.)
	   (of_arr .<[|1;2;3;4;5;6|]>.)
	   (iota .<1>.)
  |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

(*
let 91 = Runcode.run testz0
*)


(* add zips when one stream is infinite; zips with two flattened maps;
   and perhaps one is infinite
 *)
let testz1 =
  (zip_with (fun e1 e2 -> .<(.~e1,.~e2)>.)
	    (
	      of_arr .<[|1.0;2.0;3.0;4.0;5.0;6.0|]>.
	      |> map (fun x -> .<truncate .~x>.)
	      |> take .<5>.
	      |> filter (fun x -> .<.~x mod 2 = 0>.)
	      |> map (fun x -> .<.~x * .~x>.))
	    (
	      iota .<10>.
	      |> map (fun x -> .<float_of_int (.~x * .~x)>.))
  )
  (* result *)
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.;;

(*
let [(16, 121.); (4, 100.)] = (Runcode.run testz1)
*)

(* switching the order of streams *)
let testz1' =
  (zip_with (fun e1 e2 -> .<(.~e2,.~e1)>.)
	    (
	      iota .<10>.
	      |> map (fun x -> .<float_of_int (.~x * .~x)>.))
	    (
	      of_arr .<[|1.0;2.0;3.0;4.0;5.0;6.0|]>.
	      |> map (fun x -> .<truncate .~x>.)
	      |> take .<5>.
	      |> filter (fun x -> .<.~x mod 2 = 0>.)
	      |> map (fun x -> .<.~x * .~x>.))
  )
  (* result *)
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.;;
(*
let [(16, 121.); (4, 100.)] = (Runcode.run testz1');;
*)


(* Filters and counters on both streams *)
let testz2 =
  (zip_with (fun e1 e2 -> .<(.~e1,.~e2)>.)
	    (
	      of_arr .<[|1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0|]>.
	      |> map (fun x -> .<truncate .~x>.)
	      |> take .<10>.
	      |> filter (fun x -> .<.~x mod 2 = 0>.)
	      |> map (fun x -> .<.~x * .~x>.))
	    (
	      iota .<20>.
	      |> filter (fun x -> .<.~x mod 3 = 0>.)
	      |> map (fun x -> .<float_of_int (.~x * .~x)>.)
	      |> take .<3>.)
  )
  (* result *)
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.;;

(*
let [(36, 729.); (16, 576.); (4, 441.)] = (Runcode.run testz2) ;;
*)



let testz3 =
  (zip_with (fun e1 e2 -> .<(.~e1,.~e2)>.)
	    (
	      of_arr .<[|1.0;2.0;3.0;4.0;5.0;6.0|]>.
	      |> take .<5>.)
	    (
	      iota .<1>.
	      |> flat_map (fun x -> iota .<.~x+1>. |> take .<3>.))
  )
  (* result *)
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.;;

(*
let [(5., 4); (4., 3); (3., 4); (2., 3); (1., 2)] = (Runcode.run testz3) ;;
*)

let testz3' =
  (zip_with (fun e1 e2 -> .<(.~e2,.~e1)>.)
	    (
	      iota .<1>.
	      |> flat_map (fun x -> iota .<.~x+1>. |> take .<3>.))
	    (
	      of_arr .<[|1.0;2.0;3.0;4.0;5.0;6.0|]>.
	      |> take .<5>.)
  )
  (* result *)
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.;;

(*
let [(5., 4); (4., 3); (3., 4); (2., 3); (1., 2)] = (Runcode.run testz3') ;;
*)

let testz4 =
  (zip_with (fun e1 e2 -> .<(.~e1,.~e2)>.)
	    (
	      of_arr .<[|1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0;9.0;10.0|]>.
	      |> map (fun x -> .<truncate .~x>.)
	      |> take .<12>.
	      |> filter (fun x -> .<.~x mod 2 = 0>.)
	      |> map (fun x -> .<.~x * .~x>.))
	    (
	      iota .<1>.
	      |> flat_map (fun x -> iota .<.~x+1>. |> take .<3>.)
	      |> filter (fun x -> .<.~x mod 2 = 0>.))
  )
  (* result *)
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.;;

(*
let [(100, 6); (64, 4); (36, 4); (16, 4); (4, 2)] = (Runcode.run testz4)  ;;
*)

(* Nested zips *)
let testz5 =
  (zip_with (fun e1 e2 -> .<(.~e1,.~e2)>.)
	    (
	      of_arr .<[|1.0;2.0;3.0;4.0;5.0;6.0|]>.
	      |> take .<5>.)
	    (zip_with (fun e1 e2 -> .<(.~e1,.~e2)>.)
		      (
			of_arr .<[|1.0;2.0;3.0;4.0;5.0;6.0|]>.
			|> take .<12>.)
		      (
			iota .<1>.
			|> flat_map (fun x -> iota .<.~x+1>. |> take .<3>.))
	    ))
  (* result *)
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.;;

(*
let [(5., (5., 4)); (4., (4., 3)); (3., (3., 4)); (2., (2., 3)); (1., (1., 2))]
 = (Runcode.run testz5)  ;;
*)


let testz6 = zip_with (fun x y -> .<.~x + .~y>.)
        (zip_with (fun x y -> .<.~x * .~y>.)
          (of_arr .<[|0;1;2;3;4|]>.
           |> map (fun a -> .<.~a * 1>.))
          (of_arr .<[|0;1;2;3|]>.))
        (zip_with (fun x y -> .<.~x / .~y>.)
          (of_arr .<[|0;1;2;3;4|]>.
           |> map (fun a -> .<.~a * 2>.))
          (of_arr .<[|1;2;3|]>.))
       |> map (fun a -> .<1 + .~a>.)
       |> fold (fun x y -> .<.~x + .~y>.) .<0>.
;;

(*
let 10 = (Runcode.run testz6)  ;;
*)

let testz7 = zip_with (fun x y -> .<.~x + .~y>.)
        (zip_with (fun x y -> .<.~x * .~y>.)
          (of_arr .<[|-1;0;-1;1;-1;2;3;4|]>.
           |> filter (fun a -> .<.~a >= 0 >.))
          (of_arr .<[|0;1;10;2;3|]>. |> filter (fun a -> .<.~a < 10 >.)))
        (zip_with (fun x y -> .<.~x / .~y>.)
          (of_arr .<[|-1;-1;-1;0;1;-1;-1;2;3;4|]>.
           |> map (fun a -> .<.~a * 2>.)
           |> filter (fun a -> .<.~a >= 0>.))
          (of_arr .<[|1;2;3|]>.))
       |> map (fun a -> .<1 + .~a>.)
       |> fold (fun x y -> .<.~x + .~y>.) .<0>.
;;

(*
let 10 = (Runcode.run testz7)  ;;
*)

(* The most complex example: zipping of deeply nested streams *)
let testxx =
 (of_arr .<[|0;1;2;3|]>.
       |> flat_map (fun e -> of_arr .<[|.~e; .~e+1|]>.)
       |> flat_map (fun e -> of_arr .<[|.~e; .~e+1|]>.)
       |> filter (fun x -> .<.~x mod 2 = 0>.))
 |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
;;
(*
let [4; 4; 4; 2; 2; 2; 2; 0] = Runcode.run testxx;;
*)

let testyy =
 (of_arr .<[|1;2;3|]>.
       |> flat_map (fun e -> of_arr .<[|.~e; .~e+1|]>.)
       |> filter (fun x -> .<.~x mod 2 = 0>.)
       |> flat_map (fun e -> of_arr .<[|.~e; .~e+1|]>.))
 |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.

;;
(*
let [5; 4; 3; 2; 3; 2] = Runcode.run testyy;;
*)

(*
let testz8 = zip_with (fun x y -> .<(.~x, .~y)>.)
 (of_arr .<[|0;1;2;3|]>.
       |> flat_map (fun e -> of_arr
             .<[|(Printf.printf "s1 %d\n" .~e ; .~e); .~e+1|]>.)
       |> flat_map (fun e -> of_arr
             .<[|(Printf.printf "s2 %d\n" .~e ; .~e); .~e+1|]>.)
       |> filter (fun x -> .<(Printf.printf "s3 %d\n" .~x; .~x mod 2 = 0)>.))
 (of_arr .<[|1;2;3;4;5;6;7;8;9;10|]>.
       |> filter (fun x -> .<.~x mod 1 = 0>.))
 |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
*)

let testz8 = zip_with (fun x y -> .<(.~x, .~y)>.)
 (of_arr .<[|0;1;2;3|]>.
       |> flat_map (fun e -> of_arr .<[|.~e; .~e+1|]>.)
       |> flat_map (fun e -> of_arr .<[|.~e; .~e+1|]>.)
       |> filter (fun x -> .<.~x mod 2 = 0>.))
 (of_arr .<[|1;2;3|]>.
       |> flat_map (fun e -> of_arr .<[|.~e; .~e+1|]>.)
       |> filter (fun x -> .<.~x mod 2 = 0>.)
       |> flat_map (fun e -> of_arr .<[|.~e; .~e+1|]>.))
 |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
;;

(*
let [(4, 5); (2, 4); (2, 3); (2, 2); (2, 3); (0, 2)]
 = Runcode.run testz8;;
*)

(* Example from the paper *)
let testc = zip_with (fun e1 e2 -> .<(.~e1,.~e2)>.)
 (* First stream to zip *)
 (of_arr .<[|0;1;2;3|]>.
   |> map (fun x -> .<.~x * .~x>.)
   |> take .<12>.
   |> filter (fun x -> .<.~x mod 2 = 0>.)
   |> map (fun x -> .<.~x * .~x>.))
 (* Second stream to zip *)
 (iota .<1>.
   |> flat_map (fun x -> iota .<.~x+1>. |> take .<3>.)
   |> filter (fun x -> .<.~x mod 2 = 0>.))
 |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
;;

(*
let [(16, 4); (0, 2)] = (Runcode.run testc)  ;;
*)

let enumFromTo x y =
 unfold (fun n -> .<if .~n <= .~y then Some (.~n,.~n+1) else None>.) x;;

let test10 =
  enumFromTo .<1>. .<5>.
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
  |> (fun l -> .<List.rev .~l>.);;

(*
let [1; 2; 3; 4; 5] = (Runcode.run test10) ;;
*)

let test10' =
  enumFromTo .<1>. .<5>.
  |> flat_map (fun x -> enumFromTo .<1>. x)
  |> fold (fun z a -> .<.~a :: .~z>.) .<[]>.
  |> (fun l -> .<List.rev .~l>.);;

let minmax : ('a code * 'a code) -> 'a stream -> ('a * 'a) code =
  fun initminmax str ->
   .< let mins = ref .~(fst initminmax) and
          maxs = ref .~(snd initminmax) in
          .~(fold_tupled (fun a z -> .<min .~a .~z>.) .<!mins>. (fun a z -> .<max .~a .~z>.) .<!maxs>. str) >.

let test11 =
   of_arr .<[|0;3;2;5;1|]>.
   |> minmax (.<100>.,.<-1>.);;

let testzff =
  zip_with (fun e1 e2 -> .<.~e1 * .~e2>.)
	   (of_arr .<[|1;2;3;4;5|]>. |> filter (fun x -> .<.~x > 2>.))
	   (of_arr .<[|1;2;3;4;5|]>. |> filter (fun x -> .<.~x < 3>.))
  |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;


(* let (0,5) = Runcode.run test11;; *)

let suite = "suite" >:::
["fold_test"                              >:: (fun ctx -> assert_equal (Runcode.run test1)  6);
 "map_fold_test"                          >:: (fun ctx -> assert_equal (Runcode.run test2)  6);
 "map_filter_map_fold_test"               >:: (fun ctx -> assert_equal (Runcode.run test4)  20);
 "map_filter_take_map_fold_test"          >:: (fun ctx -> assert_equal (Runcode.run test2)  6);
 "flat_map_revert_test"                   >:: (fun ctx -> assert_equal (Runcode.run test5)  [0; 1; 2; 2; 3; 4; 4; 5; 6]);
 "flat_map_take_infinite_test"            >:: (fun ctx -> assert_equal (Runcode.run test7)  [2; 4; 6; 8; 10; 12; 14; 16; 18; 20]);
 "flat_map_takes_infinite_test"           >:: (fun ctx -> assert_equal (Runcode.run test8)  [2; 4; 4; 4; 6; 6; 6; 8; 8; 8]);
 "flat_map_takes_infinite_nested_test"    >:: (fun ctx -> assert_equal (Runcode.run test8') [2; 3]);
 "double_nested_test"                     >:: (fun ctx -> assert_equal (Runcode.run test9)  [114; 114; 216; 114; 216; 114; 216; 216; 216; 120]);
 "dot_product_test"                       >:: (fun ctx -> assert_equal (Runcode.run testz0)  91);
 "zip_finites_take_and_infinite_test"     >:: (fun ctx -> assert_equal (Runcode.run testz1)  [(16, 121.); (4, 100.)]);
 "zip_finites_take_and_infinite_rev_test" >:: (fun ctx -> assert_equal (Runcode.run testz1') [(16, 121.); (4, 100.)]);
 "zip_with_filters_counters_test"         >:: (fun ctx -> assert_equal (Runcode.run testz2)  [(36, 729.); (16, 576.); (4, 441.)]);
 "zip_with_take_test"                     >:: (fun ctx -> assert_equal (Runcode.run testz3)  [(5., 4); (4., 3); (3., 4); (2., 3); (1., 2)]);
 "zip_with_take_reverse_test"             >:: (fun ctx -> assert_equal (Runcode.run testz3') [(5., 4); (4., 3); (3., 4); (2., 3); (1., 2)]);
 "zip_with_take_test1"                    >:: (fun ctx -> assert_equal (Runcode.run testz4)  [(100, 6); (64, 4); (36, 4); (16, 4); (4, 2)]);
 "zip_with_take_test2"                    >:: (fun ctx -> assert_equal (Runcode.run testz5)  [(5., (5., 4)); (4., (4., 3)); (3., (3., 4)); (2., (2., 3)); (1., (1., 2))]);
 "zip_multiple1"                          >:: (fun ctx -> assert_equal (Runcode.run testz6)  10);
 "zip_multiple2"                          >:: (fun ctx -> assert_equal (Runcode.run testz7)  10);
 "zip_deeply_nested_xx"                   >:: (fun ctx -> assert_equal (Runcode.run testxx)  [4; 4; 4; 2; 2; 2; 2; 0]);
 "zip_filter_filter"                      >:: (fun ctx -> assert_equal (Runcode.run testzff) 11); 
 "zip_deeply_nested_yy"                   >:: (fun ctx -> assert_equal (Runcode.run testyy)  [5; 4; 3; 2; 3; 2]);
 "zip_deeply_nested_final"                >:: (fun ctx -> assert_equal (Runcode.run testz8) [(4, 5); (2, 4); (2, 3); (2, 2); (2, 3); (0, 2)]);
 "zip_paper"                              >:: (fun ctx -> assert_equal (Runcode.run testc)   [(16, 4); (0, 2)]);
 "enumFromTo"                             >:: (fun ctx -> assert_equal (Runcode.run test10)  [1; 2; 3; 4; 5]);
 "non-constant_flat_maps"                 >:: (fun ctx -> assert_equal (Runcode.run test10')  [1; 1; 2; 1; 2; 3; 1; 2; 3; 4; 1; 2; 3; 4; 5]);
 "fused fold/tuple"                       >:: (fun ctx -> assert_equal (Runcode.run test11)  (0,5));
];;

let () = run_test_tt_main suite;;
