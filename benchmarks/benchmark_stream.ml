module Benchmark_stream = struct
   open Stream_combinators
   open Runcode
   open Benchmark_types
   open Benchmark_staged

   let sumS : int array code -> int code
   = fun arr ->
      of_arr arr
      |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let sumShand : int array code -> int code
   = fun arr1 ->
     .< let sum = ref 0 in
        for counter1 = 0 to Array.length .~arr1 - 1 do
          sum := !sum + (.~arr1).(counter1);
        done;
        !sum >.;;

   let sumOfSquaresS : int array code -> int code
   = fun arr ->
       of_arr arr
       |> map (fun x -> .<.~x * .~x>.)
       |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let sumOfSquaresShand : int array code -> int code
   = fun arr1 ->
     .< let sum = ref 0 in
        for counter1 = 0 to Array.length .~arr1 - 1 do
          let item1 = (.~arr1).(counter1) in
          sum := !sum + item1*item1;
        done;
        !sum>.;;

   let mapsS : int array code -> int code
   = fun arr ->
      of_arr arr
      |> map (fun x -> .<.~x * 1>.)
      |> map (fun x -> .<.~x * 2>.)
      |> map (fun x -> .<.~x * 3>.)
      |> map (fun x -> .<.~x * 4>.)
      |> map (fun x -> .<.~x * 5>.)
      |> map (fun x -> .<.~x * 6>.)
      |> map (fun x -> .<.~x * 7>.)
      |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let maps_hand : int array code -> int code
   = fun arr1 ->
     .< let sum = ref 0 in
        for counter1 = 0 to Array.length .~arr1 - 1 do
        let item1 = (.~arr1).(counter1) in
          sum := !sum + item1*1*2*3*4*5*6*7;
        done;
        !sum>.;;

   let filtersS : int array code -> int code
   = fun arr ->
     of_arr arr
     |> filter (fun x -> .<.~x > 1>.)
     |> filter (fun x -> .<.~x > 2>.)
     |> filter (fun x -> .<.~x > 3>.)
     |> filter (fun x -> .<.~x > 4>.)
     |> filter (fun x -> .<.~x > 5>.)
     |> filter (fun x -> .<.~x > 6>.)
     |> filter (fun x -> .<.~x > 7>.)
     |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let filters_hand : int array code -> int code
   = fun arr1 ->
    .< let sum = ref 0 in
       for counter1 = 0 to Array.length .~arr1 - 1 do
       let item1 = (.~arr1).(counter1) in
       if (item1 > 1 && item1 > 2 && item1 > 3 &&
           item1 > 4 && item1 > 5 && item1 > 6 &&
           item1 > 7) then
       begin
         sum := !sum + item1;
       end;
       done;
       !sum>.;;

   let sumOfSquaresEvenS : int array code -> int code
   = fun arr ->
     of_arr arr
     |> filter (fun x -> .<.~x mod 2 = 0>.)
     |> map (fun x -> .<.~x * .~x>.)
     |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let sumOfSquaresEvenShand : int array code -> int code
   = fun arr1 ->
      .< let sum = ref 0 in
         for counter1 = 0 to Array.length .~arr1 - 1 do
         let item1 = (.~arr1).(counter1) in
         if item1 mod 2 = 0 then
         begin
           sum := !sum + item1*item1
         end;
         done;
         !sum>.;;

   let cartS : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
       of_arr arr1
       |> flat_map (fun x -> of_arr arr2
                              |> map (fun y -> .< .~x * .~y>.))
       |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let cartShand : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
        .< let sum = ref 0 in
           for counter1 = 0 to Array.length .~arr1 - 1 do
              let item1 = (.~arr1).(counter1) in
              for counter2 = 0 to Array.length .~arr2 - 1 do
                  let item2 = (.~arr2).(counter2) in
                  sum := !sum + item1 * item2;
              done;
           done;
           !sum >.;;

   let dotProductS : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
     zip_with (fun e1 e2 -> .<.~e1 * .~e2>.)
              (of_arr arr1) (of_arr arr2)
     |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let dotProductShand : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
     .< let sum = ref 0 in
        for counter = 0 to min (Array.length .~arr1)
                               (Array.length .~arr2) - 1 do
          let item1 = (.~arr1).(counter) in
          let item2 = (.~arr2).(counter) in
          sum := !sum + item1 * item2;
        done;
        !sum>.;;

   let flatMap_after_zipWithS : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
     zip_with (fun e1 e2 -> .<.~e1 + .~e2>.)
              (of_arr arr1) (of_arr arr1)
     |> flat_map (fun x -> of_arr arr2
                             |> map (fun el -> .<.~el + .~x>.))
     |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let flatMap_after_zipWithShand : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
     .< let sum = ref 0 in
        for counter1 = 0 to Array.length .~arr1 - 1 do
           let x = (.~arr1).(counter1) + (.~arr1).(counter1) in
           for counter2 = 0 to Array.length .~arr2 - 1 do
           let item2 = (.~arr2).(counter2) in
             sum := !sum + item2 + x;
           done;
        done;
        !sum>.;;

   let zipWith_after_flatMapS : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
     of_arr arr1
     |> flat_map (fun x -> of_arr arr2 |> map (fun y -> .<.~y + .~x>.))
     |> zip_with (fun e1 e2 -> .<.~e1 + .~e2>.) (of_arr arr1)
     |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let zipWith_after_flatMapShand : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
     .< let sum = ref 0 in
        let index1 = ref 0 in
        let index2 = ref 0 in
        let flag1 = ref ((!index1) <= ((Array.length .~arr1) - 1)) in
        while (!flag1) && ((!index2) <= ((Array.length .~arr2) - 1)) do
           let el2 = (.~arr2).(!index2) in
           incr index2;
           (let index_zip = ref 0 in
            while (!flag1) && ((!index_zip) <= ((Array.length .~arr1) - 1)) do
              let el1 = (.~arr1).(!index_zip) in
              incr index_zip;
              let elz = (.~arr1).(!index1) in
              incr index1;
              flag1 := ((!index1) <= ((Array.length .~arr1) - 1));
              sum := ((!sum) + (elz + el1 + el2))
              done)
           done;
        !sum>.;;

   let flat_map_takeS : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
        of_arr arr1
        |> flat_map (fun x -> of_arr arr2 |> map (fun y -> .< .~x * .~y>.))
        |> take .<20000000>.
        |> fold (fun z a -> .<.~z + .~a>.) .<0>.;;

   let flat_map_takeShand : (int array code * int array code) -> int code
   = fun (arr1, arr2) ->
    .< let counter1 = ref 0 in
       let counter2 = ref 0 in
       let sum = ref 0 in
       let n = ref 0 in
       let flag = ref true in
       let size1 = Array.length .~arr1 in
       let size2 = Array.length .~arr2 in
       while !counter1 < size1 && !flag do
          let item1 = (.~arr1).(!counter1) in
            while !counter2 < size2 && !flag do
              let item2 = (.~arr2).(!counter2) in
              sum := !sum + item1 * item2;
              counter2 := !counter2 + 1;
              n := !n + 1;
              if !n = 20000000 then
              flag := false
            done;
          counter2 := 0;
          counter1 := !counter1 + 1;
       done;
       !sum >.;;

   (* Arrays used for benchmarking *)
   let v     = .< Array.init 100000000 (fun i -> i mod 10) >.;;
   let vHi   = .< Array.init 10000000 (fun i -> i mod 10) >.;;
   let vLo   = .< Array.init 10 (fun i -> i mod 10) >.;;
   let vFaZ  = .< Array.init 10000 (fun i -> i) >.;;
   let vZaF  = .< Array.init 10000000 (fun i -> i) >.;;

   let options = {
      repetitions = 30.0;
      final_f = (fun _ -> .<()>.);
   };;

   let script =[|
      perfS  "sum_staged" v sumS options;
      perfS  "sum_baseline" v sumShand options;
      perfS  "sumOfSquares_staged" v sumOfSquaresS options;
      perfS  "sumOfSquares_baseline" v sumOfSquaresShand options;
      perfS  "sumOfSquaresEven_staged" v sumOfSquaresEvenS options;
      perfS  "sumOfSquaresEven_baseline" v sumOfSquaresEvenShand options;
      perfS  "maps_staged" v mapsS options;
      perfS  "maps_baseline" v maps_hand options;
      perfS  "filters_staged" v filtersS options;
      perfS  "filters_baseline" v filters_hand options;
      perfS2 "cart_staged" vHi vLo cartS	options;
      perfS2 "cart_baseline" vHi vLo cartShand options;
      perfS2 "dotProduct_staged" vHi vHi dotProductS options;
      perfS2 "dotProduct_baseline" vHi vHi dotProductShand options;
      perfS2 "flatMap_after_zipWith_staged" vFaZ vFaZ flatMap_after_zipWithS options;
      perfS2 "flatMap_after_zipWith_baseline" vFaZ vFaZ flatMap_after_zipWithShand options;
      perfS2 "zipWith_after_flatMap_staged" vZaF vZaF zipWith_after_flatMapS options;
      perfS2 "zipWith_after_flatMap_baseline" vZaF vZaF zipWith_after_flatMapShand options;
      perfS2 "flat_map_take_staged" v vLo flat_map_takeS options;
      perfS2 "flat_map_take_baseline" v vLo flat_map_takeShand options
   |];;
end;;

let _ = Benchmark_staged.run_script Benchmark_stream.script
