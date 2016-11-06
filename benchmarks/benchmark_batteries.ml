module Benchmark_batteries = struct
   open Benchmark_types
   open Benchmark_staged

   let sum = fun src -> .<
       .~src
       |> BatArray.fold_left (fun z a -> z + a) 0 >.

   let sumOfSquares = fun src -> .<
      .~src
      |> BatArray.enum
      |> BatEnum.map(fun x -> x * x)
      |> BatEnum.fold (fun z a -> z + a) 0 >.

(*
   let maps_array = fun src -> .<
      .~src
      |> BatArray.map(fun x -> x * 1)
      |> BatArray.map(fun x -> x * 2)
      |> BatArray.map(fun x -> x * 3)
      |> BatArray.map(fun x -> x * 4)
      |> BatArray.map(fun x -> x * 5)
      |> BatArray.map(fun x -> x * 6)
      |> BatArray.map(fun x -> x * 7)
      |> BatArray.fold_left (fun z a -> z + a) 0 >. *)

   let maps_enum = fun src -> .<
      .~src
      |> BatArray.enum
      |> BatEnum.map(fun x -> x * 1)
      |> BatEnum.map(fun x -> x * 2)
      |> BatEnum.map(fun x -> x * 3)
      |> BatEnum.map(fun x -> x * 4)
      |> BatEnum.map(fun x -> x * 5)
      |> BatEnum.map(fun x -> x * 6)
      |> BatEnum.map(fun x -> x * 7)
      |> BatEnum.fold (fun z a -> z + a) 0 >.

   (*
   let filters_array = fun src -> .<
      .~src
      |> BatArray.filter(fun x -> x > 1)
      |> BatArray.filter(fun x -> x > 2)
      |> BatArray.filter(fun x -> x > 3)
      |> BatArray.filter(fun x -> x > 4)
      |> BatArray.filter(fun x -> x > 5)
      |> BatArray.filter(fun x -> x > 6)
      |> BatArray.filter(fun x -> x > 7)
      |> BatArray.fold_left (fun z a -> z + a) 0 >. *)

   let filters_enum = fun src -> .<
      .~src
      |> BatArray.enum
      |> BatEnum.filter(fun x -> x > 1)
      |> BatEnum.filter(fun x -> x > 2)
      |> BatEnum.filter(fun x -> x > 3)
      |> BatEnum.filter(fun x -> x > 4)
      |> BatEnum.filter(fun x -> x > 5)
      |> BatEnum.filter(fun x -> x > 6)
      |> BatEnum.filter(fun x -> x > 7)
      |> BatEnum.fold (fun z a -> z + a) 0 >.

   let sumOfSquaresEven = fun src -> .<
      .~src
      |> BatArray.enum
      |> BatEnum.filter(fun e -> e mod 2 = 0)
      |> BatEnum.map(fun x -> x * x)
      |> BatEnum.fold (fun z a -> z + a) 0 >.

   let cart : (int array code * int array code) -> int code = fun (src1, src2) -> .<
      .~src1
      |> BatArray.enum
      |> BatEnum.concat_map (fun d -> .~src2 |> BatArray.enum |> BatEnum.map (fun dp -> d * dp))
      |> BatEnum.fold (fun z a -> z + a) 0 >.

   let dotProduct_enum = fun (src1, src2) -> .<
      BatEnum.fold2 (fun z x y -> z + x * y) 0
                    (.~src1 |> BatArray.enum)
                    (.~src2 |> BatArray.enum) >.

   let dotProduct = fun (src1, src2) -> .<
      BatStream.map2 (fun e1 e2 -> e1 * e2)
         (.~src1
          |> BatArray.enum
          |> BatStream.of_enum)
         (.~src2
          |> BatArray.enum
          |> BatStream.of_enum)
     |> BatStream.enum
     |> BatEnum.fold (fun z a -> z + a) 0 >.

   let flatMap_after_zipWith = fun (src1, src2) -> .<
     BatStream.map2 (fun e1 e2 -> e1 + e2)
         (.~src1
          |> BatArray.enum
          |> BatStream.of_enum)
         (.~src1
          |> BatArray.enum
          |> BatStream.of_enum)
     |> BatStream.concat_map (fun x ->
       .~src2
       |> BatArray.enum
       |> BatStream.of_enum
       |> BatStream.map (fun el -> el + x))
     |> BatStream.enum
     |> BatEnum.fold (fun z a -> z + a) 0 >.

   let zipWith_after_flatMap = fun (src1, src2) -> .<
     .~src1
     |> BatArray.enum
     |> BatEnum.concat_map (fun x ->
       .~src2
       |> BatArray.enum
       |> BatEnum.map (fun el -> el + x))
     |> BatStream.of_enum
     |> BatStream.map2 (fun e1 e2 -> e1 + e2)
            (.~src1
             |> BatArray.enum
             |> BatStream.of_enum)
     |> BatStream.enum
     |> BatEnum.fold (fun z a -> z + a) 0 >.

   let flat_map_take = fun (arr1, arr2) -> .<
       .~arr1
         |> BatArray.enum
         |> BatEnum.concat_map (fun x -> .~arr2 |> BatArray.enum |> BatEnum.map (fun y -> x * y))
         |> BatEnum.take 20000000
         |> BatEnum.fold (fun z a -> z + a) 0 >.

   let v = .< Array.init 100000000 (fun i -> i mod 10) >.;;
   let vHi = .< Array.init 10000000 (fun i -> i mod 10) >.;;
   let vLo = .< Array.init 10 (fun i -> i mod 10) >.;;
   let vFaZ = .< Array.init 10000 (fun i -> i) >.;;
   let vZaF = .< Array.init 10000000 (fun i -> i) >.;;

   let options = {
      repetitions = 15.0;
      final_f = (fun _ -> .<()>.);
   };;

   let script =[|
      perfS  "sum_batteries" v (fun x -> sum .< .~x >.) options;
      perfS  "sumOfSquares_batteries" v (fun x -> sumOfSquares .< .~x >.) options;
      perfS  "sumOfSquaresEven_batteries" v (fun x -> sumOfSquaresEven .< .~x >.) options;
      perfS  "maps_enum_batteries" v (fun x -> maps_enum .< .~x >.) options;
      perfS  "filters_enum_batteries" v (fun x -> filters_enum .< .~x >.) options;
      perfS2 "cart_batteries" vHi vLo (fun (x, y) -> cart (.<.~x >. , .<.~y >.)) options;
      perfS2 "dotProduct_enum_batteries" vHi vHi (fun (x, y) -> dotProduct_enum (.<.~x >. , .<.~y >.)) options;
      perfS2 "dotProduct_batteries" vHi vHi (fun (x, y) -> dotProduct (.<.~x >. , .<.~y >.)) options;
      perfS2 "flatMap_after_zipWith_batteries" vFaZ vFaZ (fun (x, y) -> flatMap_after_zipWith (.<.~x >. , .<.~y >.)) options;
      perfS2 "zipWith_after_flatMap_batteries" vZaF vZaF (fun (x, y) -> zipWith_after_flatMap (.<.~x >. , .<.~y >.)) options;
      perfS2 "flat_map_take_batteries" v vLo  (fun (x, y) -> flat_map_take (.<.~x >. , .<.~y >.)) options
   |];;
end

let _ = Benchmark_staged.run_script Benchmark_batteries.script
