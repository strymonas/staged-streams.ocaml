open Runcode
open Benchmark_types
open Oml.Statistics.Distributions
open Oml.Statistics.Descriptive

let perfS : string -> 'a code -> ('a code -> 'b code) -> 'b benchmark_options -> 'b code =
  fun benchmark init f options ->
    let reps = options.repetitions in
    let final = options.final_f in
    .< let x = .~init in

       let mean_error confidence iterations arr =
         let a = (student_quantile ~degrees_of_freedom:(int_of_float @@ iterations -. 1.0) (1.0 -. ((1.0 -. confidence) /. 2.0))) in
         a *. (var_standard_error arr) /. (sqrt iterations) in

       let ret = ref .~(f (.<x>.)) in
       let measurements = Array.make (int_of_float reps) 0.0 in
       for i = 0 to (int_of_float reps)-1 do
          Gc.compact();
          let start_time = Sys.time () in
          let _ = .~(f .<x>.) in
          let end_time = Sys.time () in
          let elapsed_time = (end_time -. start_time) *. 1000.0 in
          measurements.(i) <- elapsed_time
       done;
       Printf.printf "%-30s %10.1f %10.1f %5.1f   ms/op\n%!" benchmark (mean measurements) (mean_error 0.95 reps measurements) (var_standard_error measurements);
       .~(final .<!ret>.);
       !ret >.;;

let perfS2 : string -> 'a code -> 'a code -> (('a code* 'a code) -> 'b code) -> 'b benchmark_options -> 'b code =
  fun benchmark init1 init2 f options ->
    let reps = options.repetitions in
    let final = options.final_f in

    .< let x = .~init1 in
       let y = .~init2 in

       (* TODO: can avoid CSP by transfering into separate module *)
       let mean_error confidence iterations arr =
        let a = (student_quantile ~degrees_of_freedom:(int_of_float @@ iterations -. 1.0) (1.0 -. ((1.0 -. confidence) /. 2.0))) in
        a *. (var_standard_error arr) /. (sqrt iterations) in

       let ret = ref .~(f (.<x>., .<y>.)) in
       let measurements = Array.make (int_of_float reps) 0.0 in
       for i = 0 to (int_of_float reps)-1 do
          Gc.compact();
          let start_time = Sys.time () in
          let _ = .~(f (.<x>., .<y>.)) in
          let end_time = Sys.time () in
          let elapsed_time = (end_time -. start_time) *. 1000.0 in
          measurements.(i) <- elapsed_time
       done;
       Printf.printf "%-30s %10.1f %10.1f %5.1f   ms/op\n%!" benchmark (mean measurements) (mean_error 0.95 reps measurements) (var_standard_error measurements);
       .~(final .<!ret>.);
       !ret >.;;

let write_code : string -> 'a code -> unit = fun file_name c ->
   (* make sure the code is closed *)
   (* let start_time = Sys.time () in *)
   let cde = close_code c in
   (* let end_time = Sys.time () in *)
   (* Printf.printf "closing code took: %5.1f ms\n%!" ((end_time -. start_time) *. 1000.0); *)
   let cout = open_out file_name in
   let ppf = Format.formatter_of_out_channel cout in
   (* print code *)
   (* let start_time = Sys.time () in *)
   let () = Print_code.format_code ppf cde in
   let () = Format.fprintf ppf "%!" in
   (* let end_time = Sys.time () in *)
   (* Printf.printf "emitting code took: %5.1f ms\n%!" ((end_time -. start_time) *. 1000.0); *)
   close_out cout;;

let run_natively : ?compiler:string -> 'a code -> unit =
   fun ?(compiler:string="ocamlfind ocamlopt -package oml -package batteries -linkpkg") c ->
      let fname = Filename.get_temp_dir_name() ^ "gen.ml" in write_code fname c;
      (* let start_time = Sys.time () in *)
      let retc = Sys.command (compiler ^ " " ^ fname) in
      (* let end_time = Sys.time () in *)
      (* Printf.printf "compilation took: %5.1f ms\n%!" ((end_time -. start_time) *. 1000.0); *)
      if retc = 0 then begin
         ignore (Sys.command "./a.out");
         Sys.remove fname;
         Sys.remove "./a.out"
      end;;

let run_script : 'a code array -> unit =
   Printf.printf "%-30s %10s %10s %5s %7s\n%!" "Benchmark" "Mean" "Mean-Error" "Sdev" "Unit";
   fun arr -> Array.iter (fun c -> run_natively c) arr;;
