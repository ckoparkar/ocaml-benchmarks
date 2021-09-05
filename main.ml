open Fib
open Bintree
open Kdtree
open Coins
open Countnodes
open Mergesort
open Foldconstants
open Compiler

open Common
open Checks

module A = Array

let perform_bench pool prog size iters arr_input =
  match prog with
    "seqfib" ->
     let n = Bench.print_bench prog iters fib size in
     let _ = print_endline (string_of_int n) in
     ()

  | "parfib" ->
     let n = Bench.print_bench prog iters (fib_par pool) size in
     let _ = print_endline (string_of_int n) in
     ()

  | "seqbuildfib" ->
     let tr = Bench.print_bench prog iters buildfib_seq size in
     check_buildfib size tr

  | "parbuildfib" ->
     let cutoff = 6 in
     let tr = Bench.print_bench prog iters (buildfib_par pool cutoff) size in
     check_buildfib size tr

  | "seqbuildkdtree" ->
     let arr = read3DArrayFile arr_input in
     let _ = print_endline "file parsed" in
     let tr = Bench.print_bench prog iters (fun _ -> fromList_seq arr) size in
     check_buildkdtree arr tr

  | "parbuildkdtree" ->
     let arr = read3DArrayFile arr_input in
     let _ = print_endline "file parsed" in
     let cutoff = 32000 in
     let tr = Bench.print_bench prog iters (fun _ -> fromList_par pool cutoff arr) size in
     check_buildkdtree arr tr

  | "seqcountcorr" ->
     let arr = read3DArrayFile arr_input in
     let _ = print_endline "file parsed" in
     let tr = fromList_seq arr in
     let radius = 100.0 in
     let arr' = Array.sub arr 0 size in
     let counts = Bench.print_bench prog iters
                    (fun _ -> allCountCorr_seq radius tr arr') size in
     let query = Array.get arr' 4 in
     let actual = Array.get counts 4 in
     check_countcorr arr query actual radius

  | "parcountcorr" ->
     let arr = read3DArrayFile arr_input in
     let _ = print_endline "file parsed" in
     let tr = fromList_seq arr in
     let cutoff = 8000 in
     let radius = 100.0 in
     let arr' = Array.sub arr 0 size in
     let counts = Bench.print_bench prog iters
                    (fun _ -> allCountCorr_par pool cutoff radius tr arr') size in
     let query = Array.get arr' 4 in
     let actual = Array.get counts 4 in
     check_countcorr arr query actual radius

  | "seqnearest" ->
     let arr = read3DArrayFile arr_input in
     let _ = print_endline "file parsed" in
     let tr = fromList_seq arr in
     let res = Bench.print_bench prog iters (fun _ -> allNearestNeighbors_seq tr arr) size in
     check_nearest arr res

  | "parnearest" ->
     let arr = read3DArrayFile arr_input in
     let _ = print_endline "file parsed" in
     (* let tr = fromList_seq arr in *)
     let tr = fromList_par pool 100000 arr in
     let res = Bench.print_bench prog iters (fun _ -> allNearestNeighbors_par pool tr arr) size in
     check_nearest arr res

  | "seqcoins" ->
     let ls = Bench.print_bench prog iters (fun amt -> pay_seq amt coins_input) size in
     check_coins size ls

  | "parcoins" ->
     let ls = Bench.print_bench prog iters (fun amt -> pay_par pool 3 amt coins_input) size in
     check_coins size ls

  | "seqcountnodes" ->
     let e = Parse_grammar.parse_file arr_input in
     let _ = print_endline ("file parsed") in
     let n = Bench.print_bench prog iters (fun _ -> countnodes e) size in
     check_countnodes n

  | "parcountnodes" ->
     let e = Parse_grammar.parse_file arr_input in
     let _ = print_endline ("file parsed") in
     let n = Bench.print_bench prog iters (fun _ -> par_countnodes pool e) size in
     check_countnodes n

  | "seqmergesort" ->
     let _ = Random.init size in
     let arr = A.init size (fun i -> Random.float(Float.of_int(size) *. 2.0)) in
     (* let cmp = (fun i j -> Float.to_int (i -. j)) in *)
     let cmp = Float.compare in
     let sorted = Bench.print_bench prog iters (fun _ -> mergesort_seq cmp arr) size in
     check_sorted cmp arr sorted

  | "parmergesort" ->
     let _ = Random.init size in
     let arr = A.init size (fun i -> Random.float(Float.of_int(size) *. 2.0)) in
     let cmp = Float.compare in
     let sorted = Bench.print_bench prog iters (fun _ -> mergesort pool cmp arr) size in
     (* let _ = print_float_array sorted in *)
     check_sorted cmp arr sorted

  | "seqfoldconstants" ->
     let tr = build_exp size in
     let tr1 = Bench.print_bench prog iters (fun _ -> fold_constants tr) size in
     let n = sum_exp tr1
     in print_endline (string_of_int n)

  | "parfoldconstants" ->
     let tr = build_exp size in
     let tr1 = Bench.print_bench prog iters (fun _ -> fold_constants_par pool 0 tr) size in
     let n = sum_exp tr1
     in print_endline (string_of_int n)

  | "seqcompiler" ->
     let ex = make_big_ex size 0 in
     let prg = ProgramA (intTy, ex) in
     let compiled = Bench.print_bench prog iters (fun _ -> compile prg) size in
     let _  = print_pseudox86 compiled in
     ()

  | "parcompiler" ->
     let ex = make_big_ex size 0 in
     let prg = ProgramA (intTy, ex) in
     let compiled = Bench.print_bench prog iters (fun _ -> compile_par pool prg) size in
     let _  = print_pseudox86 compiled in
     ()


  | _ -> raise (Failure ("Unknown program: " ^ prog))


let () =
  let prog = ref "seqfib" in
  let size = ref 9 in
  let iters = ref 3 in
  let arr_input = ref "Missing input file" in
  let max_cores = ref 1 in
  let speclist = [("-p", Arg.String (fun a -> prog := a), "Benchmark to run.");
                  ("-n", Arg.Int (fun a -> size := a), "The size param.");
                  ("-i", Arg.Int (fun a -> iters := a), "The iters param.");
                  ("-a", Arg.String (fun a -> arr_input := a), "Array input.");
                  ("-j", Arg.Int (fun a -> max_cores := a), "Maximum cores to use.");
                 ] in
  let usage_msg = "Ocaml benchmarks. Options:" in
  let _ = Arg.parse speclist print_endline usage_msg in
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains:(!max_cores - 1) in
  let _ = perform_bench pool !prog !size !iters !arr_input in
  let _ = T.teardown_pool pool in
  ()
