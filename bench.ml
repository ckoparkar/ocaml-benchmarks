let median (ls : 'a list) =
  let n = List.length ls in
  let idx = n / 2 in
  let ls2 = List.sort Float.compare ls in
  List.nth ls2 idx

let dotrial f arg =
  let t0 = Unix.gettimeofday () in
  let result = f arg in
  let t1 = Unix.gettimeofday () in
  let diff = Float.sub t1 t0 in
  let _ = print_endline (Printf.sprintf "iter time: %.4f" diff) in
  (result, diff)

let bench iters f arg =
  let tups = List.map (fun _ -> dotrial f arg) (List.init iters (fun i -> i)) in
  let (result, times) = List.split tups in
  let batch = List.fold_right Float.add times 0.0 in
  (List.nth result 0, batch, median times)

let print_bench msg iters f arg =
  let (result, batch, t) = bench iters f arg in
  let _ = print_endline msg  in
  let _ = print_endline ("ITERS: " ^ string_of_int iters) in
  let _ = print_endline ("SIZE: " ^ string_of_int arg) in
  let _ = print_endline ("BATCHTIME: " ^ (Printf.sprintf "%.4f" batch)) in
  let _ = print_endline ("SELFTIMED: " ^ (Printf.sprintf "%.4f" t)) in
  result
