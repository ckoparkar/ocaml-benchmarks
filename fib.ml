module T = Domainslib.Task

let rec fib n =
  if n < 2 then n
  else fib (n-1) + fib (n-2)

let rec fib_par pool n =
  if n <= 30 then fib n
  else
    let a_f = T.async pool (fun _ -> fib_par pool (n-1)) in
    let b = fib_par pool (n-2) in
    T.await pool a_f + b
