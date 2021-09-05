module T = Domainslib.Task

type tree = Leaf of int
          | Node of int * tree * tree

let rec buildtree_seq n =
  if n <= 0
  then Leaf 1
  else
    let x = buildtree_seq (n-1) in
    let y = buildtree_seq (n-1) in
    Node (n,x,y)

let rec buildtree_par pool cutoff n =
  if n <= cutoff
  then buildtree_seq n
  else if n <= 0
  then Leaf 1
  else
    let x_f = T.async pool (fun _ -> buildtree_par pool cutoff (n-1)) in
    let y = buildtree_par pool cutoff (n-1) in
    Node (n, T.await pool x_f, y)

let rec add1tree_seq tr =
  match tr with
    Leaf n -> Leaf (n+1)
  | Node (n,x,y) ->
     let x1 = add1tree_seq x in
     let y1 = add1tree_seq y in
     Node (n, x1, y1)

let rec add1tree_par pool cutoff tr =
  match tr with
    Leaf n -> Leaf (n+1)
  | Node (n,x,y) ->
     if n <= cutoff
     then let x1 = add1tree_seq x in
          let y1 = add1tree_seq y in
          Node (n+1, x1, y1)
     else let x1_f = T.async pool (fun _ -> add1tree_par pool cutoff x) in
          let y1 = add1tree_par pool cutoff y in
          Node (n, T.await pool x1_f, y1)

let rec sumtree_seq tr =
  match tr with
    Leaf n -> n
  | Node (n,x,y) -> sumtree_seq x + sumtree_seq y

let rec sumtree_par pool cutoff tr =
  match tr with
    Leaf n -> n
  | Node (n,x,y) ->
     if n <= cutoff
     then sumtree_seq tr
     else
       let n1_f = T.async pool (fun _ -> sumtree_par pool cutoff x) in
       let n2 = sumtree_par pool cutoff y in
       T.await pool n1_f + n2

let rec buildfib_seq n =
  if n <= 0
  then Leaf (Fib.fib 20)
  else
    let x = buildfib_seq (n-1) in
    let y = buildfib_seq (n-1) in
    Node (n,x,y)

let rec buildfib_par pool cutoff n =
  if n <= cutoff
  then buildfib_seq n
  else if n <= 0
  then Leaf (Fib.fib 20)
  else
    let x_f = T.async pool (fun _ -> buildfib_par pool cutoff (n-1)) in
    let y = buildfib_par pool cutoff (n-1) in
    Node (n, T.await pool x_f, y)
