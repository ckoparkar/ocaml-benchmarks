module T = Domainslib.Task

type exp = Lit of int
         | MkTrue
         | MkFalse
         | Plus of (exp * exp)

let rec fold_constants exp =
  match exp with
    Lit i -> Lit i
  | MkTrue -> MkTrue
  | MkFalse -> MkFalse
  | Plus (Lit i, Lit j) -> Lit (i+j)
  | Plus (a, b) ->
     let c = fold_constants a in
     let d = fold_constants b in
     Plus (c, d)

let rec fold_constants_par pool depth exp =
  if depth >= 8 then fold_constants exp else
    match exp with
      Lit i -> Lit i
    | MkTrue -> MkTrue
    | MkFalse -> MkFalse
    | Plus (Lit i, Lit j) -> Lit (i+j)
    | Plus (a, b) ->
       let cf = T.async pool (fun _ -> fold_constants_par pool (depth+1) a) in
       let d = fold_constants_par pool (depth+1) b in
       let c = T.await pool cf in
       Plus (c, d)

let rec sum_exp e =
  match e with
    Lit i -> i
  | Plus (a, b) -> (sum_exp a) + (sum_exp b)
  | MkTrue -> 0
  | MkFalse -> 0

let rec build_exp n =
  if n <= 0
  then Plus (Lit 0, Lit 1)
  else Plus (build_exp (n-1), build_exp (n-1))
