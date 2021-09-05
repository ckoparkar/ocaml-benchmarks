(* taken from nofib: https://github.com/ghc/nofib/tree/master/parallel/coins *)

module L = List
module T = Domainslib.Task

type coin = int * int

type alist = ANil | ASing of int | Append of (alist * alist)

let rec len (ls : alist) : int =
  match ls with
    ANil  -> 0
  | ASing _ -> 1
  | Append (l, r) -> len l + len r

let append ls1 ls2 =
  match (ls1, ls2) with
    (ANil, _) -> ls2
  | (_, ANil) -> ls1
  | _ -> Append (ls1, ls2)

let rec pay_seq (amt : int) (coins : coin list) : alist =
  if amt == 0
  then ASing 1
  else
    if L.length coins == 0
    then ANil
    else
      let (c,q) = L.hd coins in
      let coins_rst = L.tl coins in
      if c > amt
      then pay_seq amt coins_rst
      else
        let coins1 = if q ==1
                     then coins_rst
                     else L.cons (c,q-1) coins_rst in
        let left = pay_seq (amt - c) coins1 in
        let right = pay_seq amt coins_rst in
        append left right

let rec pay_par pool depth (amt : int) (coins : coin list) : alist =
  if depth == 0
  then pay_seq amt coins
  else
    if amt == 0
    then ASing 1
    else
      if L.length coins == 0
      then ANil
      else
        let (c,q) = L.hd coins in
        let coins_rst = L.tl coins in
        if c > amt
        then pay_par pool depth amt coins_rst
        else
          let (coins1, depth1) = if q ==1
                                 then (coins_rst, depth - 1)
                                 else (L.cons (c,q-1) coins_rst, depth) in
          let left = T.async pool (fun _ -> pay_par pool depth1 (amt - c) coins1) in
          let right = pay_par pool (depth - 1) amt coins_rst in
          append (T.await pool left) right

let coins_input : coin list =
  let cs = [250 ; 100 ; 25 ; 10 ; 5 ; 1] in
  let qs = [55 ; 88 ; 88 ; 99 ; 122 ; 177] in
  L.combine cs qs
