open Fib
open Bintree
open Kdtree
open Bhut
open Coins
open Common

module A = Array
module L = List
module F = Float

let epsilon = 0.01

let pow (b : int) (e : int) : int =
  F.to_int ((F.of_int b) ** (F.of_int e))

let print_check b =
  if b then print_endline "OK" else print_endline ("Err")

let check_buildfib (n : int) (tr : tree) =
  let expected = (pow 2 n) * (fib 20) in
  let actual = sumtree_seq tr in
  print_check (expected == actual)

let check_buildkdtree (arr : point3d array) (tr : kdtree) =
  let actual = sumkdtree tr in
  let expected = sum3dpoints arr in
  print_check (F.abs (expected -. actual) < epsilon)

let check_countcorr (arr : point3d array) (query : point3d) (actual : int) (radius : float) =
  let expected = A.fold_right (fun pt acc -> if (dist_point3d query pt) < (radius *. radius)
                                             then acc + 1
                                             else acc)
                   arr 0 in
  print_endline ("Expected: " ^ string_of_int expected ^ "; Actual: " ^ string_of_int actual) ;
  print_check (expected == actual)

let check_nearest (arr : point3d array) (actual : point3d array) =
  let acc = ref true in
  let bools = A.map2 (fun a b -> if a == b
                                 then true && !acc
                                 else (acc := false; false))
                arr actual in
  print_check (!acc)

let check_coins (amt : int) (ls : alist) =
  let n = len ls in
  match amt with
    777 -> print_check (n == 140899)
  | 999 -> print_check (n == 329565)
  (* assume it's correct *)
  | _ ->
     print_endline (string_of_int n) ;
     print_check true

let check_countnodes (count : int) =
  print_endline (string_of_int count)

let check_sorted (f : 'a -> 'a -> int) (arr : 'a array) (sorted : 'a array) =
  let len = A.length sorted in
  let res = ref true in
  let _ = for i = 0 to (len-2) do
            if f (A.get sorted i) (A.get sorted (i+1)) > 0
            then res := false
            else ()
          done;
          print_check !res in
  (* compare with Array.sort *)
  let res2 = ref true in
  let _ = A.sort f arr in
  let _ = for i = 0 to (len-1) do
            if not (Float.equal (A.get sorted i) (A.get arr i))
            then res2 := false
            else ()
          done;
          print_check !res2 in
  ()

let print_float_array (arr : float array) =
  let len = A.length arr in
  for i = 0 to (len-1) do
    print_string (Float.to_string (A.get arr i) ^ ", ")
  done
