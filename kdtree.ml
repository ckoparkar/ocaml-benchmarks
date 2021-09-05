open Common
module A = Array
module T = Domainslib.Task

type kdtree = KdEmpty
            | KdLeaf of point3d
            | KdNode of { pivot : point3d
                        ; total_points : int
                        ; split_axis : int
                        ; min_pt : point3d
                        ; max_pt : point3d
                        ; left : kdtree
                        ; right : kdtree
                        }

let rec print_kdtree tr =
  match tr with
    KdEmpty -> print_string "empty{}"
  | KdLeaf p -> print_string "leaf{" ; print_point3d p ; print_string "}"
  | KdNode {pivot;split_axis;left;right;_} ->
     (print_string "node{ point: " ;
      print_point3d pivot ;
      print_string ", axis: " ;
      print_int split_axis ;
      print_string ", left: " ;
      print_kdtree left ;
      print_string ", right: " ;
      print_kdtree right ;
      print_string "}"
     )

let get_min_pt (tr : kdtree) : point3d =
  match tr with
    KdEmpty -> (0.0, 0.0, 0.0)
  | KdLeaf pt -> pt
  | KdNode{min_pt;_} -> min_pt

let get_max_pt (tr : kdtree) : point3d =
  match tr with
    KdEmpty -> (0.0, 0.0, 0.0)
  | KdLeaf pt -> pt
  | KdNode{max_pt;_} -> max_pt

let get_total_points (tr : kdtree) : int =
  match tr with
    KdEmpty -> 0
  | KdLeaf pt -> 1
  | KdNode{total_points;_} -> total_points

let rec fromListWithAxis_seq (axis : int) (pts : point3d array) : kdtree =
  let len = A.length pts in
  if len == 0
  then KdEmpty
  else if len == 1
  then KdLeaf (A.get pts 0)
  else
    (* let sorted_pts = Qsort.sort (compare_point3d axis) pts in *)
    let sorted_pts = Mergesort.mergesort_seq (compare_point3d axis) pts in
    (* let sorted_pts = A.copy pts in
     * let _ = A.sort (compare_point3d axis) sorted_pts in *)
    let next_axis = (axis + 1) mod 3 in
    let pivot_idx = len / 2 in
    let pivot = A.get sorted_pts pivot_idx in
    let left_pts = A.sub sorted_pts 0 pivot_idx in
    let right_pts = A.sub sorted_pts (pivot_idx+1) (len - pivot_idx - 1) in
    let left_tr = fromListWithAxis_seq next_axis left_pts in
    let right_tr = fromListWithAxis_seq next_axis right_pts in
    let small = min_point3d pivot (min_point3d (get_min_pt left_tr) (get_min_pt right_tr)) in
    let big = max_point3d pivot (max_point3d (get_max_pt left_tr) (get_max_pt right_tr)) in
    let total_points = (get_total_points left_tr) + (get_total_points right_tr) + 1 in
    KdNode { pivot = pivot
           ; total_points = total_points
           ; split_axis = axis
           ; min_pt = small
           ; max_pt = big
           ; left = left_tr
           ; right = right_tr }

let fromList_seq (pts : point3d array) : kdtree = fromListWithAxis_seq 0 pts

let rec fromListWithAxis_par pool (cutoff : int) (axis : int) (pts : point3d array) : kdtree =
  let len = A.length pts in
  if len < cutoff
  then fromListWithAxis_seq axis pts
  else if len == 0
  then KdEmpty
  else if len == 1
  then KdLeaf (A.get pts 0)
  else
    let sorted_pts = Mergesort.mergesort pool (compare_point3d axis)  pts in
    let next_axis = (axis + 1) mod 3 in
    let pivot_idx = len / 2 in
    let pivot = A.get sorted_pts pivot_idx in
    let left_pts = A.sub sorted_pts 0 pivot_idx in
    let right_pts = A.sub sorted_pts (pivot_idx+1) (len - pivot_idx - 1) in
    let left_tr_f = T.async pool (fun _ -> fromListWithAxis_par pool cutoff next_axis left_pts) in
    let right_tr = fromListWithAxis_par pool cutoff next_axis right_pts in
    let left_tr = T.await pool left_tr_f in
    let small = min_point3d pivot (min_point3d (get_min_pt left_tr) (get_min_pt right_tr)) in
    let big = max_point3d pivot (max_point3d (get_max_pt left_tr) (get_max_pt right_tr)) in
    let total_points = (get_total_points left_tr) + (get_total_points right_tr) + 1 in
    KdNode { pivot = pivot
           ; total_points = total_points
           ; split_axis = axis
           ; min_pt = small
           ; max_pt = big
           ; left = left_tr
           ; right = right_tr }

let fromList_par pool (cutoff : int) (pts : point3d array) : kdtree =
  fromListWithAxis_par pool cutoff 0 pts

let rec sumkdtree (tr : kdtree) : float =
  match tr with
    KdEmpty -> 0.0
  | KdLeaf (x,y,z) -> x +. y +. z
  | KdNode {pivot;left;right;_} ->
     let (x,y,z) = pivot in
     let (n1,n2) = (sumkdtree left, sumkdtree right) in
     x +. y +. z +. n1 +. n2

let sum3dpoints (ls : point3d array) : float =
  A.fold_right (fun (x,y,z) acc -> acc +. x +. y +. z) ls 0.0

let rec countCorr_seq (query : point3d) (radius : float) (tr : kdtree) : int =
  match tr with
    KdEmpty -> 0
  | KdLeaf pt ->
     if (dist_point3d query pt) < (radius *. radius)
     then 1
     else 0
  | KdNode {pivot;total_points;min_pt;max_pt;left;right;_} ->
     let (min_x, min_y, min_z) = min_pt in
     let (max_x, max_y, max_z) = max_pt in
     let (p_x, p_y, p_z) = query in
     let center_x = (min_x +. max_x) /. 2.0 in
     let center_y = (min_y +. max_y) /. 2.0 in
     let center_z = (min_z +. max_z) /. 2.0 in
     let d_x = p_x -. center_x in
     let d_y = p_y -. center_y in
     let d_z = p_z -. center_z in
     let boxdist_x = (max_x -. min_x) /. 2.0 in
     let boxdist_y = (max_y -. min_y) /. 2.0 in
     let boxdist_z = (max_z -. min_z) /. 2.0 in
     let sum = (d_x *. d_x) +. (d_y *. d_y) +. (d_z *. d_z) in
     let boxsum = (boxdist_x *. boxdist_x) +. (boxdist_y *. boxdist_y) +. (boxdist_z *. boxdist_z) in
     (* let (x,y,z) = pivot in *)
     if (sum -. boxsum) < (radius *. radius)
     then
       let n1 = countCorr_seq query radius left in
       let n2 = countCorr_seq query radius right in
       if (dist_point3d pivot query) < (radius *. radius)
       then n1 + n2 + 1
       else n1 + n2
     else if (dist_point3d pivot query) < (radius *. radius)
     then 1
     else 0

let rec countCorr_par pool (cutoff : int) (query : point3d) (radius : float) (tr : kdtree) : int =
  match tr with
    KdEmpty -> 0
  | KdLeaf pt ->
     if (dist_point3d query pt) < (radius *. radius)
     then 1
     else 0
  | KdNode {pivot;total_points;min_pt;max_pt;left;right;_} ->
     if total_points < cutoff
     then countCorr_seq query radius tr
     else
       let (min_x, min_y, min_z) = min_pt in
       let (max_x, max_y, max_z) = max_pt in
       let (p_x, p_y, p_z) = query in
       let center_x = (min_x +. max_x) /. 2.0 in
       let center_y = (min_y +. max_y) /. 2.0 in
       let center_z = (min_z +. max_z) /. 2.0 in
       let d_x = p_x -. center_x in
       let d_y = p_y -. center_y in
       let d_z = p_z -. center_z in
       let boxdist_x = (max_x -. min_x) /. 2.0 in
       let boxdist_y = (max_y -. min_y) /. 2.0 in
       let boxdist_z = (max_z -. min_z) /. 2.0 in
       let sum = (d_x *. d_x) +. (d_y *. d_y) +. (d_z *. d_z) in
       let boxsum = (boxdist_x *. boxdist_x) +. (boxdist_y *. boxdist_y) +. (boxdist_z *. boxdist_z) in
       (* let (x,y,z) = pivot in *)
       if (sum -. boxsum) < (radius *. radius)
       then
         let n1_f = T.async pool (fun _ -> countCorr_par pool cutoff query radius left) in
         let n2 = countCorr_par pool cutoff query radius right in
         let n1 = T.await pool n1_f in
         if (dist_point3d pivot query) < (radius *. radius)
         then n1 + n2 + 1
         else n1 + n2
       else if (dist_point3d pivot query) < (radius *. radius)
       then 1
       else 0

let least_dist (a : point3d) (b : point3d) (c : point3d) : point3d =
  let d1 = dist_point3d a b in
  let d2 = dist_point3d a c in
  if d1 < d2
  then b
  else c

let rec nearest (tr : kdtree) (query : point3d) : point3d =
  match tr with
    KdEmpty -> (0.0, 0.0, 0.0)
  | KdLeaf pt -> pt
  | KdNode{pivot;split_axis;left;right;_} ->
     let tst_query = coord split_axis query in
     let tst_pivot = coord split_axis pivot in
     if tst_query < tst_pivot
     then find_nearest pivot query tst_pivot tst_query left right
     else find_nearest pivot query tst_pivot tst_query right left

and find_nearest (pivot : point3d) (query : point3d) (tst_pivot : float) (tst_query : float) (good_side : kdtree) (other_side : kdtree) =
  let best0 = nearest good_side query in
  let candidate1 = least_dist query best0 pivot in
  (* whether the difference between the splitting coordinate of the search point and current node
     is less than the distance (overall coordinates) from the search point to the current best. *)
  let nearest_other_side = tst_query -. tst_pivot in
  if (nearest_other_side *. nearest_other_side) <= (dist_point3d query candidate1)
  then
    let candidate2 = nearest other_side query in
    let best1 = least_dist query candidate1 candidate2 in
    best1
  else
    candidate1

let allNearestNeighbors_seq (tr : kdtree) (pts : point3d array) : point3d array =
  let n = A.length pts in
  A.init n (fun i -> nearest tr (A.get pts i))

let allNearestNeighbors_par pool (tr : kdtree) (pts : point3d array) : point3d array =
  let n = A.length pts in
  let result = A.copy pts in
  let _ = T.parallel_for ~start:0 ~finish:(n-1) ~chunk_size:1024
            ~body:(fun i -> let nn = nearest tr (A.get pts i) in
                            A.set result i nn;
                            ())
            pool in
  result

let allCountCorr_seq (radius : float) (tr : kdtree) (pts : point3d array) : int array =
  let n = A.length pts in
  A.init n (fun i -> countCorr_seq (A.get pts i) radius tr)

let allCountCorr_par pool (grain : int) (radius : float) (tr : kdtree) (pts : point3d array) : int array =
  let n = A.length pts in
  let result = A.make n 0 in
  let _ = T.parallel_for ~start:0 ~finish:(n-1) ~chunk_size:4
            ~body:(fun i -> let count = countCorr_par pool grain (A.get pts i) radius tr in
                            A.set result i count;
                            ())
            pool in
  result
