type 'a kd_tree =
    | Leaf
    | Node of ('a kd_tree) * 'a * ('a kd_tree)

type 'a point = 'a array


let compare_pt (coord : int) (u : 'a point) (v : 'a point) : int =
    (*Compare two points (u, v) on the coordinate coord.*)

    if u.(coord) = v.(coord) then
        0
    else if u.(coord) > v.(coord) then
        1
    else
        -1;;


let rec median_of_medians (pt_lst : 'a point list) (coord : int) (l : int) : 'a point =
    (*
     * Divide the point list in sub lists of size l and return the median of
     * the medians, comparing the points on the `coord`-th coordinate.
     *
     * Typically, l = 5.
     *)

    let len = List.length pt_lst in
    if len = 0 then
        failwith "kd_tree.ml: median_of_medians: error: empty list";

    let nb_sub_lst = if len mod l = 0 then len / l else len / l + 1 in (*Number of sub arrays*)

    if nb_sub_lst <= 1 then
        List.hd pt_lst
    else
        let lsts = Array.make_matrix (nb_sub_lst) l (List.hd pt_lst) in  (*Array of sub arrays*)
        List.iteri (fun i e -> lsts.(i / l).(i mod l) <- e) pt_lst;        (*filling lsts*)
        Array.iter (fun p -> Array.sort (compare_pt coord) p) lsts;      (*Sorting the sub arrays*)
        let meds = List.init nb_sub_lst (fun i -> lsts.(i).(l / 2)) in             (*Array of medians*)
        median_of_medians meds coord l;;


let rec median_coord (pt_lst : 'a point list) (coord : int) : ('a point * ('a point list) * ('a point list)) =
    (*
     * Return the median calculated from the `coord`-th coordinate of the point
     * list `pt_lst`
     *
     * - pt_lst : the points list.
     * - coord  : the index / coordinate on which to compare.
     *)

    (*Calculate the pivot*)
    let p = median_of_medians pt_lst coord 5 in
    let l_inf = ref []
    and l_sup = ref [] in
    List.iter (
        fun e ->
            if compare_pt coord e p = 1 then
                l_sup := e::!l_sup

            else if compare_pt coord e p = -1 then
                l_inf := e::!l_inf
    ) pt_lst;
    
    let i = (List.length pt_lst) / 2
    and len = List.length !l_inf in

    if len = i then
        (p, !l_inf, !l_sup)

    else if len > i then
        median_coord !l_inf coord

    else
        median_coord !l_sup coord;;


(*Todo: test the above functions. For the moment, it compiles and it ran on a random test*)

(*let arr = Array.make_matrix 50 20 0;;

for k = 0 to 49 do
    arr.(k) <- Array.init 20 (fun j -> Random.int 64)
done;;

let lst = Array.to_list arr in
Array.iter (fun i -> Printf.printf "%d, " i) (let m, _, _ = median_coord lst 3 in m);*)

let rec create_kd_tree (k : int) (i : int) (l : 'a point list) : 'a point kd_tree =
    (*
     * Return a kd_tree containing the data listed in `l`.
     *
     * - k : The dimension of the data (i.e the length of the elements in l) ;
     * - i : The depth (the coordinate) ;
     * - l : The list of points.
     *)

    match l with
    | [] -> Leaf
    | l ->
        let med, l_inf, l_sup = median_coord l (i mod k) in
        Node(create_kd_tree k (i + 1) l_inf, med, create_kd_tree k (i + 1) l_sup);;

