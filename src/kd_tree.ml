type 'a t =
    | Leaf
    | Node of ('a t) * 'a * ('a t)


let compare_pt (coord : int) (u : (Knn.data * int)) (v : (Knn.data * int)) : int =
    (*Compare two points (u, v) on the coordinate coord.*)

    let img1, _ = u
    and img2, _ = v in

    if img1.(coord) = img2.(coord) then
        0
    else if img1.(coord) > img2.(coord) then
        1
    else
        -1;;


let rec median_of_medians (pt_lst : (Knn.data * int) Seq.t) (coord : int) (l : int) : Knn.data * int =
    (*
     * Divide the point list in sub lists of size l and return the median of
     * the medians, comparing the points on the `coord`-th coordinate.
     *
     * Typically, l = 5.
     *)

    let len = Seq_test.length pt_lst in
    let nb_sub_lst = if len mod l = 0 then len / l else len / l + 1 in (*Number of sub arrays*)

    let first_elem = match pt_lst() with
    | Seq.Nil -> failwith "kd_tree.ml: median_of_medians: error: empty sequence"
    | Seq.Cons(a, _) -> a
    in

    if nb_sub_lst <= 1 then
        first_elem
    else
        let lsts = Array.make_matrix (nb_sub_lst) l first_elem in  (*Array of sub arrays (the init value is just to have the right type)*)
        Seq_test.iteri (fun i e -> lsts.(i / l).(i mod l) <- e) pt_lst;        (*filling lsts*)
        Array.iter (fun p -> Array.sort (compare_pt coord) p) lsts;      (*Sorting the sub arrays*)
        let meds = Seq.init nb_sub_lst (fun i -> lsts.(i).(l / 2)) in             (*Sequence of medians*)
        median_of_medians meds coord l;;


let rec median_coord
(pt_lst : (Knn.data * int) Seq.t)
(coord : int) :
((Knn.data * int) * ((Knn.data * int) Seq.t) * ((Knn.data * int) Seq.t)) =
    (*
     * Return the median calculated from the `coord`-th coordinate of the point
     * list `pt_lst`, as well as two sequences, containing the elements lesser,
     * respectively greater than the median.
     *
     * - pt_lst : the sequence of couple of images and associated label ;
     * - coord  : the index / coordinate on which to compare on the images.
     *)

    (*Calculate the pivot*)
    let p = median_of_medians pt_lst coord 5 in

    (*Calculate l_inf, l_sup*)
    let l_inf = ref (fun _ -> Seq.Nil)
    and l_sup = ref (fun _ -> Seq.Nil) in
    Seq.iter (
        fun e ->
            if compare_pt coord e p = 1 then
                l_sup := Seq.cons e !l_sup

            else if compare_pt coord e p = -1 then
                l_inf := Seq.cons e !l_inf
    ) pt_lst;
    
    (*Return the median or do a recursive call*)
    let i = (Seq_test.length pt_lst) / 2
    and len = Seq_test.length !l_inf in

    if len = i then
        (p, !l_inf, !l_sup)

    else if len > i then
        median_coord !l_inf coord

    else
        median_coord !l_sup coord;;


let rec create_kd_tree (k : int) (i : int) (s : (Knn.data * int) Seq.t) : (Knn.data * int) t =
    (*
     * Return a kd_tree containing the data listed in `s`.
     *
     * - k : The dimension of the data (i.e the length of the elements in s) ;
     * - i : The depth (the coordinate) ;
     * - s : The sequence of couple of images and associated labels.
     *)

    match s() with
    | Seq.Nil -> Leaf
    | _ ->
        let med, l_inf, l_sup = median_coord s (i mod k) in
        Node(create_kd_tree k (i + 1) l_inf, med, create_kd_tree k (i + 1) l_sup);;

let rec visit
(q : (int * float) PrioQueue.t)
(x : Knn.data)
(t : (Knn.data * int) t)
(i : int)
(n : int)
(dist : Knn.data -> Knn.data -> float) :
(int * float) PrioQueue.t =
    (*
     * Visits the n closers neighbors of x in the kd tree t.
     *
     * - q    : The priority queue containing the couple of label and priority ;
     * - x    : The image ;
     * - t    : The kd tree of couple of images and theirs associated label ;
     * - i    : The depth in the kd tree ;
     * - n    : The number of neighbors ;
     * - dist : The distance function.
     *)

    let d = Array.length x in

    match t with
    | Leaf -> q (*Todo: is that correct ?*) 
    | Node(l, x', r) ->
        let img, lb = x' in
        let t1, t2 = if x.(i) <= img.(i) then l, r else r, l in
        let q = visit q x t1 ((i + 1) mod d) n dist in

        let _, max_prio = PrioQueue.top q in
        let q =
            if PrioQueue.size q < n || max_prio >= float_of_int (abs (x.(i) - img.(i))) then begin
                let q' = if dist x img < max_prio then
                    PrioQueue.change_root q (lb, dist x img)
                else
                    q in
                visit q' x t2 ((i + 1) mod d) n dist
            end
            else
                q
        in q;;

