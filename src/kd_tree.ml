type 'a t =
    | Leaf
    | Node of ('a t) * 'a * ('a t)


let compare_pt (coord : int) (u : (int array * int)) (v : (int array * int)) : int =
    (*Compare two points (u, v) on the coordinate coord.*)

    let img1, _ = u
    and img2, _ = v in

    if img1.(coord) = img2.(coord) then
        0
    else if img1.(coord) > img2.(coord) then
        1
    else
        -1;;


let rec median_of_medians (pt_lst : (int array * int) Seq.t) (coord : int) (l : int) : int array * int =
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
        let meds = Seq_test.init nb_sub_lst (fun i -> lsts.(i).(l / 2)) in             (*Sequence of medians*)
        median_of_medians meds coord l;;


let id = ref 0;;

let rec median_coord
(pt_lst : (int array * int) Seq.t)
(coord : int)
(i : int) :
((int array * int) * ((int array * int) Seq.t) * ((int array * int) Seq.t)) =
    (*
     * Return the element of index i in the sorted version of pt_lst, as well
     * as two sequences, containing the elements lesser, respectively greater
     * than the element.
     *
     * To sort the sequence, the comparaison function compare the pixel `coord`
     * of the images.
     * This function does not actually sort the sequence, but rather use the
     * median of medians algorithm in order to find a pivot.
     *
     * - pt_lst : the sequence of couple of images and associated label ;
     * - coord  : the index / coordinate on which to compare on the images ;
     * - i      : the index of the returned element in the sorted sequence. For
     *            the median, it is (Seq_test.length s / 2).
     *)

    if Seq_test.length pt_lst = 0 then
        failwith "kd_tree.ml: median_coord: error: empty sequence";

    (*Calculate the pivot*)
    let p = median_of_medians pt_lst coord 5 in

    (*Calculate l_inf, l_sup*)
    let l_inf = ref (fun _ -> Seq.Nil)
    and l_sup = ref (fun _ -> Seq.Nil) in
    Seq.iter (
        fun e ->
            if compare_pt coord e p = -1 then
                l_inf := Seq.cons e !l_inf

            else (*if compare_pt coord e p = 1 then*)
                l_sup := Seq.cons e !l_sup
    ) pt_lst;
    
    (*Return the median or do a recursive call*)
    let len_inf = Seq_test.length !l_inf
    and len_sup = Seq_test.length !l_sup in
    Printf.printf "id : %d, len l_inf : %d, len l_sup : %d, i : %d\n" !id len_inf len_sup i;

    if len_inf = i then
(        incr id;
        (p, !l_inf, !l_sup))

    else if len_inf > i then
        median_coord !l_inf coord i

    else
        median_coord !l_sup coord (i - len_inf - 1);;


let naive_median (s : (int array * int) Seq.t) (coord : int) (i : int) :
((int array * int) * ((int array * int) Seq.t) * ((int array * int) Seq.t)) =
    (*Same as median_coord, but implemented naively : sort the sequence.*)

    if Seq_test.length s = 1 then
        Seq_test.get s 0, (fun _ -> Seq.Nil), (fun _ -> Seq.Nil)

    else
        let arr = Seq_test.array_of_seq s in
        Array.sort (compare_pt coord) arr;
        let m = arr.(i)
        and l_inf = Seq.init (i - 1) (fun k -> arr.(k))
        and l_sup = Seq.init (Array.length arr - i - 2) (fun k -> arr.(k + i + 1)) in
        m, l_inf, l_sup;;

let median_func = naive_median;;


let rec create (k : int) (i : int) (s : (int array * int) Seq.t) : (int array * int) t =
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
        let med, l_inf, l_sup = median_func s (i mod k) (Seq_test.length s / 2) in
        Node(create k (i + 1) l_inf, med, create k (i + 1) l_sup);;


let rec visit
(q : (int * float) PrioQueue.t)
(x : int array)
(tree : (int array * int) t)
(i : int)
(n : int)
(dist : int array -> int array -> float) :
(int * float) PrioQueue.t =
    (*
     * Visits the n closers neighbors of x in the kd tree tree.
     *
     * - q    : The priority queue containing the couple of label and priority ;
     * - x    : The image ;
     * - tree : The kd tree of couple of images and theirs associated label ;
     * - i    : The depth in the kd tree ;
     * - n    : The number of neighbors ;
     * - dist : The distance function.
     *)

    let d = Array.length x in

    match tree with
    | Leaf -> q (*Todo: is that correct ?*) 
    | Node(l, x', r) ->
        let img, lb = x' in
        let dst = dist x img in
        let t1, t2 = if x.(i) <= img.(i) then l, r else r, l in
        let q = visit q x t1 ((i + 1) mod d) n dist in

        if PrioQueue.size q < n then
            let q' = PrioQueue.insert q (lb, dst) in
            visit q' x t2 ((i + 1) mod d) n dist

        else
            let _, max_prio = PrioQueue.top q in
            let q' =
                if max_prio >= float_of_int (abs (x.(i) - img.(i))) then
                    if dst < max_prio then
                        PrioQueue.change_root q (lb, dst)
                    else
                        q

                else
                    q
            in
            visit q' x t2 ((i + 1) mod d) n dist;;

