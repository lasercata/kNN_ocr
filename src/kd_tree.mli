type 'a t =
    | Leaf
    | Node of ('a t) * 'a * ('a t)

(*val median_of_medians : (Knn.data * int) Seq.t -> int -> int -> Knn.data * int*)

val median_coord :
    (Knn.data * int) Seq.t ->
    int ->
    (Knn.data * int) * ((Knn.data * int) Seq.t) * ((Knn.data * int) Seq.t)

val create_kd_tree : int -> int -> (Knn.data * int) Seq.t -> (Knn.data * int) t
