type 'a kd_tree =
    | Leaf
    | Node of ('a kd_tree) * 'a * ('a kd_tree)

type 'a point = 'a array

val median_coord : 'a point list -> int -> 'a point * ('a point list) * ('a point list)
val create_kd_tree : int -> int -> 'a point list -> 'a point kd_tree
