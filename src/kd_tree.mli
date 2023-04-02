type 'a t =
    | Leaf
    | Node of ('a t) * 'a * ('a t)

(*val median_of_medians : (int array * int) Seq.t -> int -> int -> int array * int*)

val median_coord :
    (int array * int) Seq.t ->
    int ->
    int ->
    (int array * int) * ((int array * int) Seq.t) * ((int array * int) Seq.t)

val create : int -> int -> (int array * int) Seq.t -> (int array * int) t

val visit :
(int * float) PrioQueue.t ->
int array ->
(int array * int) t ->
int ->
int ->
(int array -> int array -> float) ->
(int * float) PrioQueue.t
