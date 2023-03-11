val print_seq : int Seq.t -> unit
val length : 'a Seq.t -> int
val seq_of_list : 'a list -> 'a Seq.t

val count_item : 'a -> 'a Seq.t -> int
val most_frequent : int Seq.t -> int
