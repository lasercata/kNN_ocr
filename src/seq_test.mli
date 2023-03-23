val print_seq : int Seq.t -> unit
val length : 'a Seq.t -> int
val seq_of_list : 'a list -> 'a Seq.t
val get : 'a Seq.t -> int -> 'a
val iteri : (int -> 'a -> unit) -> 'a Seq.t -> unit
