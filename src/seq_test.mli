val print_seq : int Seq.t -> unit
val length : 'a Seq.t -> int
val seq_of_list : 'a list -> 'a Seq.t
val get : 'a Seq.t -> int -> 'a
val array_of_seq : 'a Seq.t -> 'a array
val iteri : (int -> 'a -> unit) -> 'a Seq.t -> unit
val init : int -> (int -> 'a) -> 'a Seq.t
