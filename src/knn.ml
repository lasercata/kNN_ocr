type data = float array
type 'label t = (data * 'label) Seq.t

(*Q5*)
(*Function that count the number of occurrences of an element in a sequence.*)
let rec count_item (i : 'a) (s : 'a Seq.t) : int =
    match s() with
    | Seq.Nil -> 0
    | Seq.Cons(a, s') -> (if a = i then 1 else 0) + count_item i s';;

(*Q6*)
(*Function that calculate an element of s such that the frequency of this element is maximal*)
let most_frequent (s : 'a Seq.t) : 'a =
    let len = Seq_test.length s in

    if len = 0 then
        failwith "The sequence should contain at least one element !";

    (*Adding elements of the sequence in a table, in the form (i, count_item i)*)
    let table = Hashtbl.create (len) in
    Seq.iter (fun x -> Hashtbl.add table x (count_item x s)) s;

    (*Calculate the maximum*)
    let imax = ref 0 in
    let mx = ref 0 in
    Hashtbl.iter (
        fun i n ->
            if n > !mx then begin
                mx := n;
                imax := i
            end
    ) table;

    !imax;;

(*Q7*)
(*Calculates the euclidean distance between the two vectors u and v.*)
let euclidean_dist (u : float array) (v : float array) : float =
    let len = Array.length u in
    if len <> Array.length v then
        failwith "The vectors does not have the same dimension !";

    let sum = ref 0. in
    for k = 0 to len do
        sum := !sum +. (u.(k) -. v.(k)) ** 2.
    done;
    sqrt !sum;;

(*Todo: Functions for Q5 to Q7 compile, but I did not tested them yet ...*)


let init (seq : (data * 'label) Seq.t) : 'label t =
  seq

let classify (seq : 'label t) (k : int) (x : data) : 'label =
  failwith "not implemented"
