(*File defining useful functions to work with sequences and with some tests.*)


(* Definition of the type Seq.t :
type 'a node =
    | Nil
    | Cons of 'a * 'a t
and 'a t = unit -> 'a node;;*)


(*Function that print a 'a Seq.t sequence.*)
let rec print_seq (s : int Seq.t) : unit =
    match s() with
    | Nil -> Printf.printf "\b\b  \b\b\n"
    | Cons(a, b) -> Printf.printf "%d, " a; print_seq b;;

(*Function that return the length of a sequence*)
let rec length (s : 'a Seq.t) : int =
    match s() with
    | Seq.Nil -> 0
    | Seq.Cons(_, s') -> 1 + length s';;

(*Function that convert a list to a sequence*)
let rec seq_of_list (l : 'a list) : 'a Seq.t =
    match l with
    | [] -> (fun _ -> Seq.Nil)
    | t::q -> (fun _ -> Seq.Cons(t, seq_of_list q));;


(*Q5*)
(*Function that count the number of occurrences of an element in a sequence.*)
let rec count_item (i : 'a) (s : 'a Seq.t) : int =
    match s() with
    | Seq.Nil -> 0
    | Seq.Cons(a, s') -> (if a = i then 1 else 0) + count_item i s';;

(*Q6*)
(*Function that calculate an element of s (non empty) such that the frequency of this element is maximal*)
let most_frequent (s : int Seq.t) : int =
    let len = length s in

    if len = 0 then
        failwith "The sequence should contain at least one element !";

    (*Adding elements of the sequence in a table, in the form (i, count_item i)*)
    let table = Hashtbl.create (len / 2) in
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

(*Q2 : s = 17 -> 42 -> Nil*)
let s = (fun () -> (Seq.Cons (17, (fun () -> (Seq.Cons (42, (fun () -> Seq.Nil)))))));;

(*s' = 17 -> 42 -> 17 -> ...*)
let rec s' = (fun _ -> Seq.Cons (17, (fun _ -> Seq.Cons (42, s'))));;
(*Seq.iter (fun i -> Printf.printf "%d, " i) s';;*)


(*Q3*)
(*Function that recursively builds the sequence i -> (i + 1) -> (i + 2) -> ...*)
let rec seq_next_node (i : int) : int Seq.node =
    Seq.Cons (i, fun _ -> (seq_next_node (i + 1)));;

let all_int = fun _ -> seq_next_node 0;;


(*Q4*)
(*Function that return a -> a + 1 -> ... -> b - 1 -> Nil*)
let python_range (a : int) (b : int) : int Seq.t =
    let rec rng_node (i : int) : int Seq.node =
        match i with
        | i when i >= b -> Nil
        | _ -> Seq.Cons (i, fun _ -> (rng_node (i + 1)))
    in fun _ -> rng_node a;;

(*print_seq (python_range 3 8);;*)

