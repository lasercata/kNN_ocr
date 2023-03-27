(*File defining useful functions to work with sequences and with some tests.*)


(* Definition of the type Seq.t :
type 'a node =
    | Nil
    | Cons of 'a * 'a t
and 'a t = unit -> 'a node;;*)


let rec print_seq (s : int Seq.t) : unit =
    (*Print a 'a Seq.t sequence.*)

    match s() with
    | Nil -> Printf.printf "\b\b  \b\b\n"
    | Cons(a, b) -> Printf.printf "%d, " a; print_seq b;;


let rec length (s : 'a Seq.t) : int =
    (*Return the length of a sequence*)

    match s() with
    | Seq.Nil -> 0
    | Seq.Cons(_, s') -> 1 + length s';;


let rec seq_of_list (l : 'a list) : 'a Seq.t =
    (*Convert a list to a sequence*)

    match l with
    | [] -> (fun _ -> Seq.Nil)
    | t::q -> (fun _ -> Seq.Cons(t, seq_of_list q));;


let rec get (s : 'a Seq.t) (i : int) : 'a =
    (*Return the element number i in the sequence, as for an array.
     * Raise Invalid_argument if i is outside of the range 0 Seq_test.length s - 1*)

    match s() with
    | Seq.Nil -> raise (Invalid_argument "index too large")
    | Seq.Cons(h, t) ->
        if i = 0 then h
        else get t (i - 1);;


let array_of_seq (s : 'a Seq.t) : 'a array =
    (*Convert a sequence to an array.*)

    Array.init (length s) (get s);;


let iteri (f : int -> 'a -> unit) (s : 'a Seq.t) : unit =
    (*Same as List.iteri, but for sequences.*)

    let rec aux (s' : 'a Seq.t) (i : int) : unit =
        match s'() with
        | Seq.Nil -> ()
        | Seq.Cons(h, t) ->
            f i h;
            aux t (i + 1)
    in
    aux s 0;;


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
let python_range (a : int) (b : int) : int Seq.t =
    (*Return a -> a + 1 -> ... -> b - 1 -> Nil*)
    let rec rng_node (i : int) : int Seq.node =
        match i with
        | i when i >= b -> Nil
        | _ -> Seq.Cons (i, fun _ -> (rng_node (i + 1)))
    in fun _ -> rng_node a;;

(*print_seq (python_range 3 8);;*)

