(*type data = float array*)
type data = int array
type 'label t = (data * 'label) Seq.t


(*Q5*)
let rec count_item (i : 'a) (l : 'a list) : int =
    (*Count the number of occurrences of the element i in the list l.*)

    match l with
    | [] -> 0
    | h::t -> (if h = i then 1 else 0) + count_item i t;;

(*Q6*)
let most_frequent (l : 'a list) : 'a =
    (*
     * Calculate an element of l (non empty) such that the frequency of this
     * element is maximal
     *)

    let len = List.length l in

    if len = 0 then
        failwith "The list should contain at least one element !";

    (*Adding elements of the list in a table, in the form (i, count_item i)*)
    let table = Hashtbl.create (len / 2) in
    List.iter (fun x -> Hashtbl.add table x (count_item x l)) l;

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
let euclidean_dist (u : float array) (v : float array) : float =
    (*Calculate the euclidean distance between the two vectors u and v.*)

    let len = Array.length u in

    if len <> Array.length v then
        failwith "The vectors does not have the same dimension !";

    let sum = ref 0. in
    for k = 0 to len - 1 do
        sum := !sum +. (u.(k) -. v.(k)) ** 2.
    done;
    sqrt !sum;;

let euclidean_dist_2 (u : int array) (v : int array) : int =
    (*
     * Calculate the square of the euclidean distance between u and v,
     * to avoid to deal with floats.
     *)

    let len = Array.length u in

    if len <> Array.length v then
        failwith "The vectors does not have the same dimension !";

    let sum = ref 0 in
    for k = 0 to len - 1 do
        sum := !sum + (u.(k) - v.(k)) * (u.(k) - v.(k))
    done;
    !sum;;

let euclidean_dist_int (u : int array) (v : int array) : float =
    (*Convert u and v to float arrays and use the above function.*)

    let len = Array.length u in

    if len <> Array.length v then
        failwith "The vectors does not have the same dimension !";

    let uf = Array.make len 0.
    and vf = Array.make len 0. in

    for k = 0 to len - 1 do
        uf.(k) <- (float_of_int u.(k));
        vf.(k) <- (float_of_int v.(k))
    done;

    euclidean_dist uf vf;;

(*Q8*)
let rec mnist_seq (n : int) (img : Mnist.idx) (labels : Mnist.idx) : (int array * int) Seq.t =
    (*
     * Create a sequence of n couples containing an image and the corresponding
     * label.
     *
     * - n      : the length of the sequence to create ;
     * - img    : the idx list of images ;
     * - labels : the idx list of labels.
     *)

    match n with
    | 0 -> (fun _ -> Seq.Nil)
    | n -> (
        fun _ ->
            Seq.Cons(
                (Mnist.get img n, (Mnist.get labels n).(0)),
                mnist_seq (n - 1) img labels
            )
    );;

let print_unsorted_queue (f : 'a PrioQueue.t) (format_str : ('a -> 'b, out_channel, unit) format) : unit =
    (*
     * Print the elements of a 'a PrioQueue.t
     *
     * - f          : the priority queue ;
     * - format_str : the format string for Printf. You can use "%d, " for int
     *                for example.
     *)

    let l = PrioQueue.to_unsorted_list f in
    List.iter (fun a -> Printf.printf format_str a) l;
    Printf.printf "\b\b  \b\b\n"


let init (seq : (data * 'label) Seq.t) : 'label t =
  seq


(*let classify (seq : 'label t) (k : int) (x : data) : 'label =*)
let classify (seq : (int array * int) Seq.t) (k : int) (x : data) : 'label =
    (*
     * Return the guess of the label of the image x, using kNN algorithm.
     *
     * - seq : sequence of couples of image and the corresponding label (training set)
     * - k   : the parameter of kNN
     * - x   : the image to classify.
     *)

    (*Creation of the priority queue*)
    let f = ref (PrioQueue.create (
        fun a b ->
            let x1, _ = Seq_test.get seq a
            and x2, _ = Seq_test.get seq b in
            let d = euclidean_dist_2 x1 x2 in

            if d = 0 then (*<= 1e-16 then*)
                0
            else if d < 0 then
                1
            else
                -1
    )) in

    (*Insertion of the first data*)
    for i = 0 to k - 1 do
        f := PrioQueue.insert !f i
    done;

    (*Insertion of the remaining data*)
    Seq_test.iteri (
        fun i s ->
            let xi, _ = s
            and x_max, _ = Seq_test.get seq (PrioQueue.top !f) in
            if i >= k && euclidean_dist_2 x xi < (euclidean_dist_2 x x_max) then
                f := PrioQueue.change_root !f i
    ) seq;

    (*Creation of the list C = {C_i | i \in f} (list of labels)*)
    let c = List.map (fun i -> let _, l = (Seq_test.get seq i) in l) (PrioQueue.to_unsorted_list !f) in
    most_frequent c;;

