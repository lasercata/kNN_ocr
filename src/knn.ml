type data = float array
type 'label t = (data * 'label) Seq.t

(*Q7*)
(*Calculate the euclidean distance between the two vectors u and v.*)
let euclidean_dist (u : float array) (v : float array) : float =
    let len = Array.length u in

    if len <> Array.length v then
        failwith "The vectors does not have the same dimension !";

    let sum = ref 0. in
    for k = 0 to len do
        sum := !sum +. (u.(k) -. v.(k)) ** 2.
    done;
    sqrt !sum;;

(*Q8*)
let rec mnist_seq (n : int) (img : Mnist.idx) (labels : Mnist.idx) : (int array * int) Seq.t =
    match n with
    | 0 -> (fun _ -> Seq.Nil)
    | n -> (
        fun _ ->
            Seq.Cons(
                (Mnist.get img n, (Mnist.get labels n).(0)),
                mnist_seq (n - 1) img labels
            )
    )

(*Todo : test the above function.*)


let init (seq : (data * 'label) Seq.t) : 'label t =
  seq

let classify (seq : 'label t) (k : int) (x : data) : 'label =
  failwith "not implemented"
