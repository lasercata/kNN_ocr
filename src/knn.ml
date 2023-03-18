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
let mnist_seq (n : int) (img : Mnist.idx) (labels : Mnist.idx) : (int array * int) Seq.t =
    let s = ref (fun _ -> Seq.Nil) in
    for k = 0 to labels.size.(0) - 1 do
        s := (
            fun _ -> Seq.Cons((Mnist.get img k, (Mnist.get labels k).(0)), !s)
        )
    done;
    !s;;

(*Todo : test the above function.*)


let init (seq : (data * 'label) Seq.t) : 'label t =
  seq

let classify (seq : 'label t) (k : int) (x : data) : 'label =
  failwith "not implemented"
