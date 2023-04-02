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
        failwith "knn.ml: most_frequent: error: the list should contain at least one element !";

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

let most_frequent_from_Gaspart (l : 'a list) : 'a =
    let elm = List.hd l in
    fst (
        List.fold_left (
            fun (e0, c0) e1 ->
                if count_item e1 l > c0 then
                    (e1, count_item e1 l)
                else
                    (e0, c0)
        ) (elm, 0) l
    );;


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

let euclidean_dist_2 (u : int array) (v : int array) : float =
    (*
     * Calculate the square of the euclidean distance between u and v,
     * to avoid to deal with sqrt.
     *)

    let len = Array.length u in

    if len <> Array.length v then
        failwith "The vectors does not have the same dimension !";

    let sum = ref 0 in
    for k = 0 to len - 1 do
        let x = u.(k) - v.(k) in
        sum := !sum + x * x
    done;
    float_of_int !sum;;

let dist_bin (u : int array) (v : int array) : int =
    (*
     * Calculate the binarized distance between u and v, i.e change each color
     * to black or white before the calculation.
     *)

    let len = Array.length u in

    if len <> Array.length v then
        failwith "The vectors does not have the same dimension !";

    let sum = ref 0 in
    for k = 0 to len - 1 do
        let x1 = if u.(k) > 256 / 2 then 1 else 0
        and x2 = if v.(k) > 256 / 2 then 1 else 0 in
        sum := !sum + (x1 - x2) * (x1 - x2)
    done;
    !sum;;

let dist_jaccard (u : int array) (v : int array) : float =
    (*Distance for the binarized image.*)

    let m_10 = ref 0
    and m_01 = ref 0
    and m_11 = ref 0 in

    for k = 0 to Array.length u - 1 do
        let x1 = if u.(k) > 256 / 2 then 1 else 0
        and x2 = if v.(k) > 256 / 2 then 1 else 0 in

        if x1 = x2 && x1 = 1 then
            incr m_11
        else if x1 = 1 then
            incr m_10
        else if x2 = 1 then
            incr m_01
    done;

    (float_of_int (!m_10 + !m_01)) /. (float_of_int (!m_10 + !m_01 + !m_11));;


let dist_unpad (u : int array) (v : int array) : float =
    (*Calculate a distance between u and v. Ignore the padding of the images.*)

    if Array.length u = 28 then
        let sum = ref 0 in
        for i = 4 to 27 - 4 do
            for j = 4 to 27 - 4 do
                let x = u.(28*i + j) - v.(28*i + j) in
                sum := !sum + (x * x)
            done
        done;
        float_of_int !sum

    else
        euclidean_dist_2 u v;;

let dist_test (u : int array) (v : int array) : float =
    (*Calculate a distance between u and v. Ignore the padding of the images.*)

    let sum = ref 0 in
    for i = 4 to 28 - 4 do
        for j = 4 to 27 - 4 do
            let x = u.(27*i + j) - v.(28*i + j) in
            sum := !sum + abs(x * x * x * x)
        done
    done;
    float_of_int !sum;;


let distances = [|euclidean_dist_2; dist_unpad; dist_jaccard; dist_test|]


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
    Printf.printf "\b\b  \b\b\n";;


let unpad_pic (img : int array) : int array =
    (*Return the center (a 20 x 20 pixels image) of a 28 x 28 pixels image.*)

    let ret = Array.make (20*20) 0 in
    for i = 4 to 23 do
        for j = 4 to 23 do
            ret.((i - 4) * 20 + j - 4) <- img.(28*i + j)
        done
    done;
    ret;;

let unpad_seq (seq : (data * 'label) Seq.t) : 'label t =
    (*Apply unpad_pic on a sequence.*)

    Seq.map (fun s -> let img, lb = s in (unpad_pic img, lb)) seq;;


let binarize_pic (img : int array) : int array =
    (*Reduce the colors of the image `img` to only two.*)

    Array.init (Array.length img) (fun i -> if img.(i) > 256 / 2 then 1 else 0);;

let binarize_seq (seq : (data * 'label) Seq.t) : 'label t =
    (*Apply binarize_pic on a sequence.*)

    Seq.map (fun s -> let img, lb = s in (binarize_pic img, lb)) seq;;


let init (seq : (data * 'label) Seq.t) : 'label t =
    seq


(*let classify (seq : 'label t) (k : int) (x : data) : 'label =*)
let classify (seq : (int array * int) Seq.t) (k : int) (x : data) (dist : (data -> data -> float)) : 'label =
    (*
     * Return the guess of the label of the image x, using kNN algorithm.
     *
     * - seq  : sequence of couples of image and the corresponding label (training set)
     * - k    : the parameter of kNN
     * - x    : the image to classify ;
     * - dist : the distance function.
     *)

    (*Creation of the priority queue*)
    let f = ref (PrioQueue.create (
        fun a b ->
            let _, x1 = a
            and _, x2 = b in
            Float.compare x2 x1
    )) in

    (*Insertion of the data in the queue*)
    let d_max = ref infinity in

    Seq_test.iteri (
        fun i s ->
            let xi, lb = s in
            let d = dist x xi in

            (*Insertion of the first data*)
            if i < k then begin
                f := PrioQueue.insert !f (lb, d);

                if d < !d_max then
                    d_max := d
            end

            (*Insertion of the remaining data*)
            else if d < !d_max then begin
                f := PrioQueue.change_root !f (lb, d);
                d_max := d
            end
    ) seq;

    (*Creation of the list C = {C_i | i \in f} (list of labels)*)
    let c = List.map (
        fun i -> let l, _ = i in l
    )
    (PrioQueue.to_unsorted_list !f) in
    most_frequent c;;


let classify_kd (train : (int array * int) Kd_tree.t) (k : int) (x : data) (dist : (data -> data -> float)) : 'label =
    (*
     * Return the guess of the label of the image x, using kNN algorithm.
     * The trainning data is given in a kd-tree.
     *
     * - train : kd tree of couples of image and the corresponding label (training set)
     * - k     : the parameter of kNN
     * - x     : the image to classify ;
     * - dist  : the distance function.
     *)

    (*Creation of the priority queue*)
    let f = ref (PrioQueue.create (
        fun a b ->
            let _, x1 = a
            and _, x2 = b in
            Float.compare x2 x1
    )) in

    (*Filling `f` with the k closers neigbors*)
    f := Kd_tree.visit !f x train 0 k dist;

    (*Get the most frequent one*)
    let c = List.map (
        fun i -> let l, _ = i in l
    )
    (PrioQueue.to_unsorted_list !f) in
    most_frequent c;;


let test_classify
(n : int)
(m : int)
(k : int)
(kd : bool)
(dist : int)
(make_conf : bool)
(unpad : bool)
(verbose : bool) :
float * (int array array) =
    (*
     * Run Knn.classify with n training images, m tests, and return a couple
     * containing the success rate and the confusion matrix.
     *
     * - n         : The number of training images ;
     * - m         : The number of testing images ;
     * - k         : The parameter of kNN ;
     * - kd        : If true, use a kd tree to optimize neighbors research ;
     * - dist      : The index of the wanted distance. Possible values :
         * 0 : euclidean distance squared (to avoid using the sqrt) ;
         * 1 : same but ignore the padding ;
         * 2 : binarize colors and use Jaccard's distance.
     * - make_conf : If true, fill the confusion matrix. Otherwise, it is
     *               full of zeros ;
     * - unpad     : If true, pre-process images to remove the padding and
     *               make them 20x20 (instead of 28x28) ;
     * - verbose   : If true, show the time elapsed during the initialisation.
     *)

    let t0 = Sys.time () in
    (*Init*)
    let train_images = Mnist.open_in "train-images-idx3-ubyte"
    and train_labels = Mnist.open_in "train-labels-idx1-ubyte"
    and test_images = Mnist.open_in "t10k-images-idx3-ubyte"
    and test_labels = Mnist.open_in "t10k-labels-idx1-ubyte" in

    let train_seq0 = mnist_seq n train_images train_labels
    and test_seq0 = mnist_seq m test_images test_labels
    and confusion = Array.make_matrix 10 10 0
    and correct_count = ref 0 in

    (*Unpadding*)
    let train_seq = if unpad then unpad_seq train_seq0 else train_seq0
    and test_seq = if unpad then unpad_seq test_seq0 else test_seq0 in

    (*Choosing the function to classify according to the bool `kd`*)
    let classifyer = 
        if kd then
            let img0, _ = Seq_test.get train_seq 0 in
            let train_kd = Kd_tree.create (Array.length img0) 0 train_seq in
            classify_kd train_kd
        else
            classify train_seq
    in

    if verbose then
        Print_mnist.print_time (Sys.time () -. t0) "Initialisation duration";

    let t1 = Sys.time () in

    (*The tests*)
    Seq.iter (
        fun s ->
            let img, lb = s in
            let guessed_lb = classifyer k img distances.(dist) in
            if make_conf then
                confusion.(lb).(guessed_lb) <- confusion.(lb).(guessed_lb) + 1;
            if lb = guessed_lb then incr correct_count
    )
    test_seq;

    if verbose then
        Print_mnist.print_time (Sys.time () -. t1) "Testing duration";
    
    let rate = (float_of_int !correct_count) *. 100. /. (float_of_int m) in
    (rate, confusion);;

