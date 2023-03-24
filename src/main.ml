let test_classify (n : int) (m : int) (k : int) : float =
    (*
     * Run Knn.classify with n training images, m tests, and return the success rate.
     *
     * - n       : the number of training images ;
     * - m       : the number of testing images ;
     * - k       : the parameter of kNN.
     *)

    let train_images = Mnist.open_in "train-images-idx3-ubyte" in
    let train_labels = Mnist.open_in "train-labels-idx1-ubyte" in
    let test_images = Mnist.open_in "t10k-images-idx3-ubyte" in
    let test_labels = Mnist.open_in "t10k-labels-idx1-ubyte" in

    let train_seq = Knn.mnist_seq n train_images train_labels in
    let test_seq = Knn.mnist_seq m test_images test_labels in
    let correct_count = ref 0 in

    Seq.iter (
        fun s ->
            let img, lb = s in
            let guessed_lb = Knn.classify train_seq k img in
            if lb = guessed_lb then incr correct_count
    )
    test_seq;
    
    (float_of_int !correct_count) *. 100. /. (float_of_int m);;


let main (_ : unit) : unit =
    let rate = test_classify 300 100 1 in
    Printf.printf "Success rate : %f%s\n" rate "%";;

let _ : unit =
    main ()
