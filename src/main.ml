let main (_ : unit) : unit =
    let train_images = Mnist.open_in "train-images-idx3-ubyte" in
    let train_labels = Mnist.open_in "train-labels-idx1-ubyte" in
    let test_images = Mnist.open_in "t10k-images-idx3-ubyte" in
    let test_labels = Mnist.open_in "t10k-labels-idx1-ubyte" in

    let seq = Knn.mnist_seq 100 train_images train_labels
    and img = Mnist.get test_images 2
    and good_lb = (Mnist.get test_labels 2).(0) in
    let lb = Knn.classify seq 3 img in
    Printf.printf "Guessed label : %d\nCorrect label : %d\n" lb good_lb;;

let _ : unit =
    main ()
