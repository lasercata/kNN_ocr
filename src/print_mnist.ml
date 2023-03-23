let print_image (image : int array) : unit =
    let f = open_in "grayscale_70_levels.txt" in
    let scale = input_line f in
    for k = 0 to (Array.length image) - 1 do
        Printf.printf "%c" (scale.[(String.length scale - 1) * (256 - image.(k)) / 256]);
        if k mod 28 = 27 then Printf.printf "\n"
    done;
    close_in f;;

let main (_ : unit) : unit =
    Printf.printf "here (in print_mnist.ml)\n";
    if Array.length (Sys.argv) <> 2 then failwith "Exactly one argument is needed (the index of the image).";
    let train_images = Mnist.open_in "train-images-idx3-ubyte" in
    let train_labels = Mnist.open_in "train-labels-idx1-ubyte" in
    let index = int_of_string Sys.argv.(1) in
    let image = Mnist.get train_images index in
    let number = (Mnist.get train_labels index).(0) in
    print_image image;
    Printf.printf "Label: %d\n" number

let _ : unit =
    main ()
