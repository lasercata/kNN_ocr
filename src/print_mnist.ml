let print_image (image : int array) : unit =
    (*
     * Print the image in ascii art using the file "grayscale_70_levels.txt".
     *
     * - img : linearised array representing a 28 x 28 pixels image in greyscale.
     *)

    let f = open_in "grayscale_70_levels.txt" in
    let scale = input_line f in
    for k = 0 to (Array.length image) - 1 do
        Printf.printf "%c" (scale.[(String.length scale - 1) * (256 - image.(k)) / 256]);
        if k mod 28 = 27 then Printf.printf "\n"
    done;
    close_in f;;


let print_image_small (image : int array) : unit =
    (*Print the image in ascii art using the file "grayscale_70_levels.txt",
     * but only in 20x20 pixels.
     *)

    let f = open_in "grayscale_70_levels.txt" in
    let scale = input_line f in
    for i = 4 to 24 do
        for j = 4 to 24 do
            Printf.printf "%c" (scale.[(String.length scale - 1) * (256 - image.(i*28 + j)) / 256]);
        done;
        print_newline ()
    done;
    close_in f;;


(*
let display_image (img : int array) : unit =
    (*
     * Display the image img in a graphics window.
     *
     * - img : the 28 x 28 pixels linearised image.
     *)

    (*Convert to a Graphics.color array array*)
    let img_g = Array.make_matrix 28 28 (Graphics.rgb 0 0 0) in
    for i = 0 to 27 do
        for j = 0 to 27 do
            let c = img.(28*i + j) in
            img_g.(i).(j) = Graphics.rgb c c c
        done
    done;

    (*Display it*)
    Graphics.open_graph "";
    Graphics.resize_window (Array.length img_g.(0)) (Array.length img_g);
    Graphics.draw_image (Graphics.make_image img_g) 0 0;
    Graphics.loop_at_exit [Graphics.Key_pressed] (fun _ -> Graphics.close_graph (); raise Exit);;
*)


let print_image_and_label (train : bool) (index : int) : unit =
    (*
     * Print the `index`-th image either from the training set (if train is
     * true) or the test set.
     *
     * - train : A bool indicating if print from the training set (if true),
     *           or from the test set ;
     * - index : The index of the image to print ;
     *)

    let fn_img = if train then
        "train-images-idx3-ubyte"
    else
        "t10-images-idx3-ubyte"

    and fn_lb = if train then
        "train-labels-idx1-ubyte"
    else
        "t10-labels-idx1-ubyte"
    in

    let images = Mnist.open_in fn_img
    and labels = Mnist.open_in fn_lb in

    let img = Mnist.get images index
    and lb = (Mnist.get labels index).(0) in

    print_image_small img;
    Printf.printf "\nLabel: %d\n" lb;;


let main (_ : unit) : unit =
    if Array.length (Sys.argv) <> 2 then failwith "Exactly one argument is needed (the index of the image).";
    print_image_and_label true (int_of_string Sys.argv.(1))

(*let _ : unit =
     main ()*)
