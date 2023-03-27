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


let print_usage (argv0 : string) =
    (*
     * Print the basic help for the command line.
     *
     * -argv0 : the program name.
     *)

    Printf.printf "Usage : %s [-h] [-v] [-t] [-p INDEX] [-kd] TRAIN_NB TEST_NB K\n" argv0;;

let print_help (argv0 : string) =
    (*
     * Print the help message for the command line parser.
     *
     * -argv0 : the program name.
     *)

    print_usage argv0;

    Printf.printf "\nRecognize images from the MNIST data base.\n";

    Printf.printf "\nPositional arguments :\n";
    Printf.printf "    TRAIN_NB                 The number of train images\n";
    Printf.printf "    TEST_NB                  The number of test images\n";
    Printf.printf "    K                        The kNN parameter\n";

    Printf.printf "\nOptional arguments :\n";
    Printf.printf "    -h, --help               Print this help message and exit\n";
    Printf.printf "    -v, --verbose            Be more verbose\n";
    Printf.printf "    -t, --test               Run tests and exit (ignore positional arguments)\n";
    Printf.printf "    -kd, --kd-tree           Use a kd tree\n";
    Printf.printf "    -p INDEX, --print INDEX  Print the image at position INDEX and exit (ignore\n";
    Printf.printf "                             positional arguments)\n";;


let main (argv : string array) : unit =
    (*Parse command line arguments and execute the corresponding functions.*)

    let proj_name = "Mnist_0cr" in (*TODO: find a name !*)

    (*Init*)
    let argc = Array.length argv in
    let train_nb = ref (-1)
    and test_nb = ref (-1)
    and k = ref (-1)

    and verbose = ref false
    and tests = ref false
    and kd_tree = ref false
    and print_index = ref (-1)

    and exit = ref false
    and read_last_index = ref 0 in (*Keep index of last assignation for train_nb, test_nb, and k.*)

    (*Parsing*)
    if argc = 1 then begin
        print_usage argv.(0);
        Printf.printf "%s: error: the following arguments are requied: TRAIN_NB TEST_NB K\n" proj_name;
        exit := true
    end
    else begin
        let continue = ref true
        and i = ref 1 in

        while !continue && !i < argc do
            match argv.(!i) with
            | "-h" | "--help" ->  begin
                print_help argv.(0);
                continue := false;
                exit := true
            end
            | "-v" | "--verbose" -> begin
                verbose := true;
                incr i
            end
            | "-t" | "--test" -> begin
                tests := true;
                continue := false
            end
            | "-kd" | "--kd-tree" -> begin
                kd_tree := true;
                incr i
            end
            | "-p" | "--print" -> begin
                if !i + 1 < argc - 1 then begin
                    print_usage argv.(0);
                    Printf.printf "%s: error: the argument '-p' needs INDEX\n" proj_name;
                    exit := true;
                    continue := false;
                end
                else begin
                    match int_of_string_opt argv.(!i + 1) with
                    | None -> begin
                        print_usage argv.(0);
                        Printf.printf "%s: error: unrecognized argument : '%s'\n" proj_name argv.(!i + 1);
                        continue := false;
                        exit := true
                    end
                    | Some n when n < 0 -> begin
                        print_usage argv.(0);
                        Printf.printf "%s: error: invalid argument (should be positive) : '%s'\n" proj_name argv.(!i + 1);
                        continue := false;
                        exit :=  true
                    end
                    | Some n -> begin
                        print_index := n;
                        continue := false (*ignore other arguments*)
                    end
                end
            end
            | s when s.[0] = '-' -> begin (*Wrong argument*)
                Printf.printf "%s: error: unrecognized argument : '%s'\n" proj_name s;
                continue := false;
                exit := true
            end
            | s when !train_nb = -1 && !read_last_index = 0 -> begin (*TRAIN_NB*)
                match int_of_string_opt s with
                | None -> begin
                    print_usage argv.(0);
                    Printf.printf "%s: error: invalid argument : '%s'\n" proj_name s;
                    continue := false;
                    exit := true
                end
                | Some n when n <= 0 -> begin
                    print_usage argv.(0);
                    Printf.printf "%s: error: invalid argument (should be positive) : '%s'\n" proj_name s;
                    continue := false;
                    exit :=  true
                end
                | Some n -> begin
                    train_nb := n;
                    read_last_index := !i;
                    incr i
                end
            end
            | s when !test_nb = -1 && !read_last_index <> !i -> begin (*TEST_NB*)
                match int_of_string_opt s with
                | None -> begin
                    print_usage argv.(0);
                    Printf.printf "%s: error: invalid argument : '%s'\n" proj_name s;
                    continue := false;
                    exit :=  true
                end
                | Some n when n <= 0 -> begin
                    print_usage argv.(0);
                    Printf.printf "%s: error: invalid argument (should be positive) : '%s'\n" proj_name s;
                    continue := false;
                    exit :=  true
                end
                | Some n -> begin
                    test_nb := n;
                    read_last_index := !i;
                    incr i
                end
            end
            | s when !k = -1 && !read_last_index <> !i -> begin (*k*)
                match int_of_string_opt s with
                | None -> begin
                    print_usage argv.(0);
                    Printf.printf "%s: error: invalid argument : '%s'\n" proj_name s;
                    continue := false;
                    exit :=  true
                end
                | Some n when n <= 0 -> begin
                    print_usage argv.(0);
                    Printf.printf "%s: error: invalid argument (should be positive) : '%s'\n" proj_name s;
                    continue := false;
                    exit :=  true
                end
                | Some n -> begin
                    k := n;
                    read_last_index := !i;
                    incr i
                end
            end
            | s' when !read_last_index <> !i -> begin
                print_usage argv.(0);
                Printf.printf "%s: error: unrecognized argument: '%s'\n" proj_name s';
                continue := false;
                exit := true
            end
            | _ -> ()
        done;

        (*Use arguments*)
        if not !exit && !tests then
            Test.test ()

        else if not !exit && !print_index <> -1 then
            Print_mnist.print_image_and_label true !print_index

        else if (!train_nb = -1 || !test_nb = -1 || !k = -1) && not !exit then begin
            print_usage argv.(0);
            Printf.printf "%s: error: the following arguments are requied: TRAIN_NB TEST_NB K\n" proj_name;
            exit := true
        end

        else if not !exit then begin
            if !kd_tree then
                Printf.printf "Option -kd not implemented yet. Ignoring this argument.\n";

            (*TODO: use verbose*)
            let rate = test_classify !train_nb !test_nb !k in
            Printf.printf "Success rate : %f%s\n" rate "%"
        end
    end;;


let _ : unit =
    main Sys.argv;;
