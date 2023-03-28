let arr_max (arr : int array array) : int =
    (*Return the number of digits in the longest int from `arr`.*)

    let mx = ref 0 in
    for i = 0 to Array.length arr - 1 do
        for j = 0 to Array.length arr - 1 do
            if arr.(i).(j) > !mx then
                mx := arr.(i).(j)
        done
    done;

    String.length (string_of_int !mx);;

let print_conf (m : int array array) : unit =
    (*
     * Prints the confusion matrix.
     * It automatically detect the size of the integers in it and take
     * a good shape.
     *)

    let n = arr_max m in

    Printf.printf "\nThe confusion matrix :\n     ";

    for k = 0 to Array.length m - 1 do
        Printf.printf "%d%*s" (k + 1) n ""
    done;

    Printf.printf "\n   +";

    for k = 0 to (Array.length m) * (n + 1) do
        Printf.printf "-";
    done;

    print_newline ();

    for i = 0 to Array.length m - 1 do
        if i + 1 < 10 then
            Printf.printf " %d | " (i + 1)
        else
            Printf.printf "%d | " (i + 1);

        for j = 0 to Array.length m.(0) - 1 do
            Printf.printf "%*d " n m.(i).(j)
        done;
        Printf.printf "\n"
    done;;


let print_usage (argv0 : string) =
    (*
     * Print the basic help for the command line.
     *
     * - argv0 : the program name.
     *)

    Printf.printf "Usage : %s [-h] [-v] [-t] [-p INDEX] [-b] [-d DIST] [-kd] TRAIN_NB TEST_NB K\n" argv0;;

let print_help (argv0 : string) =
    (*
     * Print the help message for the command line parser.
     *
     * - argv0 : the program name.
     *)

    print_usage argv0;

    Printf.printf "\nRecognize images from the MNIST data base.\n";

    Printf.printf "\nPositional arguments :\n";
    Printf.printf "    TRAIN_NB                  The number of train images\n";
    Printf.printf "    TEST_NB                   The number of test images\n";
    Printf.printf "    K                         The kNN parameter\n";

    Printf.printf "\nOptional arguments :\n";
    Printf.printf "    -h, --help                Print this help message and exit\n";
    Printf.printf "    -v, --verbose             Show confusion matrix and elapsed time\n";
    Printf.printf "    -t, --test                Run tests and exit (ignore positional arguments)\n";
    Printf.printf "    -kd, --kd-tree            Use a kd tree\n";
    Printf.printf "    -b, --binarize            Preprocess the image by keeping only two colors\n";
    Printf.printf "    -d DIST, --distance DIST  Give the used distance. Possible values are :\n";
    Printf.printf "                                  - 0 : the square of the euclidean distance (default) ;\n";
    Printf.printf "                                  - 1 : same, but only in the 20 x 20 pixels\n";
    Printf.printf "                                        center of the image ;\n";
    Printf.printf "                                  - 2 : binarize image before applying the distance.\n";
    Printf.printf "    -p INDEX, --print INDEX   Print the image at position INDEX and exit (ignore\n";
    Printf.printf "                              positional arguments)\n";;


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
    and bin = ref false
    and d = ref 0
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
            | "-b" | "--binarize" -> begin
                bin := true;
                incr i
            end
            | "-d" | "--distance" -> begin
                if !i + 1 > argc - 1 then begin
                    print_usage argv.(0);
                    Printf.printf "%s: error: the argument '%s' needs DIST\n" proj_name argv.(!i);
                    exit := true;
                    continue := false;
                end
                else begin
                    match int_of_string_opt argv.(!i + 1) with
                    | None -> begin
                        print_usage argv.(0);
                        Printf.printf "%s: error: unrecognized argument for '%s' : '%s'\n" proj_name argv.(!i) argv.(!i + 1);
                        continue := false;
                        exit := true
                    end
                    | Some n when n < 0 || n > 3 -> begin
                        print_usage argv.(0);
                        Printf.printf "%s: error: invalid argument (should be 0, 1, or 2) for '%s' : '%s'\n" proj_name argv.(!i) argv.(!i + 1);
                        continue := false;
                        exit :=  true
                    end
                    | Some n -> begin
                        d := n;
                        i := !i + 2
                    end
                end
            end
            | "-p" | "--print" -> begin
                if !i + 1 > argc - 1 then begin
                    print_usage argv.(0);
                    Printf.printf "%s: error: the argument '%s' needs INDEX\n" proj_name argv.(!i);
                    exit := true;
                    continue := false;
                end
                else begin
                    match int_of_string_opt argv.(!i + 1) with
                    | None -> begin
                        print_usage argv.(0);
                        Printf.printf "%s: error: unrecognized argument for '%s' : '%s'\n" proj_name argv.(!i) argv.(!i + 1);
                        continue := false;
                        exit := true
                    end
                    | Some n when n < 0 -> begin
                        print_usage argv.(0);
                        Printf.printf "%s: error: invalid argument (should be positive) for '%s' : '%s'\n" proj_name argv.(!i) argv.(!i + 1);
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

            let t = Sys.time () in
            let rate, confusion = Knn.test_classify !train_nb !test_nb !k !d !verbose !bin in
            Printf.printf "Success rate : %.03f%s\n" rate "%";
            if !verbose then begin
                print_conf confusion;
                Printf.printf "\nTime elpased : %.03fs.\n" (Sys.time () -. t)
            end
        end
    end;;


let _ : unit =
    main Sys.argv;;
