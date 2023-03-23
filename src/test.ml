(*File that define tests.*)

let test_func (func_name : string) (t : bool) : unit =
    Printf.printf "\ttesting %s \t: %s\n" func_name (
        if t then
            "passed"
        else
            "Wrong result !"
    );;


(*Test seq_test.ml functions*)
Printf.printf "Testing Seq_test functions :\n"

let l1 = [1; 1; 1; 4; 2; 8; 7; 1; 3; 8; 6; 1; 12];;
let s1 = Seq_test.seq_of_list l1;;
let l2 = [];;
let s2 = Seq_test.seq_of_list l2;;
let l3 = [1; 2; 3];;
let s3 = Seq_test.seq_of_list l3;;

test_func "Seq_test.length" (
    List.length l1 = Seq_test.length s1 &&
    List.length l2 = Seq_test.length s2 &&
    List.length l3 = Seq_test.length s3
);;
test_func "Seq_test.get\t" (
    Seq_test.get s1 0 = 1 &&
    Seq_test.get s1 3 = 4 &&
    Seq_test.get s3 2 = 3
);;
Printf.printf "\ttesting Seq_test.iteri \t\t:\n";;
Seq_test.iteri (
    fun i s -> Printf.printf "\t\t%d: %d\n" i s
) s1;;

(*Testing Knn*)
Printf.printf "\nTesting Knn functions :\n";;
test_func "Knn.count_item\t" (
    Knn.count_item 1 l1 = 5 &&
    Knn.count_item 42 l1 = 0 &&
    Knn.count_item 8 l1 = 2 &&
    Knn.count_item 3 l1 = 1 &&
    Knn.count_item 0 l2 = 0
);;
test_func "Knn.most_frequent" (
    Knn.most_frequent l1 = 1
);;

(*Test knn.ml functions*)
Printf.printf "\nTesting Knn functions :\n";;

Printf.printf "Knn.mnist_seq :\n";;
let train_images = Mnist.open_in "train-images-idx3-ubyte" in
let train_labels = Mnist.open_in "train-labels-idx1-ubyte" in
Seq.iter (
    fun s -> let img, lb = s in Printf.printf "\tlabel : %d\n" lb
)
(
    Knn.mnist_seq 10 train_images train_labels
);;
