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
test_func "Seq_test.count_item" (
    Seq_test.count_item 1 s1 = 5 &&
    Seq_test.count_item 42 s1 = 0 &&
    Seq_test.count_item 8 s1 = 2 &&
    Seq_test.count_item 3 s1 = 1 &&
    Seq_test.count_item 0 s2 = 0
);;
test_func "Seq_test.most_frequent" (
    Seq_test.most_frequent s1 = 1
);;

