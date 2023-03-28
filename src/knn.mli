(** Type des données à classer *)
(*type data = float array*)
type data = int array

(** Type interne à ce module pour représenter ses données d'entrainement.
    Dans la partie sans prétraitement, on choisit simplement la séquence des
    couples (donnée, étiquette).
    Dans la partie avec arbre k-d, ce sera un arbre dimensionnel. *)
type 'label t

val count_item : 'a -> 'a list -> int
val most_frequent : int list -> int

val mnist_seq : int -> Mnist.idx -> Mnist.idx -> (int array * int) Seq.t

(** Initialisation des données d'entrainement *)
val init : (data * 'label) Seq.t -> 'label t

(** Classification d'une donnée *)
(*val classify : 'label t -> int -> data -> 'label*)
val classify : (data * int) Seq.t -> int -> data -> (data -> data -> int) -> int

val test_classify : int -> int -> int -> int -> bool -> bool -> float * (int array array)
