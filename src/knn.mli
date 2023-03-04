(** Type des données à classer *)
type data = float array

(** Type interne à ce module pour représenter ses données d'entrainement.
    Dans la partie sans prétraitement, on choisit simplement la séquence des
    couples (donnée, étiquette).
    Dans la partie avec arbre k-d, ce sera un arbre dimensionnel. *)
type 'label t

(** Initialisation des données d'entrainement *)
val init : (data * 'label) Seq.t -> 'label t

(** Classification d'une donnée *)
val classify : 'label t -> int -> data -> 'label
