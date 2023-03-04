(** Type des files de priorité *)
type 'a t

(** Création d'une file vide.
    On donne une fonction de comparaison pour les éléments de la file. *)
val create : ('a -> 'a -> int) -> 'a t

(** Nombre d'éléments contenus dans une file. *)
val size : 'a t -> int

(** Élément en tête de la file.
    Il s'agit de l'élément minimal pour la fonction de comparaison. *)
val top : 'a t -> 'a

(** Suppression de la tête de la file. *)
val remove_top : 'a t -> 'a t

(** Changement de la tête de la file.
 * Équivaut à une suppression de la racine puis un ajout, mais s'exécute plus
 * rapidement que les deux opérations séparées.
 *)
val change_root : 'a t -> 'a -> 'a t

(** Insertion d'un élément dans la file. *)
val insert : 'a t -> 'a -> 'a t

(** Obtenir tous les élements de la file, sans garantie d'ordre. *)
val to_unsorted_list : 'a t -> 'a list
