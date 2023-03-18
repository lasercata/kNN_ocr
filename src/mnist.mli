(** Types des données pouvant être contenues dans le fichier IDX *)
type datatype = UByte | Byte | Short | Int

(** Représentation opaque d'un fichier IDX *)
type idx =
  {
    channel : in_channel;
    datatype : datatype;
    dimensions : int;
    size : int array;
    dim0_size : int;
  }

(** Création à partir d'un fichier préalablement ouvert *)
val of_channel : in_channel -> idx

(** Ouverture en lecture à partir d'un nom de fichier *)
val open_in : string -> idx

(** Obtenir la i-ème donnée *)
val get : idx -> int -> int array
