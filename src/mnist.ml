type datatype =
  | UByte
  | Byte
  | Short
  | Int

type idx =
  {
    channel : in_channel;
    datatype : datatype;
    dimensions : int;
    size : int array;
    dim0_size : int;
  }

let datatype_of_code code =
  match code with
  |'\x08' -> UByte
  |'\x09' -> Byte
  |'\x0B' -> Short
  |'\x0C' -> Int
  | _ -> invalid_arg "datatype_of_code"

let item_size datatype =
  match datatype with
  | UByte -> 1
  | Byte -> 1
  | Short -> 2
  | Int -> 4

let of_channel channel =
  seek_in channel 0;
  set_binary_mode_in channel true;
  let _ = input_char channel in
  let _ = input_char channel in
  let datatype = datatype_of_code (input_char channel) in
  let dimensions = int_of_char (input_char channel) in
  let size = Array.make dimensions 0 in
  for i = 0 to dimensions - 1 do
    size.(i) <- input_binary_int channel
  done;
  let dim0_size =
    Array.fold_left (fun acc x -> if acc = -1 then 1 else acc * x) (-1) size
  in
  { channel; datatype; dimensions; size ; dim0_size }

let open_in filename =
  of_channel (Stdlib.open_in filename)

let get_input_function datatype =
  match datatype with
  | UByte
    | Byte -> input_byte
  | Short ->
     fun channel ->
     let c1 = input_byte channel in
     let c2 = input_byte channel in
     c1 lsl 8 + c2
  | Int -> input_binary_int

let get idx pos =
  seek_in idx.channel (item_size Int + item_size Int * idx.dimensions +
                         pos * idx.dim0_size * item_size idx.datatype);
  let res = Array.make idx.dim0_size 0 in
  let input_function = get_input_function idx.datatype in
  for i = 0 to Array.length res - 1 do
    res.(i) <- input_function idx.channel
  done;
  res
