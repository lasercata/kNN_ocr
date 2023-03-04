let print_image (image : int array) : unit =
  failwith "not implemented"

let main (_ : unit) : unit =
  let train_images = Mnist.open_in "train-images-idx3-ubyte" in
  let train_labels = Mnist.open_in "train-labels-idx1-ubyte" in
  let index = int_of_string Sys.argv.(1) in
  let image = Mnist.get train_images index in
  let number = (Mnist.get train_labels index).(0) in
  print_image image;
  Printf.printf "Label: %d\n" number

let _ : unit =
  main ()
