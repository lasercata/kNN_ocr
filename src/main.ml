let main (_ : unit) : unit =
  let train_images = Mnist.open_in "train-images-idx3-ubyte" in
  let train_labels = Mnist.open_in "train-labels-idx1-ubyte" in
  let test_images = Mnist.open_in "t10k-images-idx3-ubyte" in
  let test_labels = Mnist.open_in "t10k-labels-idx1-ubyte" in
  failwith "not implemented"

let _ : unit =
  main ()
