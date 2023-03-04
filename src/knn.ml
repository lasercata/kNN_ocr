type data = float array
type 'label t = (data * 'label) Seq.t

let init (seq : (data * 'label) Seq.t) : 'label t =
  seq

let classify (seq : 'label t) (k : int) (x : data) : 'label =
  failwith "not implemented"
