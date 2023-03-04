type 'a heaptree =
  | Leaf
  | Node of 'a heaptree * 'a * 'a heaptree

type 'a t =
  {
    size : int;
    tree : 'a heaptree;
    compare : 'a -> 'a -> int;
  }

let create compare =
  { size = 0; tree = Leaf ; compare }

let top queue =
  match queue.tree with
  | Leaf -> invalid_arg "top"
  | Node(_, x, _) -> x

let size queue =
  queue.size

let rec path_of_size size =
  let rec aux size =
    if size <= 1
    then []
    else (size mod 2 = 0) :: aux (size / 2)
  in List.rev (aux size)

let rec remove_node tree path =
  match (tree, path) with
  | (Leaf, _) -> assert false
  | (Node(_, x, _), []) -> (Leaf, x)
  | (Node(left, x, right), true :: path) ->
     let (new_left, last) = remove_node left path
     in (Node(new_left, x, right), last)
  | (Node(left, x, right), false :: path) ->
     let (new_right, last) = remove_node right path
     in (Node(left, x, new_right), last)

let change_root_unsafe tree new_root =
  match tree with
  | Leaf -> invalid_arg "change_root_unsafe"
  | Node(left, _, right) -> Node(left, new_root, right)

let rec repair_top_bottom compare tree =
  match tree with
  | Node((Node(_, left_x, _) as left), x, (Node(_, right_x, _) as right)) ->
     if compare left_x x < 0
     then
       if compare right_x left_x < 0
       then Node(left, right_x,
                 repair_top_bottom compare (change_root_unsafe right x))
       else Node(repair_top_bottom compare (change_root_unsafe left x), left_x,
                 right)
     else
       if compare right_x x < 0
       then Node(left, right_x,
                 repair_top_bottom compare (change_root_unsafe right x))
       else tree

  | Node((Node(_, left_x, _) as left), x, Leaf) when compare left_x x < 0 ->
     Node(repair_top_bottom compare (change_root_unsafe left x), left_x, Leaf)

  | _ -> tree

let change_root heap item =
  { heap with tree = repair_top_bottom heap.compare
                       (change_root_unsafe heap.tree item) }

let remove_top heap =
  let (tree_without_last, last) =
    remove_node heap.tree (path_of_size heap.size)
  in
  {
    size = heap.size - 1;
    tree = repair_top_bottom heap.compare
             (change_root_unsafe tree_without_last last);
    compare = heap.compare;
  }

let repair_root compare tree =
  let min2 x y = if compare x y < 0 then x else y in
  let min3 x y z = min2 (min2 x y) z in
  match tree with
  | Leaf -> Leaf
  | Node(Node(ll, left_x, lr), x, Node(rl, right_x, rr)) ->
     let root = min3 left_x x right_x in
     Node(
         Node(ll, (if root = left_x then x else left_x), lr),
         root,
         Node(rl, (if root = right_x then x else right_x), rr)
       )
  | Node(Node(ll, left_x, lr), x, Leaf) ->
     let root = min2 left_x x in
     Node(
         Node(ll, (if root = left_x then x else left_x), lr),
         root,
         Leaf
       )
  | _ -> tree

let insert heap item =
  let rec insert_node tree path =
    match (tree, path) with
    | (Leaf, []) -> Node(Leaf, item, Leaf)
    | (Node(left, x, right), true :: path) ->
       repair_root heap.compare (Node(insert_node left path, x, right))
    | (Node(left, x, right), false :: path) ->
       repair_root heap.compare (Node(left, x, insert_node right path))
    | _ -> assert false
  in
  {
    size = heap.size + 1;
    tree = insert_node heap.tree (path_of_size (heap.size + 1));
    compare = heap.compare;
  }

let to_unsorted_list heap =
  let rec search tree =
    match tree with
    | Leaf -> []
    | Node(left, x, right) -> x :: search left @ search right
  in search heap.tree
