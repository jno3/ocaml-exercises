type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
[@@deriving show]

let rec string_of_tree tree = 
  match tree with
    | Empty -> "."
    | Node (value, left, right) -> Printf.sprintf "(%c %s %s)" value (string_of_tree left) (string_of_tree right)

let cbal_tree n =
  let rec aux curr_n =
    if curr_n = 0 then Empty
    else
      let left_n = (curr_n - 1) / 2 in
      let right_n = curr_n - 1 - left_n in
      Node('x', aux left_n, aux right_n)
  in
  aux n


let () =
  print_endline (string_of_tree (cbal_tree 7))
