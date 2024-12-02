let read_file filename =
  let rec read_lines_rec chan acc =
    try
      let line = input_line chan in
      read_lines_rec chan (line :: acc)
    with End_of_file ->
      close_in chan;
      List.rev acc
  in
  let chan = open_in filename in
  read_lines_rec chan []

let (left_nbrs, right_nbrs) =
  "input_simple"
  |> read_file
  |> List.map (String.split_on_char ' ')
  |> List.map (List.filter (fun s -> String.length s > 0))
  |> List.map (List.map (int_of_string))
  |> List.fold_left (fun (left_nbrs, right_nbrs) pair ->
       match pair with
       | [nbr1; nbr2] -> (nbr1 :: left_nbrs, nbr2 :: right_nbrs)
       | _ -> failwith "Each sublist must contain exactly 2 integers"
     )  ([], [])
  |> fun (left, right) ->
    (List.sort compare left, List.sort compare right)

let rec sum_lists list_pair =
  match list_pair with
  | ([], []) -> 0
  | ([], _) -> 0
  | (_, []) -> 0
  | (x::xs, y::ys) -> (abs (x - y)) + (sum_lists (xs, ys))

let () = print_int (sum_lists (left_nbrs, right_nbrs))
