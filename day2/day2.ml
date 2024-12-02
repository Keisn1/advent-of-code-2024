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


let rec is_safe_desc l =
  match l with
  | [] | [ _ ] -> true
  | a :: (b :: _ as tail) -> if  b < a && (abs (a-b) <= 3) then is_safe_desc tail else false

let rec is_safe_asc l =
  match l with
  | [] | [_] -> true
  | a :: (b :: _ as tail) -> if b > a && (abs (a-b) <= 3) then is_safe_asc tail else false

let is_safe l = (is_safe_asc l) || (is_safe_desc l)

let sum_true l = List.fold_left (fun acc x -> if x then acc + 1 else acc) 0 l

let lines =
  read_file "input"
  |> List.map (String.split_on_char ' ')
  |> List.map (List.map int_of_string)
  |> List.map is_safe
  |> sum_true
