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

let lines =
  read_file "input"
  |> String.concat "";;

Str.regexp;;
