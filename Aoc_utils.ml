let read_file path =
  let chan = open_in path in
  let rec add_line prev_lines =
    try
      let next_line = input_line chan in
      add_line (next_line :: prev_lines)
    with
    | End_of_file ->
       prev_lines
  in
  add_line [] |> List.rev

let read_whole_file path =Core.In_channel.read_all path
