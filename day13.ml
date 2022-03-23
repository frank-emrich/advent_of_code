open! Core
module PS = Machine.Pure_state
module M = Machine.Make (PS)

let part1 () =
  let initial_prog_state = M.create_initial_state "day13_data.txt" in
  let final_state = M.eval initial_prog_state in
  let output = PS.output final_state |> List.rev in
  let rec count_block_tiles output =
    match output with
    | _ :: _ :: tile_id :: xs when Z.equal tile_id (Z.of_int 2) ->
        1 + count_block_tiles xs
    | _ :: _ :: _ :: xs -> count_block_tiles xs
    | _ -> 0
  in
  let count = count_block_tiles output in
  Printf.printf "output: %s\n"
    (String.concat ~sep:", " (List.map ~f:Z.to_string output));
  count

module Arcade = struct end

let part2 () = failwith "todo"
let solve () = [ part1 () |> Int.to_string ]
