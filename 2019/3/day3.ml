type movement =
  | R of int
  | L of int
  | D of int
  | U of int


let parse_mov mov_str =
  let len = String.length mov_str in
  let dist =
    String.sub mov_str 1 (len - 1)
    |> int_of_string
  in
  match mov_str.[0] with
  | 'R' -> R dist
  | 'L' -> L dist
  | 'D' -> D dist
  | 'U' -> U dist
  | _ -> failwith "Could not parse step"

type position = int * int

let single_step_of_movement =
  let sign n =
    if n > 0 then
      1
    else if n = 0 then
      0
    else
      -1
  in
  function
  | R d -> (sign d, 0)
  | L d -> (sign (-d), 0)
  | D d -> (0, sign (-d))
  | U d -> (0, sign d)

let manhattan_dist (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

let dist_of_movement = function
  | R d
  | L d
  | D d
  | U d -> d


let move_pos (x1, y1) (x2, y2) =
  (x1 + x2, y1 + y2)


let trace_path
    (pos_handler : 'a -> position -> 'a)
    (accumulator : 'a)
    (movements : movement list)
    position
       : ('a * position) =

  let rec follow_step acc pos step remaining_steps =
    Printf.printf "following position %d, %d\n" (fst pos) (snd pos);
    if remaining_steps = 0 then
      acc, pos
    else
      let new_pos = move_pos pos step in
      let new_acc = pos_handler acc new_pos in
      follow_step new_acc new_pos step (remaining_steps - 1)
  in
  let follow_movement (acc, pos) mov =
    let step = single_step_of_movement mov in
    let dist = dist_of_movement mov in
    let new_acc, new_pos = follow_step acc pos step dist in
    new_acc, new_pos
  in
  let acc, (x, y) = List.fold_left follow_movement (accumulator, position) movements in
  Printf.printf "final position of trace_path: %d, %d\n" x y;
  acc, (x, y)



module Position_Comparable = Core.Tuple.Comparable(Core.Int)(Core.Int)
module Position_Set = Position_Comparable.Set


let build_pos_set movements =
  let initial_set = Position_Set.empty in
  let pos_set, _ =
    trace_path Position_Set.add initial_set movements (0,0) in
  pos_set


let find_intersections movements position_set =
  let check_intersection intersections pos =
    if Position_Set.mem position_set pos then
      (Printf.printf "Found intersection at %d,%d\n" (fst pos) (snd pos);
      pos :: intersections)
    else
      intersections
  in
  trace_path check_intersection [] movements (0, 0) |> fst

let find_max_dist =
  let dist (x, y) = abs x + abs y in
  let min_init = List.hd points |> dist in
  List.fold_left
    (fun m p -> min m @@ dist p)
    min_init



let solve () =
  let input = Aoc_utils.read_file "3/data.txt" in
  let first_wire =
    List.nth input 0
    |> String.split_on_char ','
    |> List.map parse_mov
  in
  let second_wire =
    List.nth input 1
    |> String.split_on_char ','
    |> List.map parse_mov
  in
  let first_part =
    build_pos_set first_wire
    |> find_intersections second_wire
    |> find_max_dist
  in

  [string_of_int first_part]
