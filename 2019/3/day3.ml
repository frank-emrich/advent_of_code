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

module Position_Comparable = Core.Tuple.Comparable(Core.Int)(Core.Int)
module Position_Map = Position_Comparable.Map

let build_pos_map movements : (int Position_Map.t) =
  let handle_pos (map : int Position_Map.t) pos step_count =
    let min_steps_to_pos =
      match Position_Map.find map pos with
      | None -> step_count
      | Some prev_steps -> min step_count prev_steps
    in
    Position_Map.set map ~key:pos ~data:min_steps_to_pos
  in
  let rec follow_step acc pos step remaining_steps step_count =
    if remaining_steps = 0 then
      acc, pos, step_count
    else
      let new_pos = move_pos pos step in
      let new_acc = handle_pos acc new_pos step_count in
      follow_step new_acc new_pos step (remaining_steps - 1) (step_count + 1)
  in
  let follow_movement (acc, pos, step_count) mov =
    let step = single_step_of_movement mov in
    let dist = dist_of_movement mov in
    let new_acc, new_pos, new_step_count =
      follow_step acc pos step dist step_count
    in
    new_acc, new_pos, new_step_count
  in
  let map, _, _ =
    List.fold_left follow_movement (Position_Map.empty, (0,0), 1) movements in
  map


let find_intersections pos_map_1 pos_map_2 =
  let check_intersection  ~key:pos ~data:steps_1 intersections =
    match Position_Map.find pos_map_2 pos with
      | None -> intersections
      | Some steps_2 ->
         (pos, steps_1, steps_2) :: intersections
  in
  Position_Map.fold pos_map_1 ~init:[] ~f:check_intersection


let find_min_intersection mk_init min intersections =
  let min_init = List.hd intersections |> mk_init in
  List.fold_left min min_init intersections


let solve () =
  let input = Aoc_utils.read_file "3/data.txt" in
  let mk_position_map str =
    String.split_on_char ',' str
    |> List.map parse_mov
    |> build_pos_map
  in

  let pos_map_1 = mk_position_map @@ List.nth input 0 in
  let pos_map_2 = mk_position_map @@ List.nth input 1 in
  let intersections = find_intersections pos_map_1 pos_map_2 in

  let man_dist ((x, y), _, _) = abs x + abs y in
  let comb_steps (_, steps_1, steps_2) = steps_1 + steps_2 in
  let mk_min f = (fun m i -> min m @@ f i) in

  let first_part =
    find_min_intersection man_dist (mk_min man_dist) intersections in
  let second_part =
    find_min_intersection comb_steps (mk_min comb_steps) intersections in


  [string_of_int first_part; string_of_int second_part]
