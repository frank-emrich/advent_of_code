open! Core

module Color = struct
  type t = White | Black

  let of_int_exn = function
    | 0 -> Black
    | 1 -> White
    | _ -> failwith "illegal color int"

  let to_int = function Black -> 0 | White -> 1

  let to_char = function Black -> ' ' | White -> 'X'
end

module Direction = struct
  type t = Up | Down | Left | Right

  let of_string = function
    | "^" -> Some Up
    | ">" -> Some Right
    | "<" -> Some Left
    | "v" -> Some Down
    | _ -> None

  let to_int = function Up -> 0 | Right -> 1 | Down -> 2 | Left -> 3

  let of_int_exn = function
    | 0 -> Up
    | 1 -> Right
    | 2 -> Down
    | 3 -> Left
    | bad -> failwith (Printf.sprintf "bad direction: %d\n" bad)

  let turn_left d = (to_int d + 3) mod 4 |> of_int_exn

  let turn_right d = (to_int d + 5) mod 4 |> of_int_exn

  let to_delta_xy = function
    | Up -> (0, 1)
    | Right -> (1, 0)
    | Down -> (0, -1)
    | Left -> (-1, 0)
end

module Point = struct
  module T = struct
    type t = int * int [@@deriving show, compare, sexp]
  end

  include T
  include Comparator.Make (T)

  let move (x, y) direction =
    let delta_x, delta_y = Direction.to_delta_xy direction in
    (x + delta_x, y + delta_y)
end

module Field = struct
  type t = (Point.t, Color.t, Point.comparator_witness) Map.t

  let initial_color = Color.Black

  let color_at field point =
    Option.value ~default:initial_color (Map.find field point)

  let set_color_at field point color = Map.set field ~key:point ~data:color

  let initial = Map.empty (module Point)

  (** Returns bottom left and top right point such that the resulting rectangle encloses all points *)
  let bounding_box field =
    let update_bounds ~key:(x, y) ~data:_ (min_x, min_y, max_x, max_y) =
      let min_x = min min_x x in
      let min_y = min min_y y in
      let max_x = max max_x x in
      let max_y = max max_y y in
      (min_x, min_y, max_x, max_y)
    in

    let origin_bounds = (0, 0, 0, 0) in
    let min_x, min_y, max_x, max_y =
      Map.fold field ~f:update_bounds ~init:origin_bounds
    in
    ((min_x, min_y), (max_x, max_y))

  let show field =
    let (((min_x, min_y), (max_x, max_y)) as _b_box) = bounding_box field in
    let width = max_x - min_x + 1 in
    let height = max_y - min_y + 1 in
    let shift_x x = x - min_x in
    let shift_y y = y - min_y in
    let field_array =
      Array.make_matrix ~dimx:width ~dimy:height (Color.to_char Color.Black)
    in

    let set_color ~key:(x, y) ~data:color () =
      field_array.(shift_x x).(shift_y y) <- Color.to_char color
    in
    Map.fold field ~f:set_color ~init:();

    let convert_row row = Array.to_list row |> String.of_char_list in
    let rows = Array.map field_array ~f:convert_row in
    let rows = Array.to_list rows in
    String.concat ~sep:"\n" rows
end

module Painting_robot = struct
  module PS = Machine.Pure_state
  module M = Machine.Make (PS)

  type state = {
    field : Field.t;
    position : int * int;
    direction : Direction.t;
    colored_fields : (Point.t, Point.comparator_witness) Set.t;
  }

  let initial_state color_of_first =
    {
      field = Map.set Field.initial ~key:(0, 0) ~data:color_of_first;
      position = (0, 0);
      direction = Up;
      colored_fields = Set.empty (module Point);
    }

  let count_repainting = false

  let load_initial_prog_state () = M.create_initial_state "11/data.txt"

  let run initial_color =
    let initial_m_state = load_initial_prog_state () in

    let provide_color m_state r_state =
      let color = Field.color_at r_state.field r_state.position in
      PS.push_input m_state (Color.to_int color |> Z.of_int)
    in

    let paint_and_update m_state r_state =
      assert (PS.is_awaiting_input m_state);

      match PS.output m_state with
      | turn_direction :: paint_color :: _ ->
          let new_dir =
            if Z.to_int turn_direction = 0 then
              Direction.turn_left r_state.direction
            else Direction.turn_right r_state.direction
          in
          let old_color = Field.color_at r_state.field r_state.position in
          let paint_color = Z.to_int paint_color |> Color.of_int_exn in
          let new_colored =
            if count_repainting || Poly.(old_color <> paint_color) then
              Set.add r_state.colored_fields r_state.position
            else r_state.colored_fields
          in
          let new_field =
            Field.set_color_at r_state.field r_state.position paint_color
          in
          let new_pos = Point.move r_state.position new_dir in
          {
            field = new_field;
            position = new_pos;
            direction = new_dir;
            colored_fields = new_colored;
          }
      | _ -> r_state
    in

    let rec go r_state m_state =
      assert (PS.is_awaiting_input m_state);

      let m_state = provide_color m_state r_state in
      let m_state = M.eval m_state in
      if PS.is_finished m_state then r_state
      else
        (* (Printf.printf "awaiting input, output length is %d\n" (PS.output m_state |> List.length); *)
        let r_state = paint_and_update m_state r_state in
        go r_state m_state
    in

    let m_state = M.eval initial_m_state in
    go (initial_state initial_color) m_state
end

let solve () =
  let run_robot initial_color = Painting_robot.run initial_color in

  let part1_final = run_robot Black in
  let part_1 = Set.length part1_final.colored_fields |> Int.to_string in

  let part2_final = run_robot White in

  [ part_1; Field.show part2_final.field ]
