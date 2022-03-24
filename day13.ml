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

module Arcade = struct
  type tile_kind = Empty | Wall | Block | Paddle | Ball | Other of int

  let string_of_tile_kind = function
    | Empty -> " "
    | Wall -> "|"
    | Block -> "X"
    | Ball -> "O"
    | Paddle -> "■"
    | Other _ -> failwith "must not print Other"

  let int_of_tile_kind = function
    | Empty -> 0
    | Wall -> 1
    | Block -> 2
    | Ball -> 4
    | Paddle -> 3
    | Other v -> v

  let tile_kind_of_int = function
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Paddle
    | 4 -> Ball
    | o -> Other o

  module Position = struct
    module T = struct
      type t = int * int [@@deriving sexp, ord]
    end

    include T
    include Comparable.Make (T)

    let score_dummy = (-1, 0)
    let dummy = (-1, -1)
  end

  module State = struct
    type t = {
      blocks : bool Array.t Array.t;
      score : int;
      ball : Position.t;
      paddle : Position.t;
    }

    let of_list list =
      let rec go blocks score ball paddle wall_max_x wall_max_y = function
        | -1 :: 0 :: score :: rest ->
            go blocks score ball paddle wall_max_x wall_max_y rest
        | x :: y :: 0 :: rest ->
            go
              (Set.remove blocks (x, y))
              score ball paddle wall_max_x wall_max_y rest
        | x :: y :: 1 :: rest ->
            go blocks score ball paddle (Int.max wall_max_x x)
              (Int.max wall_max_y y) rest
        | x :: y :: 2 :: rest ->
            go
              (Set.add blocks (x, y))
              score ball paddle wall_max_x wall_max_y rest
        | x :: y :: 3 :: rest ->
            go blocks score ball (x, y) wall_max_x wall_max_y rest
        | x :: y :: 4 :: rest ->
            go blocks score (x, y) paddle wall_max_x wall_max_y rest
        | [] ->
            if snd ball <> wall_max_y then failwith "ball unexpected"
            else
              let blocks =
                Array.init wall_max_y ~f:(fun y ->
                    Array.init (wall_max_x - 1) ~f:(fun x ->
                        Set.mem blocks (x, y)))
              in
              { blocks; score; ball; paddle }
        | _ -> failwith "bad list"
      in
      go (Set.empty (module Position)) 0 Position.dummy Position.dummy 0 0 list
  end

  module Output = struct
    let clean output =
      let rec go seen_pos = function
        | _ :: y :: x :: rest when Set.mem seen_pos (x, y) -> go seen_pos rest
        | v :: y :: x :: rest ->
            v :: y :: x :: go (Set.add seen_pos (x, y)) rest
        | [] -> []
        | _ -> failwith "bad output"
      in
      go (Set.empty (module Position)) output
  end

  module Screen = struct
    type t = (Position.t, tile_kind, Position.comparator_witness) Map.t

    let of_list l =
      let rec go map = function
        | x :: y :: tile_id :: rest ->
            go (Map.set map ~key:(x, y) ~data:(tile_kind_of_int tile_id)) rest
        | [] -> map
        | _ -> failwith "bad output"
      in
      go (Map.empty (module Position)) l

    let score screen =
      match Map.find screen Position.score_dummy with
      | Some k -> int_of_tile_kind k
      | None -> 0

    let bounds screen =
      let take_max ~key:(x, y) ~data:_ (max_x, max_y) =
        (Int.max max_x x, Int.max max_y y)
      in
      Map.fold screen ~f:take_max ~init:(0, 0)

    let draw screen =
      let max_x, max_y = bounds screen in

      let matrix =
        Array.init (max_y + 1) ~f:(fun _ ->
            Array.init (max_x + 1) ~f:(Fun.const Empty))
      in

      Map.iteri screen ~f:(fun ~key:(x, y) ~data ->
          if Position.equal (x, y) Position.score_dummy then ()
          else matrix.(y).(x) <- data);

      let string_of_line line_array =
        Array.to_list line_array
        |> List.map ~f:string_of_tile_kind
        |> String.concat ~sep:""
      in
      Printf.printf "Score: %d\n" (score screen);
      Array.iter matrix ~f:(fun line ->
          Printf.printf "%s\n" (string_of_line line))

  end
end

let part2 () =
  let initial_prog_state =
    PS.write_memory (M.create_initial_state "day13_data.txt") 0 (Z.of_int 2)
  in

  let print state =
    let screen =
      PS.output state |> List.rev |> List.map ~f:Z.to_int
      |> Arcade.Screen.of_list
    in
    Arcade.Screen.draw screen
  in

  let read_line () =
    Out_channel.(flush stdout);
    In_channel.(input_line_exn stdin)
  in
  let get_input state =
    print state;
    let rec go () =
      Printf.printf "Choose joystick movement (L,R,N)";
      match read_line () with "L" -> -1 | "R" -> 1 | "N" -> 0 | _ -> go ()
    in
    Z.of_int (go ())
  in
  Printf.printf "hello!\n";
  M.eval_input_loop initial_prog_state ~get_input |> print

let solve () = [ part1 () |> Int.to_string ]
