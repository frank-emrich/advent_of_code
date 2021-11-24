open! Core

module Point = struct
  module T =
  struct
    type t = int * int [@@deriving show, compare, sexp]

    (* type comparator_witness = Nothing.t *)
  end
  include T
  include Comparator.Make (T)

end

(* module Point_comparator = Comparator.Make(Point) *)


type line = {
  slope : float;
  y_intercept : float
}

let line_of_points p1 p2 =
  let (x1, y1) = p1 in
  let (x2, y2) = p2 in
  let y_diff = Float.of_int (y1 - y2) in
  let x_diff = Float.of_int (x1 - x2) in
  let slope = Float.( (y_diff) / (x_diff) ) in
  let y_intercept = Float.(Float.of_int y1 - slope * Float.of_int x1) in
  {slope; y_intercept}




(* module Point_comparator = Comparator.Make (Point) *)
module Float_approx = struct
  module T =
  struct
    type t = float [@@deriving compare, sexp]

    let epsilon = 0.0001

    (* type comparator_witness = Nothing.t *)

    let compare v1 v2 =
      let diff = Float.(v1 - v2) in
      if Float.(v1 = v2 || abs diff < epsilon) then 0
      else if Float.(diff < Float.zero) then -1
      else 1
  end
  include T
  include Comparator.Make (T)
end

(* module Float_approx_comparator = Comparator.Make(Float_approx) *)

module Asteroid_map = struct
  type points = Point.t array

  type slopes =
    (float, Float_approx.comparator_witness) Set.t

  (** We express sets of lines by mappings of the form s -> I,
      where [s] is the slope of the line, and [I] is a set of y-intercepts,
      each of which represents a line with the given slope and y-intercept*)

  type point_to_lines = (Point.t, slopes, Point.comparator_witness) Map.t

  type t = point_to_lines

  let parse_points_of_map map : Point.t array =
    let rows = String.split_lines map in
    (* let height = List.length rows in *)
    (* let width = String.length (List.hd_exn rows) in *)
    let parse_entry row_index (points, column_index) char =
      let asteroid_marker = '#' in
      let point = (column_index, row_index) in
      if Char.equal char asteroid_marker then
        (* (Printf.printf "found asteroid at %s\n" (Point.show point); *)
        (point :: points, column_index + 1)
      else (points, column_index + 1)
    in
    let parse_row row_index row =
      (* Printf.printf "parsing row %s\n" row; *)
      fst (String.fold row ~f:(parse_entry row_index) ~init:([], 0))
    in
    let rows = List.mapi rows ~f:parse_row in
    let array = List.concat rows |> Array.of_list in
    Array.sort ~compare:Point.compare array;
    array

  let parse_map map : t =
    let points = parse_points_of_map map in

    (* let empty_intercepts  = Set.empty (module Float_approx) in *)
    let empty_lines : slopes = Set.empty (module Float_approx) in
    let empty = Map.empty (module Point) in


    let build_line p1 (lines : slopes) p2 =
      if Point.compare p1 p2 = 0 then
        lines
      else
        let {slope; _} = line_of_points p1 p2 in

        let new_lines = Set.add lines slope in
        (* Printf.printf "Line from %s to %s has slope %f, now have %d slopes \n" (Point.show p1) (Point.show p2) slope (Set.length new_lines); *)
        new_lines

        (* let y_intercepts_for_slope = Option.value ~default:(empty_intercepts) (Map.find lines slope) in *)
        (* let y_intercepts_for_slope = Set.add y_intercepts_for_slope y_intercept in *)
        (* Map.set lines ~key:slope ~data:y_intercepts_for_slope *)
    in
    let build_lines (point_to_lines : t) p1 =
      (* let empty_lines = Set.empty (module Float_approx) in *)
      let lines : slopes = Array.fold points ~f:(build_line p1) ~init:empty_lines in
      Map.add_exn point_to_lines ~key:p1 ~data:lines
    in
    let point_to_lines : t = Array.fold points ~f:build_lines ~init:empty in
    point_to_lines

end

let map1 =
{|.#..#
.....
#####
....#
...##|} [@ocamlformat "disable=true"]


let best_asteroid ast_map =
  (* let count_in_set init lines = Set.fold ~init ~f:(fun sum set -> sum + Set.length set) in *)
  (* let count_lines lines = Map.fold lines ~init:0 ~f:(fun ~key:_ ~data:set sum -> sum + Set.length set) in *)
  let point_to_observable = Map.map ast_map ~f:Set.length in
  let choose_best ~key:point ~data:count ((_, max_count) as prev) =
    if count >= max_count then
      (point, count)
    else
      prev
  in
  Map.fold point_to_observable ~f:(choose_best) ~init:((0, 0), -1)


let test () =
  let map1 =
    ".#..#\n\
     .....\n\
     #####\n\
     ....#\n\
     ...##" [@ocamlformat "disable=true"]
  in
  let map2 =
    "......#.#.\n\
     #..#.#....\n\
     ..#######.\n\
     .#.#.###..\n\
     .#..#.....\n\
     ..#....#.#\n\
     #..#....#.\n\
     .##.#..###\n\
     ##...#..#.\n\
     .#....####" [@ocamlformat "disable=true"]
  in
  let map3 =
    "#.#...#.#.\n\
     .###....#.\n\
     .#....#...\n\
     ##.#.#.#.#\n\
     ....#.#.#.\n\
     .##..###.#\n\
     ..#...##..\n\
     ..##....##\n\
     ......#...\n\
     .####.###." [@ocamlformat "disable=true"]
  in
  let map4 =
    ".#..#..###\n\
     ####.###.#\n\
     ....###.#.\n\
     ..###.##.#\n\
     ##.##.#.#.\n\
     ....###..#\n\
     ..#.#..#.#\n\
     #..#.#.###\n\
     .##...##.#\n\
     .....#.#.." [@ocamlformat "disable=true"]
  in
  let map5 =
    ".#..##.###...#######\n\
     ##.############..##.\n\
     .#.######.########.#\n\
     .###.#######.####.#.\n\
     #####.##.#.##.###.##\n\
     ..#####..#.#########\n\
     ####################\n\
     #.####....###.#.#.##\n\
     ##.#################\n\
     #####.##.###..####..\n\
     ..######..##.#######\n\
     ####.##.####...##..#\n\
     .#####..#.######.###\n\
     ##...#.##########...\n\
     #.##########.#######\n\
     .####.#.###.###.#.##\n\
     ....##.##.###..#####\n\
     .#.#.###########.###\n\
     #.#.#.#####.####.###\n\
     ###.##.####.##.#..##" [@ocamlformat "disable=true"]
  in
  let tests = [
    (map1, (3, 4), 8);
    (* (map2, (5, 8), 33); *)
    (map3, (1, 2), 35);
    (* (map4, (6, 3), 41); *)
    (* (map5, (11, 13), 210) *)
  ]
  in
  let _ = (map1, map2, map3, map4, map5) in
  let test_map test_index (m, best_point, best_score) =
    let m = Asteroid_map.parse_map m in
    let (actual_point, actual_score) = best_asteroid m in
    if Point.compare actual_point best_point <> 0 || actual_score <> best_score then
      failwith (Printf.sprintf
                  "test failure in test %d:\
                   Actual point %s vs expected point: %s.\
                   Actual score %d vs expected score: %d"
                  test_index
                  (Point.show actual_point) (Point.show best_point) actual_score best_score)
    else
      Printf.printf "asteroid test succeeded"
  in
  List.iteri tests ~f:test_map


let _ = test ()

let solve () =
  let map = Aoc_utils.read_file "10/data.txt" |> String.concat ~sep:"\n" in
  let map = Asteroid_map.parse_map map in
  let _, best_observable = best_asteroid map in
  [Int.to_string best_observable]
