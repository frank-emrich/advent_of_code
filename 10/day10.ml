open! Core

module Point = struct
  module T = struct
    type t = int * int [@@deriving show, compare, sexp, eq]

    (* type comparator_witness = Nothing.t *)
  end

  include T
  include Comparator.Make (T)
end

(* module Point_comparator = Comparator.Make(Point) *)

(* type line = { slope : float; y_intercept : float } *)

(* let line_of_points p1 p2 = *)
(*   let x1, y1 = p1 in *)
(*   let x2, y2 = p2 in *)
(*   let y_diff = Float.of_int (y1 - y2) in *)
(*   let x_diff = Float.of_int (x1 - x2) in *)
(*   let slope = Float.(y_diff / x_diff) in *)
(*   let y_intercept = Float.(Float.of_int y1 - (slope * Float.of_int x1)) in *)
(*   { slope; y_intercept } *)

let angle_between_points p_base p_other =
  let x1, y1 = p_base in
  let x2, y2 = p_other in
  let y_diff = Float.of_int (y1 - y2) in
  let x_diff = Float.of_int (x2 - x1) in
  let angle_with_x_axis = Float.atan2 y_diff x_diff in
  let angle_with_y_axis = Float.(angle_with_x_axis - (pi / 2.0)) in

  Float.(
    if angle_with_y_axis > 0.0 then angle_with_y_axis
    else (2.0 * pi) + angle_with_y_axis)

let distance_between_points p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  let y_diff = Float.of_int (y2 - y1) in
  let x_diff = Float.of_int (x2 - x1) in

  Float.(sqrt ((y_diff ** 2.0) + (x_diff ** 2.0)))

(* module Point_comparator = Comparator.Make (Point) *)
module Make_Float_approx (T : sig
  type t [@@deriving sexp]

  val key : t -> float
end) =
struct
  module T = struct
    type t = T.t [@@deriving sexp]

    let epsilon = 0.0001

    let compare v1 v2 =
      let k1 = T.key v1 in
      let k2 = T.key v2 in
      let diff = Float.(k1 - k2) in
      if Float.(k1 = k2 || abs diff < epsilon) then 0
      else if Float.(diff < Float.zero) then -1
      else 1
  end

  include T
  include Comparator.Make (T)
end

module Float_approx = Make_Float_approx (struct
  type t = float [@@deriving sexp]

  let key = Fn.id
end)

module Float_and_point_approx = Make_Float_approx (struct
  type t = float * Point.t [@@deriving sexp]

  let key = fst
end)

(* module Float_approx_comparator = Comparator.Make(Float_approx) *)

module Asteroid_map = struct
  type points = Point.t array

  type w1 = Float_approx.comparator_witness

  type w2 = Float_and_point_approx.comparator_witness

  type angle_to_distance = (float, (float * Point.t, w2) Set.t, w1) Map.t

  (** We express sets of lines by mappings of the form s -> I,
      where [s] is the slope of the line, and [I] is a set of y-intercepts,
      each of which represents a line with the given slope and y-intercept*)

  type point_to_lines =
    (Point.t, angle_to_distance, Point.comparator_witness) Map.t

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

    let empty_distances = Set.empty (module Float_and_point_approx) in
    let empty_lines : angle_to_distance = Map.empty (module Float_approx) in
    let empty = Map.empty (module Point) in

    let build_line p1 (lines : angle_to_distance) p2 =
      if Point.compare p1 p2 = 0 then lines
      else
        let angle = angle_between_points p1 p2 in
        let dist = distance_between_points p1 p2 in
        let existing_with_same_angle =
          Option.value ~default:empty_distances (Map.find lines angle)
        in

        let with_same_angle = Set.add existing_with_same_angle (dist, p2) in
        (* if fst p1 = 8 && snd p1 = 3 then *)
        (*   Printf.printf "Line from %s to %s has angle %f, distance %f\n" *)
        (*     (Point.show p1) (Point.show p2) *)
        (*     Float.(360.0 * angle / (2.0 * pi)) *)
        (*     dist; *)
        Map.set lines ~key:angle ~data:with_same_angle
      (* let new_lines = Set.add lines slope in *)

      (* new_lines *)
      (* let y_intercepts_for_slope = Option.value ~default:(empty_intercepts) (Map.find lines slope) in *)
      (* let y_intercepts_for_slope = Set.add y_intercepts_for_slope y_intercept in *)
      (* Map.set lines ~key:slope ~data:y_intercepts_for_slope *)
    in

    let build_lines (point_to_lines : t) p1 =
      (* let empty_lines = Set.empty (module Float_approx) in *)
      let lines : angle_to_distance =
        Array.fold points ~f:(build_line p1) ~init:empty_lines
      in
      Map.add_exn point_to_lines ~key:p1 ~data:lines
    in
    let point_to_lines : t = Array.fold points ~f:build_lines ~init:empty in
    point_to_lines
end

let best_observatory ast_map =
  (* let count_in_set init lines = Set.fold ~init ~f:(fun sum set -> sum + Set.length set) in *)
  (* let count_lines lines = Map.fold lines ~init:0 ~f:(fun ~key:_ ~data:set sum -> sum + Set.length set) in *)
  let point_to_observable = Map.map ast_map ~f:Map.length in
  let choose_best ~key:point ~data:count ((_, max_count) as prev) =
    if count >= max_count then (point, count) else prev
  in
  Map.fold point_to_observable ~f:choose_best ~init:((0, 0), -1)

let shoot_laser (map : Asteroid_map.t) (observatory_point : Point.t) =
  let rec turn (atd : Asteroid_map.angle_to_distance) =
    let open Sequence.Generator in
    if Map.is_empty atd then return ()
    else
      let mappings = Map.to_alist ~key_order:`Decreasing atd in
      let rec iterate atd = function
        | [] -> turn atd
        | (angle, distances) :: mappings ->
            let _, nearest_point = Set.min_elt_exn distances in
            let removed = Set.remove_index distances 0 in
            let atd =
              if Set.is_empty removed then Map.remove atd angle
              else Map.set atd ~key:angle ~data:removed
            in
            yield nearest_point >>= fun () -> iterate atd mappings
      in
      iterate atd mappings
  in
  turn (Map.find_exn map observatory_point) |> Sequence.Generator.run

let test_observation_position () =
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
  let map6 = Aoc_utils.read_whole_file "10/data.txt" in
  let tests =
    [
      (map1, (3, 4), 8);
      (map2, (5, 8), 33);
      (map3, (1, 2), 35);
      (map4, (6, 3), 41);
      (map5, (11, 13), 210);
      (map6, (26, 28), 267);
    ]
  in
  let _ = (map1, map2, map3, map4, map5) in
  let test_map test_index (m, best_point, best_score) =
    let m = Asteroid_map.parse_map m in
    let actual_point, actual_score = best_observatory m in
    if Point.compare actual_point best_point <> 0 || actual_score <> best_score
    then
      failwith
        (Printf.sprintf
           "test failure in test %d:Actual point %s vs expected point: \
            %s.Actual score %d vs expected score: %d"
           test_index (Point.show actual_point) (Point.show best_point)
           actual_score best_score)
  in
  List.iteri tests ~f:test_map

let test_laser () =
  let map1 =
    ".#....#####...#..\n\
     ##...##.#####..##\n\
     ##...#...#.#####.\n\
     ..#.....#...###..\n\
     ..#.#.....#....##"
  in
  (* let show_list show_elt l = *)
  (*   List.map l ~f:show_elt |> String.concat ~sep:", " *)
  (* in *)
  let exp1 =
    [
      (8, 1);
      (9, 0);
      (9, 1);
      (10, 0);
      (9, 2);
      (11, 1);
      (12, 1);
      (11, 2);
      (15, 1);
      (12, 2);
      (13, 2);
      (14, 2);
      (15, 2);
      (12, 3);
      (16, 4);
      (15, 4);
      (10, 4);
      (4, 4);
      (2, 4);
      (2, 3);
      (0, 2);
      (1, 2);
      (0, 1);
      (1, 1);
      (5, 2);
      (1, 0);
      (5, 1);
      (6, 1);
      (6, 0);
      (7, 0);
      (8, 0);
      (10, 1);
      (14, 0);
      (16, 1);
      (13, 3);
      (14, 3);
    ]
  in
  let m = Asteroid_map.parse_map map1 in
  let l =
    Sequence.to_list (shoot_laser m (8, 3))
    |> (Fn.flip List.take) (List.length exp1)
  in
  let check act exp =
    if not (Point.equal act exp) then
      failwith (Printf.sprintf "mismatch! expected: %s\n" (Point.show exp))
  in
  List.iter2_exn l exp1 ~f:check;

  let map2 = Aoc_utils.read_whole_file "10/data.txt" in
  let m2 = Asteroid_map.parse_map map2 in
  let l2 = shoot_laser m2 (26, 28) in
  if not (Point.equal (Sequence.nth_exn l2 199) (13, 09)) then
    failwith "mismatch"

let _ = test_observation_position ()

let _ = test_laser ()

let solve () =
  let map = Aoc_utils.read_file "10/data.txt" |> String.concat ~sep:"\n" in
  let map = Asteroid_map.parse_map map in
  let best_observatory, best_observable = best_observatory map in

  let vaporization_sequence = shoot_laser map best_observatory in
  let vaporized = Sequence.nth_exn vaporization_sequence (200 - 1) in
  let vaporized_res = (fst vaporized * 100) + snd vaporized in

  Printf.printf "%s\n" (Point.show best_observatory);
  [ Int.to_string best_observable; Int.to_string vaporized_res ]
