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
    if Float.(diff < epsilon) then 0
    else if Float.(diff < Float.zero) then -1
    else 1
end
  include T
  include Comparator.Make (T)
end

(* module Float_approx_comparator = Comparator.Make(Float_approx) *)

module Asteroid_map = struct
  type points = Point.t array

  type lines =
    ( float,
      (float, Float_approx.comparator_witness) Set.t,
      Float_approx.comparator_witness )
    Map.t
  (** We express sets of lines by mappings of the form s -> I,
      where [s] is the slope of the line, and [I] is a set of y-intercepts,
      each of which represents a line with the given slope and y-intercept*)

  type point_to_lines = (Point.t, lines, Point.comparator_witness) Map.t

  type t = point_to_lines

  let parse_points_of_map map : Point.t array =
  let rows = String.split_lines map in
  (* let height = List.length rows in *)
  (* let width = String.length (List.hd_exn rows) in *)
  let parse_entry row_index (points, column_index) char =
    let asteroid_marker = '#' in
    if Char.equal char asteroid_marker then
      ((column_index, row_index) :: points, column_index + 1)
    else (points, column_index + 1)
  in
  let parse_row row_index row =
    fst (String.fold row ~f:(parse_entry row_index) ~init:([], 0))
  in
  let rows = List.mapi rows ~f:parse_row in
  List.concat rows |> Array.of_list

let parse_map map : t =
  let points = parse_points_of_map map in

  let empty_intercepts  = Set.empty (module Float_approx) in
  let empty_lines : lines = Map.empty (module Float_approx) in
  let empty = Map.empty (module Point) in


  let build_line p1 (lines : lines) p2 =
    if Point.compare p1 p2 = 0 then
      lines
    else
      let {slope; y_intercept} = line_of_points p1 p2 in
      let y_intercepts_for_slope = Option.value ~default:(empty_intercepts) (Map.find lines slope) in
      let y_intercepts_for_slope = Set.add y_intercepts_for_slope y_intercept in
      Map.set lines ~key:slope ~data:y_intercepts_for_slope
  in
  let build_lines (point_to_lines : t) p1 =
    (* let empty_lines = Set.empty (module Float_approx) in *)
    let lines : lines = Array.fold points ~f:(build_line p1) ~init:empty_lines in
    Map.add_exn point_to_lines ~key:p1 ~data:lines
  in
  let point_to_lines : t = Array.fold points ~f:build_lines ~init:empty in
  point_to_lines

end

let map1 = ".#..#.....#####....#...##"


let best_asteroid ast_map =
  let count_lines p1 lines = Set.fold ~init:0 ~f:(fun sum set -> sum + Set.length set) in
  let point_to_observable = Map.map ast_map ~f:count_lines in
  let choose_best ~key:point ~data:count ((max_point, max_count) as prev) =
    if count >= max_count then
      (point, count)
    else
      prev
  in
  let best_point, best_count = Map.fold point_to_observable ~f:choose_best ~init:((0, 0), -1) in
  best_count






let solve () = [ "123" ]
