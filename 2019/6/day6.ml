module SMap = Core.String.Map


(** A tree with parent pointers.
    First component maps orbitee (node being orbited) to orbiters (nodes
    orbiting the orbitee). Second map is from orbiter to unique orbitee.  **)
type bi_tree = string list SMap.t * string SMap.t

let parse_orbit_spec orb_spec : (string * string)=
  let paranthesis_pos = String.index orb_spec ')'  in
  let len = String.length orb_spec in
  let orbitee = String.sub orb_spec 0 paranthesis_pos in
  let orbiter =
    String.sub orb_spec
      (paranthesis_pos + 1)
      (len - paranthesis_pos - 1)
  in
  (orbitee, orbiter)


let add_to_graph (graph : bi_tree) (orbitee, orbiter) =
  let fwd_map, backwd_map = graph in
  let add_orbiter l_opt =
    Option.value ~default:[] l_opt |> List.cons orbiter
  in
  let new_fwd_map =
    SMap.update fwd_map orbitee ~f:add_orbiter in
  let new_backwd_map =
    SMap.set backwd_map ~key:orbiter ~data:orbitee in
  (new_fwd_map, new_backwd_map)


let build_graph orbit_info : bi_tree =
  List.fold_left add_to_graph (SMap.empty, SMap.empty) orbit_info


let rec count_orbits (graph : bi_tree) depth orbitee =
  let fwd_map, _ = graph in
  let orbiters =
    SMap.find fwd_map orbitee |> Option.value ~default:[]
  in
  let orbiter_counts =
    List.fold_left
      (fun acc node -> acc + count_orbits graph (depth + 1) node)
      0
      orbiters
  in
  depth + orbiter_counts


let rec dist_between (graph : bi_tree) start destination : int option =
  if start = destination then
    Some 0
  else
    let fwd_map, _ = graph in
    let orbiters = SMap.find fwd_map start in
    let open Core in
    let find_dest (other_start : string) : int option =
      (dist_between graph other_start destination : int option)
      |> Option.map ~f:((+) 1)
    in
    Option.map orbiters ~f:(List.find_map ~f:find_dest)
    |> Option.join

(* (Inefficient) search for distance between node A and B in tree:
   If there is no path from A to B, return 1 + dist(C to B), where C is
   the predecessor (i.e., orbitee) of A *)
let rec search_backwards (graph : bi_tree) start destination =
  match dist_between graph start destination with
  | None ->
     let _, backwd_map = graph in
     begin
       match SMap.find backwd_map start with
       | None ->
          failwith @@ Printf.sprintf "Could not find %s" destination
       | Some orbitee ->
          1 + search_backwards graph orbitee destination
     end
  | Some dist -> dist



let solve () =
  let input = Aoc_utils.read_file "6/data.txt" in
  let orbit_info = List.map parse_orbit_spec input in
  let graph = build_graph orbit_info in
  let root = "COM" in
  let part_1_result = count_orbits graph 0 root in

  (* search_backwards returns the distance in edges,
     and the desired number of "orbital transfers" is edges - 2 *)
  let part_2_result = search_backwards graph "YOU" "SAN" - 2 in

  [
    string_of_int part_1_result;
    string_of_int part_2_result
  ]
