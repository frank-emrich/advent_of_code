open! Core

module Vector3 = struct
  type t = int * int * int [@@deriving show]

  let get_x = fst3

  let get_y = snd3

  let get_z = trd3

  let neutral = (0, 0, 0)

  let abs_sum (x, y, z) = abs x + abs y + abs z

  let ( + ) (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

  let set_x ?(v = neutral) x = v + (x, 0, 0)

  let set_y ?(v = neutral) y = v + (0, y, 0)

  let set_z ?(v = neutral) z = v + (0, 0, z)
end

type moon = { position : Vector3.t; velocity : Vector3.t }

let still_moon position = { position; velocity = Vector3.neutral }

let kinetic_energy moon = Vector3.abs_sum moon.velocity

let potential_energy moon = Vector3.abs_sum moon.position

type system = moon list

let total_energy_of_system system =
  List.fold system
    ~f:(fun energy moon ->
      energy + (kinetic_energy moon * potential_energy moon))
    ~init:0

let simulate_step (state : system) =
  let apply_gravity_pair scrutinee other =
    let calculate_delta unpack pack =
      let v1 = unpack scrutinee.position in
      let v2 = unpack other.position in
      if v1 < v2 then pack 1 else if v1 = v2 then pack 0 else pack (-1)
    in
    let open Vector3 in
    let delta_vx = calculate_delta get_x set_x in
    let delta_vy = calculate_delta get_y set_y in
    let delta_vz = calculate_delta get_z set_z in
    let velocity =
      Vector3.(scrutinee.velocity + delta_vx + delta_vy + delta_vz)
    in
    { scrutinee with velocity }
  in

  let apply_gravity moon_index (moon : moon) =
    snd
    @@ List.fold state
         ~f:(fun (other_index, scrutinee) other_moon ->
           let next_index = other_index + 1 in
           if moon_index = other_index then (next_index, scrutinee)
           else (next_index, apply_gravity_pair scrutinee other_moon))
         ~init:(0, moon)
  in
  let apply_velocity moon =
    { moon with position = Vector3.(moon.position + moon.velocity) }
  in
  let apply moon_index = Fn.compose apply_velocity (apply_gravity moon_index) in
  List.mapi state ~f:apply

let simulate_steps initial_state step_count =
  let rec go state = function
    | 0 -> state
    | n -> go (simulate_step state) (n - 1)
  in
  go initial_state step_count

let input_system =
  [
    still_moon (13, 9, 5);
    still_moon (8, 14, -2);
    still_moon (-5, 4, 11);
    still_moon (2, -6, 1);
  ]

let test_system =
  [
    still_moon (-1, 0, 2);
    still_moon (2, -10, -7);
    still_moon (4, -8, 8);
    still_moon (3, 5, -1);
  ]

let solve () =
  let system = input_system in
  let final_state = simulate_steps system 1000 in
  let energy = total_energy_of_system final_state in
  [ Int.to_string energy ]
