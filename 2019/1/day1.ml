
let compute_fuel mass =
  let mass_float = float_of_int mass in
  let minus_two a = a -. 2.0 in
  mass_float /. 3.0 |> floor |> minus_two |> int_of_float



let rec compute_fuel_with_own_weight mass =
  let fuel_needed_no_own_weight = compute_fuel mass in
  if fuel_needed_no_own_weight < 0 then
    0
  else
    fuel_needed_no_own_weight
    + compute_fuel_with_own_weight fuel_needed_no_own_weight

let solve () =
  let data_str = Aoc_utils.read_file "1/data.txt" in
  let data_int = List.map int_of_string data_str in

  let result_part_one =
    List.fold_left
      (fun sum mass -> sum + compute_fuel mass)
      0
      data_int
  in
  let result_part_two =
    List.fold_left
      (fun sum mass -> sum + compute_fuel_with_own_weight mass)
      0
      data_int
  in
  [
    string_of_int result_part_one;
    string_of_int result_part_two
  ]
