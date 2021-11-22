let rec eval vals pos =
  let opcode = vals.(pos) in
  let fun_of_opcode = function
      | 1 -> ( + )
      | 2 -> ( * )
      | opc -> failwith (Printf.sprintf "Illegal opcode %d" opc)
  in
  match opcode with
    | 99 -> vals
    | op ->
       let (s1, s2, t) = vals.(pos + 1), vals.(pos + 2), vals.(pos + 3) in
       let op_fun = fun_of_opcode op in
       let result = op_fun vals.(s1)  vals.(s2) in
       vals.(t) <- result;
       eval vals (pos + 4)

let solve () =
  let first_line = Aoc_utils.read_file "2/data.txt" |> List.hd in
  let numbers_str = String.split_on_char ',' first_line |> Array.of_list in
  let initial_numbers = Array.map int_of_string numbers_str in


  let eval at_1 at_2 =
    let numbers = Array.copy initial_numbers in
    numbers.(1) <- at_1;
    numbers.(2) <- at_2;
    let final_numbers = eval numbers 0 in
    final_numbers.(0)

  in

  (* part 1 *)
  let part_1_result = eval 12 2 in

  (* part 2 *)
  let rec search_result expected_result at_1 at_2 =
    let result = eval at_1 at_2 in
    if result = expected_result then
      (at_1, at_2)
    else
       match at_1, at_2 with
         | 99, 99 ->
            raise (Invalid_argument "couldn't find value")
         | _, 99 ->
            search_result expected_result (at_1 + 1) 0
         | _, _ ->
            search_result expected_result at_1 (at_2 + 1)
    in
    let noun, verb = search_result 19690720 0 0 in
    let part_2_result = 100 * noun + verb in
  [
    string_of_int part_1_result;
    string_of_int part_2_result;
  ]
