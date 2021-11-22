(* open! Core *)

module IntMap = Core.Int.Map

type state = {
  input : int list;
  output : int list;
  data : int IntMap.t;
  relative_base : int;
  stop : bool;
  index : int;
}

let make_op_fun (op_code : int) : state -> (int * int) array -> state =
  let inc_index i args = i + Array.length args + 1 in
  let mk_arith op state args =
    let result = op (fst args.(0)) (fst args.(1)) in
    let update_index = snd args.(2) in
    let new_data = IntMap.set state.data ~key:update_index ~data:result in
    { state with data = new_data; index = inc_index state.index args }
  in
  let mk_cond_jmp op state args =
    if op (fst args.(0)) then
      let alt_value = fst args.(1) in
      { state with index = alt_value }
    else { state with index = inc_index state.index args }
  in
  let mk_comp op state args =
    let result = if op (fst args.(0)) (fst args.(1)) then 1 else 0 in
    let new_data = IntMap.set state.data ~key:(snd args.(2)) ~data:result in
    { state with data = new_data; index = inc_index state.index args }
  in
  let mk_adjust_relative_base state args =
    { state with relative_base = state.relative_base + fst args.(0) }
  in
  match op_code with
  | 1 -> mk_arith ( + )
  | 2 -> mk_arith ( * )
  | 3 ->
      fun state args ->
        let next_input = List.hd state.input in
        let update_index = snd args.(0) in
        let new_data =
          IntMap.set state.data ~key:update_index ~data:next_input
        in
        {
          state with
          data = new_data;
          input = List.tl state.input;
          index = inc_index state.index args;
        }
  | 4 ->
      fun state args ->
        {
          state with
          output = fst args.(0) :: state.output;
          index = inc_index state.index args;
        }
  | 5 -> mk_cond_jmp (( <> ) 0)
  | 6 -> mk_cond_jmp (( = ) 0)
  | 7 -> mk_comp ( < )
  | 8 -> mk_comp ( = )
  | 9 -> mk_adjust_relative_base
  | 99 -> fun state _args -> { state with stop = true }
  | bad -> failwith @@ Printf.sprintf "unexpected op_code: %d\n" bad

let parse_op_spec op_spec =
  let param_count_of_op = function
    | 99 -> 0
    | 3 | 4 -> 1
    | 5 | 6 -> 2
    | 1 | 2 | 7 | 8 -> 3
    | 9 -> 1
    | _ -> failwith "illegal opcode"
  in
  let rec parse_param_mode count code =
    if count = 0 then []
    else
      let rem = code mod 10 in
      let mode =
        match rem with
        | 2 -> `Relative
        | 1 -> `Immediate
        | 0 -> `Parameter
        | _ -> failwith "unexpected parameter mode"
      in
      let next = code / 10 in
      mode :: parse_param_mode (count - 1) next
  in
  let op_code = op_spec mod 100 in
  let param_spec = op_spec / 100 in
  let param_count = param_count_of_op op_code in
  let param_spec = parse_param_mode param_count param_spec in
  (op_code, param_count, param_spec)

let read_memory state address =
  Option.value ~default:0 (IntMap.find state.data address)

let fetch_args state arg_index param_count param_specs =
  let args = Array.make param_count (0, 0) in
  let fetch index = function
    | `Immediate ->
        let v = read_memory state (arg_index + index) in
        args.(index) <- (v, v)
    | `Parameter ->
        let indirect_index = read_memory state (arg_index + index) in
        let indirect_result = read_memory state indirect_index in
        (* Printf.printf "indirect index is %d\n" indirect_index; *)
        args.(index) <- (indirect_result, indirect_index)
    | `Relative ->
        let relative_index =
          read_memory state (arg_index + index) + state.relative_base
        in
        let relative_result = read_memory state relative_index in
        args.(index) <- (relative_result, relative_index)
  in

  List.iteri fetch param_specs;
  args

let rec eval (state : state) : state =
  let op_code, param_count, param_specs =
    parse_op_spec (IntMap.find_exn state.data state.index)
  in
  let args = fetch_args state (state.index + 1) param_count param_specs in
  let op_fun = make_op_fun op_code in
  let new_state = op_fun state args in
  if new_state.stop then new_state else eval new_state

let run input data =
  let mapify (data, index) num =
    (IntMap.set data ~key:index ~data:num, index + 1)
  in
  let initial_state =
    {
      stop = false;
      output = [];
      input;
      data = List.fold_left mapify (IntMap.empty, 0) data |> fst;
      relative_base = 0;
      index = 0;
    }
  in
  eval initial_state

let test () =
  let part_two_larger_prog =
    [3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;
     1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;
     999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99] in
  let tests =
    [
       (*             0    1     2    3    4    5    6   *)
      ([],        [1001;   0; -902;  5;  104;   0;  99],   99);
      ([99],       [ 3;    4;   4;   4; -100],             99);
      ([],      [  102;  -10;   1;   5;  104;  -1;  99 ],  100);
      ([],      [  102;    2;   1;   5;    4;  -1;  99 ],  4);

      ([0], [3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9], 0);
      ([123], [3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9], 1);
      ([0], [3;3;1105;-1;9;1101;0;0;12;4;12;99;1], 0);
      ([456], [3;3;1105;-1;9;1101;0;0;12;4;12;99;1], 1);

      ([7], part_two_larger_prog, 999);
      ([8], part_two_larger_prog, 1000);
      ([9], part_two_larger_prog, 1001);
      ([], [1102; 34915192; 34915192; 7; 4; 7; 99; 0], 1219070632396864)
    ]
  in
  let check i (input, data, expected) =
    let res_state = run input data in
    let res = res_state.output |> List.hd in
    (* Printf.printf "result %d: %d, expected: %d\n" i res expected; *)
    if res = expected then Printf.printf "Test %d succeeded\n" i
    else
      Printf.printf "error in test %d, actual: %d vs expected %d\n" i res
        expected
  in
  List.iteri check tests
    [@@ocamlformat "disable=true"]

let _ = test ()

let solve () =
  (* test (); *)
  let first_line = Aoc_utils.read_file "5/data.txt" |> List.hd in
  let numbers_str = String.split_on_char ',' first_line in
  let initial_numbers = List.map int_of_string numbers_str in

  let part_1_final_state = run [ 1 ] initial_numbers in
  let part_1_result = part_1_final_state.output |> List.hd in

  let part_2_final_state = run [ 5 ] initial_numbers in
  let part_2_result = part_2_final_state.output |> List.hd in

  [ string_of_int part_1_result; string_of_int part_2_result ]
