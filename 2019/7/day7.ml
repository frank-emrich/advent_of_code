open Core


module IntCodeMachine =
struct

  module IntMap = Core.Int.Map

  type state =
    {
      input: int list;
      output: int list;
      data: int IntMap.t;
      stop: bool;
      index: int
    }


  let make_op_fun (op_code : int) : (state -> (int * int) array -> state) =
    let inc_index i args = i + Array.length args + 1 in
    let mk_arith op =
      fun state args ->
       let result = op (fst args.(0)) (fst args.(1)) in
       let update_index = snd args.(2) in
       let new_data = IntMap.set state.data ~key:update_index ~data:result in
       {state with data=new_data; index=inc_index state.index args}
    in
    let mk_cond_jmp op =
      fun state args ->
       if op (fst args.(0)) then
         let alt_value = fst args.(1) in
         {state with index=alt_value}
       else
         {state with index=inc_index state.index args}
    in
    let mk_comp op =
      fun state args ->
      let result =
        if op (fst args.(0)) (fst args.(1)) then 1
        else 0
      in
      let new_data = IntMap.set state.data ~key:(snd args.(2)) ~data:result in
      {state with data=new_data; index=inc_index state.index args}
    in
    match op_code with
    | 1 -> mk_arith ( + )
    | 2 -> mk_arith ( * )
    | 3 ->
       fun state args ->
       let next_input = List.hd_exn state.input in
       let update_index = snd args.(0) in
       let new_data = IntMap.set state.data ~key:update_index ~data:next_input in
       {state with data=new_data;
                   input=List.tl_exn state.input;
                   index=inc_index state.index args}
    | 4 ->
       fun state args ->
       {state with output = (fst args.(0)) :: state.output;
                   index=inc_index state.index args}
    | 5 -> mk_cond_jmp ((<>) 0)
    | 6 -> mk_cond_jmp ((=) 0)
    | 7 -> mk_comp (<)
    | 8 -> mk_comp (=)
    | 99 ->
       fun state _args -> {state with stop = true}
    | bad -> failwith @@ Printf.sprintf "unexpected op_code: %d\n" bad



  let parse_op_spec op_spec =
    let param_count_of_op = function
    | 99 -> 0
    | 3
    | 4 -> 1
    | 5
    | 6 -> 2
    | 1
    | 2
    | 7
    | 8 -> 3
    | _ -> failwith "illegal opcode"
    in
    let rec parse_param_mode count code =
      if count = 0 then
        []
      else
        let rem = code mod 10 in
        let mode =
          match rem with
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
    let param_spec =  parse_param_mode param_count param_spec in
    (op_code, param_count, param_spec)



  let fetch_args data arg_index param_count param_specs  =
    let args = Array.create ~len:param_count (0,0) in
    let fetch index = function
      | `Immediate ->
         let v = IntMap.find_exn data (arg_index + index) in
           args.(index) <- (v, v)
      | `Parameter ->
         let indirect_index = IntMap.find_exn data (arg_index + index) in
         let indirect_result = IntMap.find_exn data indirect_index in
         (* Printf.printf "indirect index is %d\n" indirect_index; *)
         args.(index) <- (indirect_result, indirect_index);
    in
    List.iteri ~f:fetch param_specs;
    args




  let rec eval (state : state) : state =
    let (op_code, param_count, param_specs) =
      parse_op_spec (IntMap.find_exn state.data state.index)
    in
    let args = fetch_args state.data (state.index + 1) param_count param_specs in
    let op_fun = make_op_fun op_code in
    let new_state = op_fun state args in
    if new_state.stop then
      new_state
    else
      eval new_state


  let run input data =
    let mapify (data, index) num =
      IntMap.set data ~key:index ~data:num, index + 1
    in
    let initial_state =
      {
        stop=false;
        output=[];
        input;
        data=List.fold_left ~f:mapify ~init:(IntMap.empty, 0) data |> fst;
        index=0
      }
    in
    eval initial_state

end


(** Lazily generated permutations **)
module PermuteSeq :
sig
  val permutations : 'a list -> 'a list Base.Sequence.t
end
=
struct
  type 'a state =
    {
      insert_elt: 'a;
      insert_pos : int;
      max_insert_pos : int;
      inner_seq : 'a list Base.Sequence.t;
    }

  let rec next_permutation sub_list state =
    let module BS = Base.Sequence in
    let open BS.Generator in
    if BS.is_empty state.inner_seq then
      if state.insert_pos >= state.max_insert_pos - 1
      then
        return ()
      else
        next_permutation
          sub_list
          {state with
            insert_pos = state.insert_pos + 1;
            inner_seq = permutations sub_list}
    else
      let next_inner_perm, next_inner_seq =
        Option.value_exn (BS.next state.inner_seq)
      in
      let pre, post = List.split_n next_inner_perm state.insert_pos in
      let next_result = (pre @ (state.insert_elt :: post)) in
      yield next_result >>= fun () ->
      next_permutation sub_list {state with inner_seq = next_inner_seq}

    and permutations : 'a list -> 'a list Base.Sequence.t = function
    | [] -> Base.Sequence.singleton []
    | (x :: xs) as orig ->
       let inner_seq = permutations xs in
       let initial_state =
         {
           insert_elt = x; insert_pos = 0;
           max_insert_pos = List.length orig;
           inner_seq;
         }
       in
       Base.Sequence.Generator.run (next_permutation xs initial_state)


end


let run_on_phase_settings (program : int list) (phase_settings : int list) =
  let eval prev_output phase_setting =
    let result_state =
      IntCodeMachine.run
        [phase_setting; prev_output]
        program in
    List.hd_exn result_state.output
  in
  List.fold_left ~f:eval ~init:0 phase_settings



let solve () =
  let module BS = Base.Sequence in
  let first_line = Aoc_utils.read_file "7/data.txt" |> List.hd_exn in
  let numbers_str = Stdlib.String.split_on_char ',' first_line  in
  let program = List.map ~f:int_of_string numbers_str in

  let all_phase_settings = PermuteSeq.permutations [0;1;2;3;4] in
  let max_eval m phase_settings =
    let res = run_on_phase_settings program phase_settings in
    Option.value_map ~default:(Some res) ~f:(Fn.compose Option.some (Int.max res)) m
  in

  let part_1_result =
    Option.value_exn
      (BS.fold ~f:max_eval ~init:None all_phase_settings)
  in
 [string_of_int part_1_result]
