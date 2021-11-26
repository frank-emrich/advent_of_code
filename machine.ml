open! Core
module IntMap = Core.Int.Map

module type State = sig
  type t

  val output : t -> Z.t list

  val relative_base : t -> int

  val pc : t -> int

  val is_finished : t -> bool

  val is_awaiting_input : t -> bool

  val with_pc : t -> int -> t

  val with_relative_base : t -> int -> t

  val has_input : t -> bool

  val pop_input_exn : t -> t * Z.t

  val push_output : t -> Z.t -> t

  val push_input : t -> Z.t -> t

  val read_memory : t -> int -> Z.t

  val write_memory : t -> int -> Z.t -> t

  val stop : t -> t

  val wait_input : t -> t

  val running : t -> t

  val create_initial : input:Z.t list -> initial_data:Z.t list -> t
end

module Common_state = struct
  type status = Running | Finished | Need_input

  type 'a state = {
    input : Z.t list;
    output : Z.t list;
    data : 'a;
    relative_base : int;
    status : status;
    index : int;
  }

  let pop_input_exn s =
    ({ s with input = List.tl_exn s.input }, List.hd_exn s.input)

  let output s = s.output

  let relative_base s = s.relative_base

  let pc s = s.index

  let is_finished s = Poly.(s.status = Finished)

  let is_awaiting_input s = Poly.(s.status = Need_input)

  let with_pc s pc = { s with index = pc }

  let with_relative_base s relative_base = { s with relative_base }

  let push_output s new_out = { s with output = new_out :: s.output }

  let stop s = { s with status = Finished }

  let wait_input s = { s with status = Need_input }

  let running s = { s with status = Running }

  let push_input s input_data = { s with input = input_data :: s.input }

  let has_input s = not (List.is_empty s.input)

  let create input data =
    { status = Running; output = []; input; data; relative_base = 0; index = 0 }
end

module Pure_state = struct
  include Common_state

  type t = Z.t IntMap.t state

  let read_memory state address =
    Option.value ~default:Z.zero (IntMap.find state.data address)

  let write_memory state address value =
    { state with data = IntMap.set state.data ~key:address ~data:value }

  let create_initial ~input ~initial_data =
    let mapify (data, index) num =
      (IntMap.set data ~key:index ~data:num, index + 1)
    in
    let data =
      List.fold_left ~f:mapify ~init:(IntMap.empty, 0) initial_data |> fst
    in
    create input data
end

module Mutable_state = struct
  include Common_state

  type t = (int, Z.t) Hashtbl.t state

  let read_memory state address =
    Option.value ~default:Z.zero (Hashtbl.find state.data address)

  let write_memory state address value =
    Hashtbl.set state.data ~key:address ~data:value;
    state

  let create_initial ~input ~initial_data =
    let data = Hashtbl.create (module Int) in
    List.iteri initial_data ~f:(fun i num -> Hashtbl.set data ~key:i ~data:num);
    create input data
end

module Make (State : State) = struct
  type nonrec state = State.t

  let make_op_fun (op_code : int) : state -> (Z.t * int) array -> state =
    let inc_index i args = i + Array.length args + 1 in
    let skip_args s args = State.with_pc s (inc_index (State.pc s) args) in
    let mk_arith op state args =
      let result = op (fst args.(0)) (fst args.(1)) in
      let update_index = snd args.(2) in
      let state = State.write_memory state update_index result in
      State.with_pc state (inc_index (State.pc state) args)
    in
    let mk_cond_jmp (op : Z.t -> bool) state args =
      if op (fst args.(0)) then
        let alt_value = fst args.(1) |> Z.to_int in
        State.with_pc state alt_value
      else State.with_pc state (inc_index (State.pc state) args)
    in
    let mk_comp op state args =
      let result = if op (fst args.(0)) (fst args.(1)) then Z.one else Z.zero in
      let state = State.write_memory state (snd args.(2)) result in
      State.with_pc state (inc_index (State.pc state) args)
    in
    let mk_adjust_relative_base state args =
      let rb = State.relative_base state in
      let state =
        State.with_relative_base state (rb + (fst args.(0) |> Z.to_int))
      in
      skip_args state args
    in
    match op_code with
    | 1 -> mk_arith Z.( + )
    | 2 -> mk_arith Z.( * )
    | 3 ->
        fun state args ->
          if State.has_input state then
            let state, next_input = State.pop_input_exn state in
            let update_index = snd args.(0) in
            let state = State.write_memory state update_index next_input in
            skip_args state args
          else State.wait_input state
    | 4 ->
        fun state args ->
          let state = State.push_output state (fst args.(0)) in
          skip_args state args
    | 5 -> mk_cond_jmp (Z.Compare.( <> ) Z.zero)
    | 6 -> mk_cond_jmp (Z.Compare.( = ) Z.zero)
    | 7 -> mk_comp Z.Compare.( < )
    | 8 -> mk_comp Z.Compare.( = )
    | 9 -> mk_adjust_relative_base
    | 99 -> fun state _args -> State.stop state
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

  let fetch_args state arg_index param_count param_specs =
    let args = Array.create ~len:param_count (Z.zero, 0) in
    let fetch index = function
      | `Immediate ->
          let v = State.read_memory state (arg_index + index) in
          args.(index) <- (v, v |> Z.to_int)
      | `Parameter ->
          let indirect_index =
            Z.to_int (State.read_memory state (arg_index + index))
          in
          let indirect_result = State.read_memory state indirect_index in
          (* Printf.printf "indirect index is %d\n" indirect_index; *)
          args.(index) <- (indirect_result, indirect_index)
      | `Relative ->
          let relative_index : int =
            Z.to_int (State.read_memory state (arg_index + index))
            + State.relative_base state
          in
          let relative_result = State.read_memory state relative_index in
          args.(index) <- (relative_result, relative_index)
    in

    List.iteri ~f:fetch param_specs;
    args

  let rec eval (state : state) : state =
    let state = State.running state in
    let op_code, param_count, param_specs =
      parse_op_spec (State.read_memory state (State.pc state) |> Z.to_int)
    in
    let args = fetch_args state (State.pc state + 1) param_count param_specs in
    let op_fun = make_op_fun op_code in
    let new_state = op_fun state args in
    (* Printf.printf "after eval, data has now size: %d\n%!" (IntMap.length state.data); *)
    if State.is_finished new_state || State.is_awaiting_input new_state then
      new_state
    else eval new_state

  let run input data =
    let initial_state =
      State.create_initial
        ~input:(List.map ~f:Z.of_int input)
        ~initial_data:data
    in
    eval initial_state

  let create_initial_state ?(input = []) path =
    let prog = Aoc_utils.read_file path |> List.hd_exn in
    let numbers_str = String.split_on_chars ~on:[ ',' ] prog in
    let initial_data = List.map ~f:Z.of_string numbers_str in
    State.create_initial ~input ~initial_data
end
