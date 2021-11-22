let rec listify_number num =
  if num < 10 then
    [num]
  else
    (num mod 10) :: listify_number (num / 10)

let check_number_naive second_part_cond num =
  (* let's be horribly inefficient :) *)
  let digits = listify_number num in
  let module IntSet = Core.Set.Make(Core.Int) in
  let module IntMap = Core.Map.Make(Core.Int) in
  let is_valid (prev, counts, pairs, is_non_ascending) c =
    let new_pairs =
      if prev = c then IntSet.add pairs c
      else pairs
    in
    let inc_opt = function
      | None -> 1
      | Some i -> i + 1
    in
    let new_counts = IntMap.update counts c ~f:inc_opt in
    (c, new_counts, new_pairs, is_non_ascending && prev >= c)
  in
  let (_, counts, pairs, non_ascending) =
    List.fold_left is_valid (10, IntMap.empty, IntSet.empty, true) digits
  in
  let pair_validator =
    if second_part_cond then (fun v -> IntMap.find_exn counts v = 2)
    else (fun _ -> true)
  in
  non_ascending && IntSet.exists pairs ~f:pair_validator

let count_valid_naive second_part_cond from_inc to_inc  =
  let seq =
    Core.Sequence.range
      ~start:`inclusive
      ~stop:`inclusive
      from_inc
      to_inc
  in
  let add_valid count num =
    if check_number_naive second_part_cond num then count + 1
    else count in
  Core.Sequence.fold
    seq
    ~f:add_valid
    ~init:0



let test () =

  let tests =
    [
      (10, 99, 9, 9);
      (111, 111, 1, 0);
      (22333, 22333, 1, 1);
      (222333, 222333, 1, 0);
      (357253, 892942, 530, 324)
    ]
  in

  let check i (from_inc, to_inc, expected_part_1, expected_part_2) =
    let actual_part_1 = count_valid_naive false from_inc to_inc in
    let actual_part_2 = count_valid_naive true from_inc to_inc in
    if actual_part_1 <> expected_part_1 || actual_part_2 <> expected_part_2 then
      failwith @@
        Printf.sprintf
          "Test %d failed, %d (actual_1) vs %d (expected_1) and %d (actual_2) vs %d (expected_2)\n"
          i
          actual_part_1
          expected_part_1
          actual_part_2
          expected_part_2
    else
      ()
  in
  List.iteri check tests


let solve () =

  [
    string_of_int @@ count_valid_naive false 357253 892942;
    string_of_int @@ count_valid_naive true 357253 892942
  ]
