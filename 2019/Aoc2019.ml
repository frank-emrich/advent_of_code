

module type AOC_SOLVER =
sig
  val solve : unit -> string list
end


let print_solutions =
  let modules : (module AOC_SOLVER) list=
    [
      (module Day1);
      (module Day2);
      (module Day3)
    ]

  in
  let print_solution_parts day_z_based solver_module =
    let module Solver = (val solver_module : AOC_SOLVER) in
    let results = Solver.solve () in
    let print_result part_z_based part_result =
      Printf.printf
        "result part %d for day %d is:\n%s\n"
        (part_z_based + 1)
        (day_z_based + 1)
        part_result
    in
    List.iteri print_result results
  in
  List.iteri print_solution_parts modules
