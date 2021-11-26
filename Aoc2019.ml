

module type AOC_SOLVER =
sig
  val solve : unit -> string list
end


let print_solutions =
  let modules : ((module AOC_SOLVER) * string) list=
    [
      ((module Day1), "day 1");
      ((module Day2), "day 2");
      ((module Day3), "day 3");
      ((module Day4), "day 4");
      ((module Day5), "day 5");
      ((module Day6), "day 6");
      ((module Day7), "day 7");
      ((module Day9), "day 9");
      ((module Day10), "day 10");
      ((module Day11), "day 11");
      ((module Day12), "day 12")
    ]

  in
  let print_solution_parts _day_z_based (solver_module, name) =
    let module Solver = (val solver_module : AOC_SOLVER) in
    let results = Solver.solve () in
    let print_result part_z_based part_result =
      Printf.printf
        "result part %d for %s is:\n%s\n"
        (part_z_based + 1)
        name
        part_result
    in
    List.iteri print_result results
  in
  List.iteri print_solution_parts modules
