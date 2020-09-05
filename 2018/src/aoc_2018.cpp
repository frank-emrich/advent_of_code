#include <iostream>
#include <vector>

#include "day1/day1.h"
#include "day2/day2.h"
#include "day3/day3.h"
#include "day4/day4.h"

using namespace std;

namespace AoC2018 {
void print_solutions() {
  auto results = {AoC2018::solve_day1(), AoC2018::solve_day2(), AoC2018::solve_day3(), AoC2018::solve_day4()};
  std::cout << "Results:" << std::endl;

  int day = 0;
  for (auto& res : results) {
    cout << "Day: " << ++day << ": " << get<0>(res) << ", " << get<1>(res) << endl;
}
}
} // namespace AoC2018

int main() {
  std::cout << "AoC 2018" << std::endl;
  AoC2018::print_solutions();
}
