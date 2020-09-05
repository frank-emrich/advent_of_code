#include "day1.h"

#include <fstream>
#include <iostream>
#include <unordered_set>
#include <vector>



using namespace std;

namespace AoC2018 {
namespace Day1 {

const string filename = "../src/day1/input.txt";

vector<int> get_entries() {
  vector<int> result;
  std::ifstream input(Day1::filename);
  std::string line;
  for (; getline(input, line);) {
    result.push_back(stoi(line));
  }
  return result;
}

string part_one() {
  int result = 0;
  for (int num : get_entries()) {
    result += num;
  }
  return to_string(result);
}

string part_two() {
  vector<int> entries = get_entries();
  unordered_set<int> frequencies;
  int cur_frequency = 0;
  int index = 0;
  while (frequencies.count(cur_frequency) == 0) {
    frequencies.insert(cur_frequency);
    cur_frequency += entries[index];
    index = (index + 1) % entries.size();
  }
  return to_string(cur_frequency);
}

} // namespace Day1

tuple<string, string> solve_day1() {
  return make_tuple(Day1::part_one(), Day1::part_two());
}

} // namespace AoC2018
