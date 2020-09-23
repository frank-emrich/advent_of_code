#include "day5.h"

#include <algorithm>
#include <cstdio>
#include <iostream>
#include <regex>
#include <string>
#include <unordered_set>

#include <aoc_utils.h>

using namespace std;

namespace AoC2018 {

namespace Day5 {

const string input_path = "../src/day5/input.txt";

bool match(char c1, char c2) {
  // cout << "comparing " << c1 << " and " << c2 << endl;
  bool uc1 = isupper(c1);
  bool uc2 = isupper(c2);

  return toupper(c1) == toupper(c2) && (uc1 != uc2);
}

pair<string, string> react(string const &input) {

  // cout << "Input is " << input << endl;

  string state = input;

  size_t i = 1;
  while (i < state.size()) {
    // cout << "State is " << state << endl;
    if (match(state[i - 1], state[i])) {
      // cout << "Deleting " << state[i - 1] << " at " << i << endl;
      state.erase(state.begin() + i - 1, state.begin() + i + 1);
      if (i > 1)
        --i;
    } else {
      ++i;
    }
  }

  // cout << "State is " << state << endl;
  return make_pair(to_string(state.size()), state);
}

pair<string, string> part_one(string const &input) { return react(input); }

string part_two(string const &input) {
  unordered_set<char> units;

  for (char c : input) {
    units.insert(toupper(c));
  }
  // cout << "units size: " << units.size() << endl;

  int min_length = input.size();
  for (const char unit : units) {
    string data = input;

    // cout << "\nbefore removal length \n" << data.size() << endl;

    auto newend = std::remove_if(data.begin(), data.end(), [unit](char c) {
      return c == unit || toupper(c) == unit;
    });
    data.erase(newend, data.end());

    int length = react(data).second.size();
    min_length = min(length, min_length);

    // cout << "\nafter removal length \n" << data.size() << endl;
  }
  return to_string(min_length);
}

} // namespace Day5

tuple<string, string> solve_day5() {
  string input = trim(lines_of_file(Day5::input_path).at(0));

  auto res1 = Day5::part_one(input);
  // cout << endl << "finished first iteration: " << res1.second << endl;
  auto res2 = Day5::part_two(input);

  return make_tuple(res1.first, res2);
}
} // namespace AoC2018
