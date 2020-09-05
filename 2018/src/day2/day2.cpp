#include "day2.h"

#include <iostream>
#include <unordered_map>

#include <aoc_utils.h>

using namespace std;

namespace AoC2018 {
namespace Day2 {

const string input_path = "../src/day2/input.txt";

size_t differ_only_at(string &s1, string &s2) {
  if (s1.size() != s2.size()) {
    return string::npos;
  }
  size_t diff_pos = string::npos;
  for (size_t i = 0; i < s1.size(); ++i) {
    if (s1[i] != s2[i]) {
      if (diff_pos == string::npos) {
        diff_pos = i;
      } else {
        return string::npos;
      }
    }
  }
  return diff_pos;
}

pair<bool, bool> has_double_or_triple(string &s) {
  unordered_map<char, unsigned> counts;
  for (char c : s) {
    ++counts[c];
  }
  bool has_pair = false;
  bool has_triple = false;
  for (auto entry : counts) {
    if (entry.second == 2) {
      has_pair = true;
    } else if (entry.second == 3) {
      has_triple = true;
    }
  }

  return make_pair(has_pair, has_triple);
}

string part_one() {
  vector<string> entries = lines_of_file(input_path);
  int pairs = 0, triples = 0;
  for (auto &entry : entries) {
    auto [has_pair, has_triple] = has_double_or_triple(entry);
    pairs += has_pair;
    triples += has_triple;
  }
  return to_string(pairs * triples);
}

string part_two() {
  vector<string> entries = lines_of_file(input_path);
  // bruteforce ftw!
  for (size_t i = 0; i < entries.size(); ++i) {
    for (size_t j = 0; j < i; ++j) {
      size_t diff_pos = differ_only_at(entries[i], entries[j]);
      if (diff_pos != string::npos) {
        entries[i].erase(diff_pos, 1);
        return entries[i];
      }
    }
  }
  return "<no answer>";
}

} // namespace Day2

tuple<string, string> solve_day2() {
  return make_tuple(Day2::part_one(), Day2::part_two());
}
} // namespace AoC2018
