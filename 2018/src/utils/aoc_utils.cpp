#include "aoc_utils.h"

#include <fstream>


using namespace std;

namespace AoC2018 {

vector<string> lines_of_file(const string& path) {
  vector<string> result;
  std::ifstream input(path);
  std::string line;
  for (; getline(input, line);) {
    result.push_back(line);
  }
  return result;
}
}
