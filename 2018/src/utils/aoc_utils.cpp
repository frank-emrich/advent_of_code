#include "aoc_utils.h"

#include <fstream>
#include <regex>


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

string trim(string const& s) {
    regex e("^\\s+|\\s+$");
    return regex_replace(s, e, "");
}

}
