#include "day3.h"

#include <algorithm>
#include <array>
#include <iostream>
#include <regex>

#include <aoc_utils.h>

using namespace std;

namespace AoC2018 {
namespace Day3 {

constexpr int size = 1000;

const regex spec_regex("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)\\s*");

const string input_path = "../src/day3/input.txt";

// struct coordinate {
//   int x, y;
// };

class FabricCutout {
public:
  int id;
  // coordinate system starting at top left corner
  short x, y;

  short width, height;

  FabricCutout(int id, short x, short y, short width, short height)
      : id{id}, x{x}, y{y}, width{width}, height{height} {}

  bool contains(short x, short y) {
    return this->x <= x && x < this->x + this->width && this->y <= y &&
           y < this->y + this->height;
  }
};

FabricCutout parseCutoutSpec(const string &spec) {
  cout << "parsing " << spec << endl;
  smatch m;
  if (regex_match(spec, m, spec_regex)) {
    // cout << m[1] << m[2] << m[3] << m[4] << m[5] << endl;
    return FabricCutout(stoi(m[1]), stoi(m[2]), stoi(m[3]), stoi(m[4]),
                        stoi(m[5]));

  } else {
    cout << "Could not parse " << spec << endl;
    return FabricCutout(0, 0, 0, 0, 0);
  }
}

ostream &operator<<(ostream &out, const FabricCutout &cutout) {
  return out << "{ id: " << cutout.id << ", x: " << cutout.x
             << ", y: " << cutout.y << ", width: " << cutout.width
             << ", height: " << cutout.height << "}";
}

using overlap_map = vector<vector<vector<int>>>;

overlap_map make_overlap_map(vector<FabricCutout> &cutouts) {
  vector<vector<int>> inners(size);
  vector<vector<vector<int>>> overlap_map(size, inners);

  for (auto &cutout : cutouts) {
    for (int x = cutout.x; x < cutout.x + cutout.width; ++x)
      for (int y = cutout.y; y < cutout.y + cutout.height; ++y) {
        overlap_map.at(x).at(y).push_back(cutout.id);
      }
  }

  return overlap_map;
}

string part_one(overlap_map &overlap_map) {

  int overlap_count = 0;
  for (short x = 0; x < size; ++x)
    for (short y = 0; y < size; ++y) {
      overlap_count += overlap_map[x][y].size() > 1;
    }

  // // correct answer: 104439
  return to_string(overlap_count);
}

// correct answer: 701
string part_two(vector<FabricCutout> &cutouts, overlap_map &overlap_map) {
  for (auto &cutout : cutouts) {
    for (int x = cutout.x; x < cutout.x + cutout.width; ++x) {
      for (int y = cutout.y; y < cutout.y + cutout.height; ++y) {
        if (overlap_map.at(x).at(y).size() > 1)
          goto cnt;
      }
    }
    return to_string(cutout.id);
  cnt:;
  }
  return "-1";
}

} // namespace Day3

tuple<string, string> solve_day3() {
  vector<string> specs = lines_of_file(Day3::input_path);
  vector<Day3::FabricCutout> cutouts;

  transform(specs.cbegin(), specs.cend(), back_inserter(cutouts),
            Day3::parseCutoutSpec);

  Day3::overlap_map overlap_map = Day3::make_overlap_map(cutouts);

  return make_tuple(Day3::part_one(overlap_map),
                    Day3::part_two(cutouts, overlap_map));
  // return make_tuple("","");
}
} // namespace AoC2018
