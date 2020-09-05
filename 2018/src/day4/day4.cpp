#include "day4.h"

#include <algorithm>
#include <array>
#include <chrono>
#include <iostream>
#include <numeric>
#include <regex>
#include <unordered_map>

#include <aoc_utils.h>

using namespace std;

namespace AoC2018 {
namespace Day4 {

const string input_path = "../src/day4/input_sorted.txt";

const regex date_re("\\[(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\]");

const regex asleep_re("\\s*falls asleep\\s*");
const regex wakeup_re("\\s*wakes up\\s*");
const regex shift_start_re("\\s*Guard #(.+) begins shift\\s*");

const int date_spec_length = 18;

struct Time {
  short year, month, day, hours, minutes;
};

enum class EventType { start, wakeup, sleep };

class Event {
public:
  int guard_id;
  EventType type = EventType::sleep;
  Time time;

  Event() {}

  Event(int guard_id, EventType type, Time time)
      : guard_id{guard_id}, type{type}, time{time} {}
};

ostream &operator<<(ostream &os, EventType et) {
  switch (et) {
  case EventType::start:
    cout << "EventType::start";
    break;
  case EventType::sleep:
    cout << "EventType::sleep";
    break;
  case EventType::wakeup:
    cout << "EventType::wakeup";
    break;
  }
  return os;
}

ostream &operator<<(ostream &os, Event const &e) {
  cout << "([" << e.time.year << "-" << e.time.month << "-" << e.time.day << " "
       << e.time.hours << ":" << e.time.minutes << "], guard: " << e.guard_id
       << ", type: " << e.type << ")";
  return os;
}

Event parseEvent(int prev_active_guard, string const &input) {
  smatch m;

  if (regex_search(input.cbegin(), input.cbegin() + date_spec_length, m,
                   date_re)) {
    Time t = {static_cast<short>(stoi(m[1])), static_cast<short>(stoi(m[2])),
              static_cast<short>(stoi(m[3])), static_cast<short>(stoi(m[4])),
              static_cast<short>(stoi(m[5]))};
    int active_guard = prev_active_guard;
    EventType et;

    if (regex_search(input.cbegin() + date_spec_length, input.cend(), m,
                     shift_start_re)) {
      active_guard = stoi(m[1]);
      et = EventType::start;
    } else if (regex_search(input.cbegin() + date_spec_length, input.cend(), m,
                            wakeup_re)) {
      et = EventType::wakeup;
    } else {
      et = EventType::sleep;
    }

    return Event{active_guard, et, t};
  }
  return Event{};
}

void finalize_shift(unordered_map<int, array<int, 60>> &guard_sleep_times,
                    int guard_id, int sleep_start_time, EventType state,
                    array<bool, 60> asleep_minutes) {
  if (state == EventType::sleep) {
    cout << "Adding final " << (60 - sleep_start_time) << " minutes to guard "
         << guard_id << endl;
    for (int i = sleep_start_time; i < 60; ++i) {
      asleep_minutes[i] = true;
    }
  }
  array<int, 60> &prev_sleep_mins = guard_sleep_times[guard_id];
  for (int i = 0; i < 60; ++i)
    prev_sleep_mins[i] += asleep_minutes[i];

}

unordered_map<int, array<int, 60>> processEvents(vector<string> const &input) {
  unordered_map<int, array<int, 60>> guard_sleep_times;

  int prev_active_guard = -1;

  array<bool, 60> asleep_minutes;
  int prev_guard_sleep_start_time = 0;
  EventType prev_state = EventType::sleep;

  for (auto const &spec : input) {
    Event e = parseEvent(prev_active_guard, spec);
    cout << "Parsed event: " << e << endl;

    int minutes = e.time.minutes;

    switch (e.type) {
    case EventType::start:
      if (prev_active_guard != -1) {
        finalize_shift(guard_sleep_times, prev_active_guard,
                       prev_guard_sleep_start_time, prev_state, asleep_minutes);
      }
      asleep_minutes = array<bool, 60>{};
      prev_guard_sleep_start_time = 0;

      break;
    case EventType::sleep:
      if (e.time.hours == 0)
        prev_guard_sleep_start_time = minutes;
      else
        prev_guard_sleep_start_time = 0;

      break;
    case EventType::wakeup:
      if (e.time.hours == 0) {
        int duration = minutes - prev_guard_sleep_start_time;
        cout << "adding " << duration << " minutes for guard "
             << prev_active_guard << endl;
        for (int m = prev_guard_sleep_start_time; m < minutes; ++m) {
          asleep_minutes[m] = true;
        }
      }
      break;
    }
    prev_state = e.type;
    prev_active_guard = e.guard_id;
  }

  finalize_shift(guard_sleep_times, prev_active_guard,
                 prev_guard_sleep_start_time, prev_state, asleep_minutes);

  return guard_sleep_times;
}

int sum(array<int, 60> const &a) { return accumulate(a.cbegin(), a.cend(), 0); }

} // namespace Day4

tuple<string, string> solve_day4() {
  vector<string> specs = lines_of_file(Day4::input_path);

  unordered_map<int, array<int, 60>> guard_sleep_times =
      Day4::processEvents(specs);

  auto max_entry = max_element(
      guard_sleep_times.begin(), guard_sleep_times.end(), [](auto e1, auto e2) {
        return Day4::sum(e1.second) < Day4::sum(e2.second);
      });

  cout << "Maximum is guard " << max_entry->first << " with "
       << Day4::sum(max_entry->second) << " minutes." << endl;

  array<int, 60> &max_array = max_entry->second;
  int max_minute_val = 0;
  int max_minute = 0;

  for (int i = 0; i < 60; ++i) {
    if (max_array[i] > max_minute_val) {
      max_minute = i;
      max_minute_val = max_array[i];
    }
  }

  cout << "most frequently asleep at minute " << max_minute << ", with value "
       << max_minute_val << endl;

  int result = max_entry->first * max_minute;

  return make_tuple(to_string(result), "");
}
} // namespace AoC2018
