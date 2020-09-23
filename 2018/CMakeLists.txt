cmake_minimum_required(VERSION 3.16)

set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -Wextra -std=c++17")

project(AoC2018 VERSION 1.0
                  LANGUAGES CXX)

add_executable(aoc_2018 src/aoc_2018.cpp)

add_library(utils STATIC src/utils/aoc_utils.cpp src/utils/aoc_utils.h )
target_include_directories(utils PUBLIC src/utils)


add_library(day1 STATIC src/day1/day1.cpp )
target_include_directories(day1 PUBLIC src/day1/)
target_link_libraries(aoc_2018 PUBLIC day1)


add_library(day2 STATIC src/day2/day2.cpp )
target_include_directories(day2 PUBLIC src/day2/)
target_link_libraries(aoc_2018 PUBLIC day2)
target_link_libraries(day2 PUBLIC utils)


add_library(day3 STATIC src/day3/day3.cpp )
target_include_directories(day3 PUBLIC src/day3/)
target_link_libraries(aoc_2018 PUBLIC day3)
target_link_libraries(day3 PUBLIC utils)

add_library(day4 STATIC src/day4/day4.cpp )
target_include_directories(day4 PUBLIC src/day4/)
target_link_libraries(aoc_2018 PUBLIC day4)
target_link_libraries(day4 PUBLIC utils)

add_library(day5 STATIC src/day5/day5.cpp)
target_include_directories(day5 PUBLIC src/day5/)
target_link_libraries(aoc_2018 PUBLIC day5)
target_link_libraries(day5 PUBLIC utils)
