cmake_minimum_required(VERSION 3.21)

project(aoc-2024)

add_executable(aoc-2024 src/main.cpp)

target_compile_features(aoc-2024 PRIVATE cxx_std_23)
target_compile_options(aoc-2024 PRIVATE -Wall -Wextra -Wpedantic -Werror -Wno-unused -Wno-unused-parameter)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(aoc-2024 PRIVATE -fsanitize=address -fno-omit-frame-pointer)
    target_link_options(aoc-2024 PRIVATE -fsanitize=address)
endif()

find_package(CURL REQUIRED)
target_link_libraries(aoc-2024 PRIVATE CURL::libcurl)

