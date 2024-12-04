#pragma once

#include <algorithm>
#include <cstdint>
#include <format>
#include <iostream>
#include <iterator>
#include <numeric>
#include <print>
#include <ranges>
#include <regex>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "aoc.hpp"
#include "grid.hpp"

template <>
struct Solution<4> {
    static Grid<char> parse_input(const std::string& input) {
        const std::vector<std::string> rows = std::ranges::to<std::vector<std::string>>(
            input | std::views::split('\n') | std::views::filter([](const auto& x) { return !x.empty(); }));

        return {rows};
    }

    static auto part1(const std::string& input) {
        const auto grid = parse_input(input);
        const auto& [width, height] = grid.size();

        auto sequences = Directions | std::views::transform([&](const auto& x) {
                             return std::views::cartesian_product(std::views::iota(0u, width),
                                                                  std::views::iota(0u, height), std::views::single(x));
                         }) |
                         std::views::join | std::views::transform([&](const auto& t) {
                             const auto& [x, y, direction] = t;
                             return grid.neighbors(x, y, direction, 4);
                         }) |
                         std::views::filter([](const auto& x) { return x.has_value(); }) |
                         std::views::transform([](const auto& x) { return std::string{x->begin(), x->end()}; });

        return std::ranges::count_if(sequences, [](const auto& x) { return x == "XMAS"; });
    }

    static auto part2(const std::string& input) {
        const auto grid = parse_input(input);
        const auto& [width, height] = grid.size();

        auto sequences = std::views::cartesian_product(std::views::iota(0u, width), std::views::iota(0u, height)) |
                         std::views::transform([&](const auto& t) {
                             const auto& [x, y] = t;
                             const auto right_down = grid.neighbors(x, y, Direction::DownRight, 3);
                             const auto left_down = grid.neighbors(x + 2, y, Direction::DownLeft, 3);
                             return std::tuple{right_down, left_down};
                         }) |
                         std::views::filter([](const auto& x) {
                             const auto& [right_down, left_down] = x;
                             return right_down.has_value() && left_down.has_value();
                         }) |
                         std::views::transform([](const auto& x) {
                             auto& [right_down, left_down] = x;
                             return std::tuple{std::string{right_down->begin(), right_down->end()},
                                               std::string{left_down->begin(), left_down->end()}};
                         });

        return std::ranges::count_if(sequences, [](const auto& x) {
            const auto& [right_down, left_down] = x;
            return (right_down == "MAS" || right_down == "SAM") && (left_down == "MAS" || left_down == "SAM");
        });
    }
};