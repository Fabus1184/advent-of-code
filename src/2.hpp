#pragma once

#include <algorithm>
#include <cstdint>
#include <format>
#include <iostream>
#include <iterator>
#include <numeric>
#include <ranges>
#include <string>
#include <unordered_map>
#include <vector>

#include "aoc.hpp"

template <>
struct Solution<2> {
    static std::vector<std::vector<std::int32_t>> parse_input(const std::string& input) {
        auto lines = input | std::ranges::views::split('\n') |
                     std::ranges::views::filter([](const auto& x) { return !x.empty(); }) |
                     std::ranges::views::transform([](const auto& line) {
                         auto numbers =
                             line | std::ranges::views::split(' ') |
                             std::ranges::views::filter([](const auto& x) { return !x.empty(); }) |
                             std::ranges::views::transform([](const auto& x) { return std::stoi(x.data()); });
                         return std::ranges::to<std::vector<std::int32_t>>(numbers);
                     });

        return std::ranges::to<std::vector<std::vector<std::int32_t>>>(lines);
    }

    static bool safe(const std::vector<std::int32_t>& levels) {
        auto diff = [](const auto& x) {
            auto diff = std::abs(x[0] - x[1]);
            return diff < 1 || diff > 3;
        };
        return (std::ranges::is_sorted(levels, std::less{}) || std::ranges::is_sorted(levels, std::greater{})) &&
               std::ranges::count_if(levels | std::ranges::views::slide(2), diff) == 0;
    }

    static auto part1(const std::string& input) {
        auto parsed_input = parse_input(std::move(input));

        return std::ranges::count_if(parsed_input, safe);
    }

    static auto part2(const std::string& input) {
        auto parsed_input = parse_input(std::move(input));

        auto any_safe = [](const auto& levels) {
            auto safe_without_i = [&](std::size_t i) {
                auto copy = levels;
                copy.erase(copy.begin() + i);

                return safe(copy);
            };

            return safe(levels) || std::ranges::count_if(std::views::iota(0u, levels.size()), safe_without_i) > 0;
        };

        return std::ranges::count_if(parsed_input, any_safe);
    }
};