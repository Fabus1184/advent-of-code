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
struct Solution<1> {
    static std::tuple<std::vector<std::uint32_t>, std::vector<std::uint32_t>> parse_input(const std::string& input) {
        std::vector<std::uint32_t> lefts, rights;
        for (const auto& numbers :
             input | std::views::split('\n') | std::views::filter([](const auto& x) { return !x.empty(); }) |
                 std::views::transform([](const auto& line) {
                     return std::ranges::to<std::vector<std::string>>(
                         line | std::views::split(' ') | std::views::filter([](const auto& x) { return !x.empty(); }));
                 })) {
            lefts.emplace_back(std::stoul(numbers[0]));
            rights.emplace_back(std::stoul(numbers[1]));
        }

        std::sort(lefts.begin(), lefts.end());
        std::sort(rights.begin(), rights.end());

        return {lefts, rights};
    }

    static auto part1(const std::string& input) {
        const auto [lefts, rights] = parse_input(std::move(input));

        return std::ranges::fold_left(
            std::ranges::zip_view(lefts, rights) | std::views::transform([](const auto& x) {
                return abs(static_cast<std::int32_t>(std::get<0>(x)) - static_cast<std::int32_t>(std::get<1>(x)));
            }),
            0, std::plus{});
    }

    static auto part2(const std::string& input) {
        const auto [lefts, rights] = parse_input(std::move(input));

        auto right_counts = std::ranges::fold_left(rights, std::unordered_map<std::uint32_t, std::uint32_t>{},
                                                   [](auto acc, const auto& x) {
                                                       acc[x]++;
                                                       return acc;
                                                   });

        return std::ranges::fold_left(
            lefts | std::views::transform([&](const auto& left) { return left * right_counts[left]; }), 0, std::plus{});
    }
};
