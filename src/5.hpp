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
#include <set>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "aoc.hpp"
#include "grid.hpp"

struct Input {
    std::vector<std::tuple<std::uint32_t, std::uint32_t>> rules;
    std::vector<std::vector<std::uint32_t>> updates;
};

template <>
struct Solution<5> {
    static Input parse_input(const std::string& input) {
        const auto parts =
            std::ranges::to<std::vector<std::string>>(std::views::split(input, std::string_view("\n\n")));

        const auto rules = std::ranges::to<std::vector<std::tuple<std::uint32_t, std::uint32_t>>>(
            parts[0] | std::views::split('\n') | std::views::filter([](const auto& x) { return !x.empty(); }) |
            std::views::transform([](const auto& x) {
                const std::string str{x.begin(), x.end()};
                return std::tuple{std::stoul(str.substr(0, str.find('|'))), std::stoul(str.substr(str.find('|') + 1))};
            }));

        const auto updates = std::ranges::to<std::vector<std::vector<std::uint32_t>>>(
            parts[1] | std::views::split('\n') | std::views::filter([](const auto& x) { return !x.empty(); }) |
            std::views::transform([](const auto& x) {
                auto numbers = x | std::views::split(',') | std::views::transform([](const auto& x) {
                                   const std::string str{x.begin(), x.end()};
                                   return std::stoul(str);
                               });
                return std::ranges::to<std::vector<std::uint32_t>>(numbers);
            }));

        return {rules, updates};
    }

    static std::optional<std::size_t> violates_rule(
        const std::vector<std::uint32_t>& update, const std::vector<std::tuple<std::uint32_t, std::uint32_t>>& rules) {
        for (const auto& [i, rule] : rules | std::views::enumerate) {
            const auto& [a, b] = rule;

            const auto it_a = std::find(update.begin(), update.end(), a);
            const auto it_b = std::find(update.begin(), update.end(), b);

            if (it_a != update.end() && it_b != update.end() && std::distance(it_a, it_b) < 0) {
                return {i};
            }
        }

        return std::nullopt;
    }

    static auto part1(const std::string& input) {
        const auto& [rules, updates] = parse_input(input);

        auto wrong_ones =
            updates |
            std::views::filter([&](const auto& update) { return !violates_rule(update, rules).has_value(); }) |
            std::views::transform([](const auto& update) { return update[update.size() / 2]; });

        return std::ranges::fold_left(wrong_ones, 0u, std::plus{});
    }

    static auto part2(const std::string& input) {
        auto i = parse_input(input);
        auto& [rules, updates] = i;

        auto corrected =
            updates | std::views::filter([&](const auto& update) { return violates_rule(update, rules).has_value(); }) |
            std::views::transform([&](const auto& update) {
                auto copy = update;

                while (const auto index = violates_rule(copy, rules)) {
                    const auto& [a, b] = rules[*index];
                    const auto it_a = std::find(copy.begin(), copy.end(), a);
                    const auto it_b = std::find(copy.begin(), copy.end(), b);

                    std::iter_swap(it_a, it_b);
                }

                return copy[copy.size() / 2];
            });

        return std::ranges::fold_left(corrected, 0u, std::plus{});
    }
};