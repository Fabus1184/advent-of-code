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

struct Mul {
    std::uint64_t a;
    std::uint64_t b;
};
struct Do {};
struct Dont {};

template <>
struct Solution<3> {
    static std::vector<std::variant<Mul, Do, Dont>> parse_input(const std::string& input) {
        std::regex mul(R"#(mul\(([0-9]{1,3}),([0-9]{1,3})\))#");
        std::regex enable(R"#(do\(\))#");
        std::regex disable(R"#(don't\(\))#");

        std::vector<std::tuple<std::smatch, std::size_t>> matches;
        for (const auto& [index, regex] : std::vector{mul, enable, disable} | std::views::enumerate) {
            for (auto it = std::sregex_iterator(input.begin(), input.end(), regex); it != std::sregex_iterator{};
                 ++it) {
                matches.emplace_back(*it, index);
            }
        }

        std::sort(matches.begin(), matches.end(),
                  [&](const auto& a, const auto& b) { return std::get<0>(a).position() < std::get<0>(b).position(); });

        std::vector<std::variant<Mul, Do, Dont>> result;
        for (const auto& [smatch, index] : matches) {
            if (index == 0) {
                result.emplace_back(Mul{std::stoull(smatch[1]), std::stoull(smatch[2])});
            } else if (index == 1) {
                result.emplace_back(Do{});
            } else if (index == 2) {
                result.emplace_back(Dont{});
            } else {
                throw std::runtime_error("Invalid index");
            }
        }

        return result;
    }

    static auto part1(const std::string& input) {
        auto parsed_input = parse_input(input);

        return std::ranges::fold_left(parsed_input, 0, [](const auto& acc, const auto& x) -> std::uint64_t {
            if (const auto* mul = std::get_if<Mul>(&x)) {
                return acc + mul->a * mul->b;
            } else {
                return acc;
            }
        });
    }

    static auto part2(const std::string& input) {
        auto parsed_input = parse_input(input);

        bool enabled = true;
        return std::ranges::fold_left(parsed_input, 0, [&](std::uint64_t acc, const auto& token) -> std::uint64_t {
            return std::visit(
                [&](const auto& x) {
                    if constexpr (std::is_same_v<std::decay_t<decltype(x)>, Do>) {
                        enabled = true;
                    } else if constexpr (std::is_same_v<std::decay_t<decltype(x)>, Dont>) {
                        enabled = false;
                    } else if constexpr (std::is_same_v<std::decay_t<decltype(x)>, Mul>) {
                        if (enabled) {
                            acc += x.a * x.b;
                        }
                    } else {
                        static_assert(false, "non-exhaustive visitor");
                    }

                    return acc;
                },
                token);
        });
    }
};