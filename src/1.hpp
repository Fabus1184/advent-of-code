#pragma once

#include <algorithm>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <numeric>
#include <ranges>
#include <string>
#include <unordered_map>
#include <vector>

namespace Day1 {

std::tuple<std::vector<std::uint32_t>, std::vector<std::uint32_t>> parse_input(const std::string&& input) {
    std::vector<std::uint32_t> lefts, rights;

    for (const auto& numbers : input | std::ranges::views::split('\n') | std::ranges::views::filter([](const auto& x) {
                                   return !x.empty();
                               }) | std::ranges::views::transform([](const auto& line) {
                                   return std::ranges::to<std::vector<std::string>>(
                                       line | std::ranges::views::split(' ') |
                                       std::ranges::views::filter([](const auto& x) { return !x.empty(); }));
                               })) {
        std::uint32_t a = std::stoul(numbers[0]);
        std::uint32_t b = std::stoul(numbers[1]);

        lefts.emplace_back(a);
        rights.emplace_back(b);
    }

    std::sort(lefts.begin(), lefts.end());
    std::sort(rights.begin(), rights.end());

    return {lefts, rights};
}

std::string problem1(const std::string&& input) {
    const auto [lefts, rights] = parse_input(std::move(input));

    auto sum = std::ranges::fold_left(
        std::ranges::zip_view(lefts, rights) | std::ranges::views::transform([](const auto& x) {
            return abs(static_cast<std::int32_t>(std::get<0>(x)) - static_cast<std::int32_t>(std::get<1>(x)));
        }),
        0, std::plus{});

    return std::to_string(sum);
}

std::string problem2(const std::string&& input) {
    const auto [lefts, rights] = parse_input(std::move(input));

    std::unordered_map<std::uint32_t, std::uint32_t> right_counts;

    std::for_each(rights.begin(), rights.end(), [&](const auto& x) { right_counts[x]++; });

    auto sum = std::ranges::fold_left(
        lefts | std::ranges::views::transform([&](const auto& left) { return left * right_counts[left]; }), 0,
        std::plus{});

    return std::to_string(sum);
}

};
