#include <algorithm>
#include <cstdint>
#include <format>
#include <iostream>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

#include "1.hpp"
#include "aoc.hpp"

struct Part1 {};
struct Part2 {};

struct RunDay {
    std::variant<Part1, Part2> part;
    std::uint8_t day;
};

struct SubmitDay {
    std::variant<Part1, Part2> part;
    std::uint8_t day;
};

struct Command {
    std::variant<RunDay, SubmitDay> command;

    // run <day> <part>
    // submit <day> <part>
    static Command parse(const std::vector<std::string>& args) {
        if (args.size() < 3) {
            throw std::runtime_error("Invalid number of arguments");
        }

        uint8_t day = std::stoul(args[2]);
        if (day < 1 || day > 25) {
            throw std::runtime_error(std::format("Invalid day: {}", day));
        }

        auto part = [&]() -> std::variant<Part1, Part2> {
            if (args.size() < 4) {
                throw std::runtime_error("Invalid number of arguments");
            }

            if (args[3] == "1") {
                return Part1{};
            } else if (args[3] == "2") {
                return Part2{};
            } else {
                throw std::runtime_error(std::format("Invalid part: {}", args[3]));
            }
        }();

        if (args[1] == "run") {
            return Command{RunDay{part, day}};
        } else if (args[1] == "submit") {
            return Command{SubmitDay{part, day}};
        } else {
            throw std::runtime_error(std::format("Invalid command: {}", args[1]));
        }
    }

    uint8_t day() const {
        return std::visit(
            [](const auto& arg) -> uint8_t {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, RunDay>) {
                    return arg.day;
                } else if constexpr (std::is_same_v<T, SubmitDay>) {
                    return arg.day;
                } else {
                    static_assert(false, "non-exhaustive visitor");
                }
            },
            command);
    }

    std::variant<Part1, Part2> part() const {
        return std::visit(
            [](const auto& arg) -> std::variant<Part1, Part2> {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, RunDay>) {
                    return arg.part;
                } else if constexpr (std::is_same_v<T, SubmitDay>) {
                    return arg.part;
                } else {
                    static_assert(false, "non-exhaustive visitor");
                }
            },
            command);
    }

    size_t part_number() const {
        return std::visit(
            [](const auto& arg) -> size_t {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, Part1>) {
                    return 1;
                } else if constexpr (std::is_same_v<T, Part2>) {
                    return 2;
                } else {
                    static_assert(false, "non-exhaustive visitor");
                }
            },
            part());
    }
};

template <auto Start, auto End, class F>
constexpr void constexpr_for(F&& f) {
    if constexpr (Start < End) {
        f(std::integral_constant<decltype(Start), Start>());
        constexpr_for<Start + 1, End>(f);
    }
}

template <class T, std::size_t = sizeof(T)>
std::true_type is_complete_impl(T*);
std::false_type is_complete_impl(...);
template <class T>
using is_complete_type = decltype(is_complete_impl(std::declval<T*>()));

const auto SOLUTIONS = []() {
    std::unordered_map<std::uint8_t, std::tuple<AocFunction, AocFunction>> solutions{};

    constexpr_for<1, 25>([&](auto day) {
        constexpr std::uint8_t Day = decltype(day)::value;

        if constexpr (is_complete_type<Solution<Day>>::value) {
            solutions[Day] = std::make_tuple(Solution<Day>::part1, Solution<Day>::part2);
        }
    });

    return solutions;
}();

std::int32_t main(std::int32_t argc, char** argv) {
    const std::vector<std::string> args(&argv[0], &argv[argc]);

    const Command command = Command::parse(args);

    const char* token_ptr = std::getenv("TOKEN");
    if (!token_ptr) {
        throw std::runtime_error("TOKEN environment variable not set");
    }
    std::string token{token_ptr};

    const constexpr std::size_t YEAR = 2024;
    const Aoc<YEAR> aoc(std::move(token));

    std::visit(
        [&](const auto& arg) {
            const auto& parts = SOLUTIONS.at(command.day());

            const std::string input = aoc.get_input(command.day());

            const auto func = std::visit(
                [&](const auto& part) -> AocFunction {
                    using T = std::decay_t<decltype(part)>;
                    if constexpr (std::is_same_v<T, Part1>) {
                        return std::get<0>(parts);
                    } else if constexpr (std::is_same_v<T, Part2>) {
                        return std::get<1>(parts);
                    } else {
                        static_assert(false, "non-exhaustive visitor");
                    }
                },
                command.part());

            const std::string result = func(std::move(input));

            std::cout << std::format("Day {} Part {}: {}", command.day(), command.part_number(), result) << std::endl;

            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, RunDay>) {
                // do nothing
            } else if constexpr (std::is_same_v<T, SubmitDay>) {
                const auto response = aoc.submit(command.day(), command.part_number(), result);
                // std::cout << response << std::endl;
            } else {
                static_assert(false, "non-exhaustive visitor");
            }
        },
        command.command);

    return 0;
}