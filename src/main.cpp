#include <algorithm>
#include <cstdint>
#include <format>
#include <iostream>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <variant>
#include <vector>

#include "1.hpp"
#include "aoc.hpp"

enum class Part {
    ONE = 1,
    TWO = 2,
};

struct RunDay {
    std::optional<Part> part;
    std::uint8_t day;
};

struct SubmitDay {
    Part part;
    std::uint8_t day;
};

struct Command {
    std::variant<RunDay, SubmitDay> command;

    // run <day> <part (optional)>
    // submit <day> <part>
    static Command parse(const std::vector<std::string>& args) {
        if (args.size() < 2) {
            throw std::runtime_error("Invalid number of arguments");
        }

        if (args[1] == "run") {
            if (args.size() < 3) {
                throw std::runtime_error("Invalid number of arguments");
            }

            std::uint8_t day = std::stoul(args[2]);

            if (args.size() == 4) {
                std::uint8_t part = std::stoul(args[3]);
                return Command{RunDay{Part(part), day}};
            } else {
                return Command{RunDay{std::nullopt, day}};
            }
        } else if (args[1] == "submit") {
            if (args.size() < 4) {
                throw std::runtime_error("Invalid number of arguments");
            }

            std::uint8_t day = std::stoul(args[2]);
            std::uint8_t part = std::stoul(args[3]);

            return Command{SubmitDay{Part(part), day}};
        } else {
            throw std::runtime_error("Invalid command");
        }
    }
};

const constexpr std::size_t YEAR = 2024;

using AocFunction = std::function<std::string(const std::string&&)>;

const std::unordered_map<std::uint8_t, std::tuple<AocFunction, AocFunction>> AOC_MAP{
    {1, {Day1::problem1, Day1::problem2}},
};

std::int32_t main(std::int32_t argc, char** argv) {
    const std::vector<std::string> args(&argv[0], &argv[argc]);

    const Command command = Command::parse(args);

    const char* token_ptr = std::getenv("TOKEN");
    if (!token_ptr) {
        throw std::runtime_error("TOKEN environment variable not set");
    }
    std::string token{token_ptr};

    std::visit(
        [token](const auto& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, RunDay>) {
                const RunDay& runDay = arg;

                const std::vector<std::tuple<Part, AocFunction>> functions = [&]() {
                    const auto [part1, part2] = AOC_MAP.at(runDay.day);
                    if (auto part = runDay.part) {
                        if (*part == Part::ONE) {
                            return std::vector{std::make_tuple(Part::ONE, part1)};
                        } else {
                            return std::vector{std::make_tuple(Part::TWO, part2)};
                        }
                    } else {
                        return std::vector{std::make_tuple(Part::ONE, part1), std::make_tuple(Part::TWO, part2)};
                    }
                }();

                const Aoc<YEAR> aoc(runDay.day, std::move(token));
                const std::string input = aoc.get_input();

                std::for_each(functions.begin(), functions.end(), [&](auto arg) {
                    const auto [part, function] = arg;

                    const std::string result = function(std::move(input));

                    std::cout << std::format("Day {}, Part {}: {}\n", runDay.day, part == Part::ONE ? 1 : 2, result);
                });

            } else if constexpr (std::is_same_v<T, SubmitDay>) {
                const SubmitDay& submitDay = arg;
                const Aoc<YEAR> aoc(submitDay.day, std::move(token));

                const std::string input = aoc.get_input();
            } else {
                static_assert(false, "non-exhaustive visitor");
            }
        },
        command.command);

    return 0;
}