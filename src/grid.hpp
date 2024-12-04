#pragma once

#include <format>
#include <optional>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

enum class Direction { Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft };

template <>
struct std::formatter<Direction> : std::formatter<std::string> {
    auto format(Direction d, format_context& ctx) const {
        std::string name;
        switch (d) {
            case Direction::Up:
                name = "Up";
                break;
            case Direction::UpRight:
                name = "UpRight";
                break;
            case Direction::Right:
                name = "Right";
                break;
            case Direction::DownRight:
                name = "DownRight";
                break;
            case Direction::Down:
                name = "Down";
                break;
            case Direction::DownLeft:
                name = "DownLeft";
                break;
            case Direction::Left:
                name = "Left";
                break;
            case Direction::UpLeft:
                name = "UpLeft";
                break;
            default:
                throw std::runtime_error("Invalid direction");
                break;
        }

        return std::formatter<std::string>::format(name, ctx);
    }
};

const Direction Directions[] = {Direction::Up,   Direction::UpRight,  Direction::Right, Direction::DownRight,
                                Direction::Down, Direction::DownLeft, Direction::Left,  Direction::UpLeft};

template <typename T>
class Grid {
   private:
    std::vector<std::vector<T>> grid;

   public:
    Grid(std::vector<std::vector<T>> grid) : grid(grid) {
        for (const auto& row : grid) {
            if (row.size() != grid[0].size()) {
                throw std::runtime_error("All rows must have the same length");
            }
        }
    }
    Grid(const std::vector<std::string>& rows) {
        for (const auto& row : rows) {
            std::vector<T> row_vector;
            for (const auto& c : row) {
                row_vector.push_back(c);
            }
            grid.push_back(row_vector);
        }

        if (grid.size() > 1) {
            const auto size = grid[0].size();
            for (const auto& row : grid) {
                if (row.size() != size) {
                    throw std::runtime_error("All rows must have the same length");
                }
            }
        }
    }

    std::tuple<std::size_t, std::size_t> size() const { return {grid[0].size(), grid.size()}; }

    T& at(std::size_t x, std::size_t y) { return grid[y][x]; }
    const T& at(std::size_t x, std::size_t y) const { return grid[y][x]; }
    T& operator[](std::size_t x) { return grid[x]; }

    std::optional<std::vector<T>> neighbors(std::size_t x, std::size_t y, Direction d, std::size_t count) const {
        const auto& [width, height] = size();
        if (x >= width || y >= height) {
            return std::nullopt;
        }

        std::vector<T> result;
        for (std::size_t i = 0; i < count; i++) {
            switch (d) {
                case Direction::Up:
                    if (y >= i) {
                        result.push_back(at(x, y - i));
                    } else {
                        return std::nullopt;
                    }
                    break;
                case Direction::UpRight:
                    if (y >= i && x + i < grid[y].size()) {
                        result.push_back(at(x + i, y - i));
                    } else {
                        return std::nullopt;
                    }
                    break;
                case Direction::Right:
                    if (x + i < grid[y].size()) {
                        result.push_back(at(x + i, y));
                    } else {
                        return std::nullopt;
                    }
                    break;
                case Direction::DownRight:
                    if (y + i < grid.size() && x + i < grid[y].size()) {
                        result.push_back(at(x + i, y + i));
                    } else {
                        return std::nullopt;
                    }
                    break;
                case Direction::Down:
                    if (y + i < grid.size()) {
                        result.push_back(at(x, y + i));
                    } else {
                        return std::nullopt;
                    }
                    break;
                case Direction::DownLeft:
                    if (y + i < grid.size() && x >= i) {
                        result.push_back(at(x - i, y + i));
                    } else {
                        return std::nullopt;
                    }
                    break;
                case Direction::Left:
                    if (x >= i) {
                        result.push_back(at(x - i, y));
                    } else {
                        return std::nullopt;
                    }
                    break;
                case Direction::UpLeft:
                    if (y >= i && x >= i) {
                        result.push_back(at(x - i, y - i));
                    } else {
                        return std::nullopt;
                    }
                    break;
            }
        }

        return result;
    }
};