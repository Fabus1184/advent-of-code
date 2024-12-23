const std = @import("std");
const grid = @import("grid.zig");

const Element = enum {
    Robot,
    Box,
    Wall,
    Empty,
    BoxLeft,
    BoxRight,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .Robot => try writer.print("R", .{}),
            .Box => try writer.print("O", .{}),
            .Wall => try writer.print("#", .{}),
            .Empty => try writer.print(".", .{}),
            .BoxLeft => try writer.print("[", .{}),
            .BoxRight => try writer.print("]", .{}),
        }
    }
};

const Input = struct {
    grid: grid.Grid(Element),
    steps: []grid.Direction,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        self.grid.deinit();
        allocator.free(self.steps);
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator, double: bool) !Input {
    var parts = std.mem.splitSequence(u8, input, "\n\n");

    var g = grid.Grid(Element).init(allocator);

    var gridLines = std.mem.splitSequence(u8, parts.next() orelse return error.@"missing grid", "\n");
    while (gridLines.next()) |line| {
        var row = std.ArrayList(Element).init(allocator);
        defer row.deinit();

        for (line) |c| {
            const e = switch (c) {
                '@' => Element.Robot,
                'O' => Element.Box,
                '#' => Element.Wall,
                '.' => Element.Empty,
                else => {
                    std.debug.panic("invalid element: {c}\n", .{c});
                },
            };

            if (double) {
                if (e == Element.Robot) {
                    try row.append(Element.Robot);
                    try row.append(Element.Empty);
                } else if (e == Element.Box) {
                    try row.append(Element.BoxLeft);
                    try row.append(Element.BoxRight);
                } else {
                    try row.append(e);
                    try row.append(e);
                }
            } else {
                try row.append(e);
            }
        }

        try g.addRow(row.items);
    }

    var steps = std.mem.splitSequence(u8, parts.next() orelse return error.@"missing steps", "\n");
    var stepList = std.ArrayList(grid.Direction).init(allocator);

    while (steps.next()) |line| {
        for (line) |c| {
            try stepList.append(switch (c) {
                '^' => grid.Direction.Up,
                'v' => grid.Direction.Down,
                '<' => grid.Direction.Left,
                '>' => grid.Direction.Right,
                else => return error.@"invalid direction",
            });
        }
    }

    return Input{ .grid = g, .steps = try stepList.toOwnedSlice() };
}

fn moveElement(g: *grid.Grid(Element), position: @Vector(2, isize), direction: grid.Direction) bool {
    switch (g.get(position + direction.toVector()).?) {
        Element.Wall => return false,
        Element.Empty => {
            const a = g.at(position).?;
            const b = g.at(position + direction.toVector()).?;
            std.mem.swap(Element, a, b);
            return true;
        },
        Element.Box => {
            if (moveElement(g, position + direction.toVector(), direction)) {
                const a = g.at(position).?;
                const b = g.at(position + direction.toVector()).?;
                std.mem.swap(Element, a, b);
                return true;
            } else {
                return false;
            }
        },
        Element.Robot => std.debug.panic("robot can't be moved\n", .{}),
        else => std.debug.panic("invalid element\n", .{}),
    }
}

fn moveElement2(g: *grid.Grid(Element), position: @Vector(2, isize), direction: grid.Direction) bool {}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !usize {
    var parsed = try parseInput(input, allocator, false);
    defer parsed.deinit(allocator);

    // find robot
    var it = parsed.grid.elements();
    var position = while (it.next()) |e| {
        if (e.element == Element.Robot) {
            break e.position;
        }
    } else {
        return error.@"robot not found";
    };

    for (parsed.steps) |step| {
        if (moveElement(&parsed.grid, position, step)) {
            position += step.toVector();
        }
    }

    var sum: u64 = 0;
    var it2 = parsed.grid.elements();
    while (it2.next()) |e| {
        if (e.element == Element.Box) {
            sum += @intCast(e.position[1] * 100 + e.position[0]);
        }
    }

    return sum;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !usize {
    var parsed = try parseInput(input, allocator, true);
    defer parsed.deinit(allocator);

    // find robot
    var it = parsed.grid.elements();
    var position = while (it.next()) |e| {
        if (e.element == Element.Robot) {
            break e.position;
        }
    } else {
        return error.@"robot not found";
    };

    for (parsed.steps) |step| {
        parsed.grid.printCustom("{any}");
        std.debug.print("step: {any}\n", .{step});

        if (moveElement2(&parsed.grid, position, step)) {
            position += step.toVector();
        }
    }

    var sum: u64 = 0;
    var it2 = parsed.grid.elements();
    while (it2.next()) |e| {
        if (e.element == Element.BoxLeft) {
            sum += @intCast(e.position[1] * 100 + e.position[0]);
        }
    }

    return sum;
}

test "part1" {
    const input = "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########\n\n<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\n";
    try std.testing.expectEqual(10092, try part1(input, std.testing.allocator));
}

test "part2" {
    const input = "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########\n\n<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\n";
    try std.testing.expectEqual(9021, try part2(input, std.testing.allocator));
}
