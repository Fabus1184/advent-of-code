const std = @import("std");
const grid = @import("grid.zig");

const Element = enum {
    Robot,
    Box,
    Wall,
    Empty,
};

const Input = struct {
    grid: grid.Grid(Element),
    steps: []grid.Direction,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        self.grid.deinit();
        allocator.free(self.steps);
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
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
                else => return error.@"invalid element",
            };

            try row.append(e);
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
    }
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !usize {
    var parsed = try parseInput(input, allocator);
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
    var parsed = try parseInput(input, allocator);
    defer parsed.deinit(allocator);

    return 0;
}
