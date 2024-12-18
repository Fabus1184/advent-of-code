const std = @import("std");
const grid = @import("grid.zig");

const Input = struct {
    grid: grid.Grid(bool),
    start: @Vector(2, isize),
    end: @Vector(2, isize),

    fn deinit(self: Input) void {
        self.grid.deinit();
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    var g = grid.Grid(bool).init(allocator);

    var start: ?@Vector(2, isize) = null;
    var end: ?@Vector(2, isize) = null;

    var it = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    var i: isize = 0;
    while (it.next()) |line| : (i += 1) {
        var row = std.ArrayList(bool).init(allocator);
        defer row.deinit();

        for (line, 0..) |col, j| {
            switch (col) {
                '#' => {
                    try row.append(true);
                },
                '.' => {
                    try row.append(false);
                },
                'S' => {
                    start = .{ @intCast(j), i };
                    try row.append(false);
                },
                'E' => {
                    end = .{ @intCast(j), i };
                    try row.append(false);
                },
                else => return error.@"invalid character",
            }
        }

        try g.addRow(row.items);
    }

    return Input{ .grid = g, .start = start.?, .end = end.? };
}

const Element = struct {
    position: @Vector(2, isize),
    direction: grid.Direction,
    cost: u64,

    fn compare(_: void, a: @This(), b: @This()) std.math.Order {
        if (a.cost < b.cost) {
            return .lt;
        } else if (a.cost > b.cost) {
            return .gt;
        } else {
            return .eq;
        }
    }
};

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u64 {
    var parsed = try parseInput(input, allocator);
    defer parsed.deinit();

    var visited = std.AutoHashMap(@Vector(2, isize), Element).init(allocator);
    defer visited.deinit();

    var queue = std.PriorityQueue(Element, void, Element.compare).init(allocator, {});
    defer queue.deinit();

    try queue.add(.{
        .position = parsed.start,
        .direction = grid.Direction.Left,
        .cost = 0,
    });

    while (queue.removeOrNull()) |element| {
        const current = element.position;

        if (visited.contains(current)) {
            continue;
        }

        try visited.put(current, element);

        if (current[0] == parsed.end[0] and current[1] == parsed.end[1]) {
            break;
        }

        inline for (.{
            element.direction,
            element.direction.rotateRight90(),
            element.direction.rotateRight90().rotateRight90().rotateRight90(),
        }, .{ 1, 1001, 1001 }) |direction, cost| {
            const next = current + direction.toVector();

            if (!parsed.grid.get(next).? and !visited.contains(next)) {
                try queue.add(.{ .position = next, .direction = direction, .cost = element.cost + cost });
            }
        }
    }

    return visited.get(parsed.end).?.cost;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u64 {
    var parsed = try parseInput(input, allocator);
    defer parsed.deinit();

    return 0;
}
