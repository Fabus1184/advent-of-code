const std = @import("std");

const grid = @import("grid.zig");

const Input = struct {
    grid: grid.Grid(bool),
    guard: @Vector(2, isize),

    pub fn deinit(self: Input, allocator: std.mem.Allocator) void {
        for (self.grid.rows.items) |row| {
            allocator.free(row);
        }
        self.grid.deinit();
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    var g = grid.Grid(bool).init(allocator);

    var guard: @Vector(2, isize) = @splat(-1);

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    var i: isize = 0;
    while (lines.next()) |line| {
        var row = try allocator.alloc(bool, line.len);
        defer allocator.free(row);

        for (line, 0..) |c, j| {
            if (c == '.') {
                row[j] = false;
            } else if (c == '#') {
                row[j] = true;
            } else if (c == '^') {
                guard = .{ @intCast(j), i };
                row[j] = false;
            } else {
                return std.debug.panic("Invalid character: {c}\n", .{c});
            }
        }

        try g.addRow(row);

        i += 1;
    }

    if (guard[0] == -1 and guard[1] == -1) {
        return std.debug.panic("Guard not found\n", .{});
    }

    return .{ .grid = g, .guard = guard };
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !usize {
    var i = try parseInput(input, allocator);
    defer i.deinit(allocator);

    var visited = std.AutoHashMap(@Vector(2, isize), struct {}).init(allocator);
    defer visited.deinit();

    var direction = grid.Direction.Up;
    while (true) {
        try visited.put(i.guard, .{});

        const next = i.grid.get(i.guard + direction.toVector()) orelse break;

        if (next) {
            direction = direction.rotateRight90();
        } else {
            i.guard += direction.toVector();
        }
    }

    return visited.unmanaged.size;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !usize {
    var i = try parseInput(input, allocator);
    defer i.deinit(allocator);

    var count: usize = 0;
    const guard = i.guard;

    for (i.grid.rows.items) |row| {
        for (row) |*cell| {
            if (!cell.*) {
                cell.* = true;
                i.guard = guard;

                if (try loops(&i)) {
                    count += 1;
                }
                cell.* = false;
            }
        }
    }

    return count;
}

fn loops(input: *Input) !bool {
    var visited = std.AutoHashMap(struct { @Vector(2, isize), grid.Direction }, struct {}).init(input.grid.allocator);
    defer visited.deinit();

    var direction = grid.Direction.Up;
    while (!visited.contains(.{ input.guard, direction })) {
        try visited.put(.{ input.guard, direction }, .{});

        const next = input.grid.get(input.guard + direction.toVector()) orelse break;

        if (next) {
            direction = direction.rotateRight90();
        } else {
            input.guard += direction.toVector();
        }
    } else {
        return true;
    }

    return false;
}
