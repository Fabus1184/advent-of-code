const std = @import("std");

const grid = @import("grid.zig");

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !grid.Grid(u8) {
    var g = grid.Grid(u8).init(allocator);

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        var l = std.ArrayList(u8).init(allocator);
        defer l.deinit();

        for (line) |item| {
            try l.append(try std.fmt.parseInt(u8, &[1]u8{item}, 10));
        }

        try g.addRow(l.items);
    }

    return g;
}

fn ways(g: *const grid.Grid(u8), pos: @Vector(2, isize), list: *std.ArrayList(@Vector(2, isize))) !void {
    if (g.get(pos)) |v| {
        if (v == 9) {
            try list.append(pos);
        }

        for (grid.Directions4) |d| {
            if (g.get(pos + d.toVector())) |v2| {
                if (v2 == v + 1) {
                    try ways(g, pos + d.toVector(), list);
                }
            }
        }
    }
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const g = try parseInput(input, allocator);
    defer g.deinit();

    var sum: u64 = 0;

    var it = g.elements();
    while (it.next()) |e| {
        if (e.element == 0) {
            var list = std.ArrayList(@Vector(2, isize)).init(allocator);
            try ways(&g, e.position, &list);

            var set = try std.DynamicBitSet.initEmpty(allocator, g.size()[0] * g.size()[1]);
            defer set.deinit();

            for (list.items) |pos| {
                set.set(@as(usize, @intCast(pos[0])) + @as(usize, @intCast(pos[1])) * g.size()[0]);
            }

            sum += set.count();
        }
    }

    return sum;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const g = try parseInput(input, allocator);
    defer g.deinit();

    var list = std.ArrayList(@Vector(2, isize)).init(allocator);
    defer list.deinit();

    var it = g.elements();
    while (it.next()) |e| {
        if (e.element == 0) {
            try ways(&g, e.position, &list);
        }
    }

    return list.items.len;
}
