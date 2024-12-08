const std = @import("std");

const grid = @import("grid.zig");

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !usize {
    var g = grid.Grid(u8).init(allocator);
    defer g.deinit();

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        try g.addRow(line);
    }

    const size = g.size();

    var count: usize = 0;

    for (0..size[0]) |x| {
        for (0..size[1]) |y| {
            for (grid.Directions8) |d| {
                var it = g.neighbors(.{ @intCast(x), @intCast(y) }, d, 4);
                const buffer: [4]u8 = [_]u8{
                    it.next() orelse continue,
                    it.next() orelse continue,
                    it.next() orelse continue,
                    it.next() orelse continue,
                };

                if (std.mem.eql(u8, &buffer, "XMAS")) {
                    count += 1;
                }
            }
        }
    }

    return count;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !usize {
    var g = grid.Grid(u8).init(allocator);
    defer g.deinit();

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        try g.addRow(line);
    }

    const size = g.size();

    var count: usize = 0;

    for (0..size[0]) |x| {
        for (0..size[1]) |y| {
            var it1 = g.neighbors(.{ @intCast(x), @intCast(y) }, .RightDown, 3);
            const str1: [3]u8 = [_]u8{
                it1.next() orelse continue,
                it1.next() orelse continue,
                it1.next() orelse continue,
            };

            var it2 = g.neighbors(.{ @intCast(x + 2), @intCast(y) }, .DownLeft, 3);
            const str2: [3]u8 = [_]u8{
                it2.next() orelse continue,
                it2.next() orelse continue,
                it2.next() orelse continue,
            };

            if ((std.mem.eql(u8, &str1, "MAS") or std.mem.eql(u8, &str1, "SAM")) and
                (std.mem.eql(u8, &str2, "MAS") or std.mem.eql(u8, &str2, "SAM")))
            {
                count += 1;
            }
        }
    }

    return count;
}
