const std = @import("std");

const grid = @import("grid.zig");

const Input = struct {
    grid: grid.Grid(u8),
    antennas: std.AutoHashMap(u8, std.ArrayList(@Vector(2, isize))),

    pub fn deinit(self: *@This()) void {
        self.grid.deinit();
        var it = self.antennas.valueIterator();
        while (it.next()) |v| {
            v.deinit();
        }
        self.antennas.deinit();
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    var g = grid.Grid(u8).init(allocator);

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        try g.addRow(line);
    }

    var antennas = std.AutoHashMap(u8, std.ArrayList(@Vector(2, isize))).init(allocator);

    for (g.rows.items, 0..) |row, y| {
        for (row, 0..) |item, x| {
            if (std.ascii.isAlphanumeric(item)) {
                const key = item;
                const value = try antennas.getOrPut(key);
                if (!value.found_existing) {
                    value.value_ptr.* = std.ArrayList(@Vector(2, isize)).init(allocator);
                }
                try value.value_ptr.append(.{ @intCast(x), @intCast(y) });
            }
        }
    }

    return .{ .grid = g, .antennas = antennas };
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !usize {
    var parsed = try parseInput(input, allocator);
    defer parsed.deinit();

    var it = parsed.antennas.iterator();
    while (it.next()) |e| {
        for (e.value_ptr.items) |a| {
            for (e.value_ptr.items) |b| {
                if (std.mem.eql(isize, &@as([2]isize, a), &@as([2]isize, b))) {
                    continue;
                }
                if (parsed.grid.at(a + (a - b))) |value| {
                    value.* = '*';
                }
            }
        }
    }

    var count: usize = 0;
    for (parsed.grid.rows.items) |row| {
        for (row) |item| {
            if (item == '*') {
                count += 1;
            }
        }
    }

    return count;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u64 {
    var parsed = try parseInput(input, allocator);
    defer parsed.deinit();

    var it = parsed.antennas.iterator();
    while (it.next()) |e| {
        for (e.value_ptr.items) |a| {
            for (e.value_ptr.items) |b| {
                if (std.mem.eql(isize, &@as([2]isize, a), &@as([2]isize, b))) {
                    continue;
                }
                var i: isize = 0;
                while (parsed.grid.at(a + @as(@Vector(2, isize), @splat(i)) * (a - b))) |value| {
                    value.* = '*';
                    i += 1;
                }
            }
        }
    }

    var count: usize = 0;
    for (parsed.grid.rows.items) |row| {
        for (row) |item| {
            if (item == '*') {
                count += 1;
            }
        }
    }

    return count;
}
