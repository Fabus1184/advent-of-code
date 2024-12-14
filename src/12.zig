const std = @import("std");

const grid = @import("grid.zig");

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !grid.Grid(u8) {
    var g = grid.Grid(u8).init(allocator);

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        try g.addRow(line);
    }

    return g;
}

pub fn regionAreaAndPerimeter(char: u8, position: @Vector(2, isize), visited: *std.DynamicBitSet, g: *const grid.Grid(u8), outside: ?*std.AutoHashMap(@Vector(2, isize), u32)) !?struct { u32, u32 } {
    if (g.get(position) != char) {
        return null;
    }

    const size = g.size();

    if (visited.isSet(@as(usize, @intCast(position[1])) * size[0] + @as(usize, @intCast(position[0])))) {
        return .{ 0, 0 };
    }
    visited.set(@as(usize, @intCast(position[1])) * size[0] + @as(usize, @intCast(position[0])));

    var area: u32 = 1;
    var perimeter: u32 = 0;

    for (grid.Directions4) |direction| {
        if (try regionAreaAndPerimeter(char, position + direction.toVector(), visited, g, outside)) |r| {
            area += r[0];
            perimeter += r[1];
        } else {
            if (outside) |o| {
                (try o.getOrPutValue(position + direction.toVector(), 0)).value_ptr.* += 1;
            }

            perimeter += 1;
        }
    }

    return .{ area, perimeter };
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const g = try parseInput(input, allocator);
    defer g.deinit();

    const size = g.size();

    var visited = try std.DynamicBitSet.initEmpty(allocator, g.size()[0] * g.size()[1]);
    defer visited.deinit();

    var price: u64 = 0;

    var it = g.elements();
    while (it.next()) |e| {
        const position = e.position;

        if (!visited.isSet(@as(usize, @intCast(position[1])) * size[0] + @as(usize, @intCast(position[0])))) {
            const r = (try regionAreaAndPerimeter(e.element, position, &visited, &g, null)).?;
            price += r[0] * r[1];
        }
    }

    return price;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u64 {
    _ = .{ input, allocator };

    return 0;
}
