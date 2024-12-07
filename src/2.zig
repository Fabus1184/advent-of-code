const std = @import("std");

const Input = struct {
    values: []const []u32,

    fn deinit(self: Input, allocator: std.mem.Allocator) void {
        for (self.values) |line| {
            allocator.free(line);
        }
        allocator.free(self.values);
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    var values = std.ArrayList([]u32).init(allocator);

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        var buffer = std.ArrayList(u32).init(allocator);
        defer buffer.deinit();

        var parts = std.mem.tokenize(u8, line, " ");
        while (parts.next()) |part| {
            const value = try std.fmt.parseInt(u32, part, 10);
            try buffer.append(value);
        }

        try values.append(try buffer.toOwnedSlice());
    }

    return .{ .values = try values.toOwnedSlice() };
}

fn isSafe(line: []const u32) bool {
    if (!(std.sort.isSorted(u32, line, {}, comptime std.sort.asc(u32)) or std.sort.isSorted(u32, line, {}, comptime std.sort.desc(u32)))) {
        return false;
    }

    var windows = std.mem.window(u32, line, 2, 1);
    while (windows.next()) |w| {
        const diff = @abs(@as(i32, @intCast(w[0])) - @as(i32, @intCast(w[1])));
        if (diff < 1 or diff > 3) {
            return false;
        }
    }

    return true;
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !usize {
    const values = try parseInput(input, allocator);
    defer values.deinit(allocator);

    var safe: usize = 0;
    for (values.values) |line| {
        if (isSafe(line)) {
            safe += 1;
        }
    }

    return safe;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !usize {
    const values = try parseInput(input, allocator);
    defer values.deinit(allocator);

    var safe: usize = 0;
    for (values.values) |line| {
        if (isSafe(line)) {
            safe += 1;
        } else {
            for (0..line.len) |i| {
                var copy = try allocator.alloc(u32, line.len - 1);
                defer allocator.free(copy);

                std.mem.copyForwards(u32, copy, line[0..i]);
                std.mem.copyForwards(u32, copy[i..], line[i + 1 ..]);
                if (isSafe(copy)) {
                    safe += 1;
                    break;
                }
            }
        }
    }

    return safe;
}
