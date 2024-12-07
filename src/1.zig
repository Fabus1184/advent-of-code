const std = @import("std");

const Input = struct {
    lefts: std.ArrayList(u32),
    rights: std.ArrayList(u32),

    fn deinit(self: *Input) void {
        self.lefts.deinit();
        self.rights.deinit();
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    var lefts = std.ArrayList(u32).init(allocator);
    var rights = std.ArrayList(u32).init(allocator);

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        var parts = std.mem.tokenizeSequence(u8, line, " ");

        const left = try std.fmt.parseInt(u32, parts.next().?, 10);
        const right = try std.fmt.parseInt(u32, parts.next().?, 10);

        try lefts.append(left);
        try rights.append(right);
    }

    return .{ .lefts = lefts, .rights = rights };
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u32 {
    var parsedInput = try parseInput(input, allocator);
    defer parsedInput.deinit();

    std.mem.sort(u32, parsedInput.lefts.items, {}, comptime std.sort.asc(u32));
    std.mem.sort(u32, parsedInput.rights.items, {}, comptime std.sort.asc(u32));

    var result: u32 = 0;
    for (parsedInput.lefts.items, parsedInput.rights.items) |left, right| {
        result += @abs(@as(i32, @intCast(left)) - @as(i32, @intCast(right)));
    }

    return result;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u32 {
    var parsedInput = try parseInput(input, allocator);
    defer parsedInput.deinit();

    std.mem.sort(u32, parsedInput.lefts.items, {}, comptime std.sort.asc(u32));

    var result: u32 = 0;
    for (parsedInput.lefts.items) |left| {
        var count: u32 = 0;
        for (parsedInput.rights.items) |right| {
            if (left == right) {
                count += 1;
            }
        }
        result += left * count;
    }

    return result;
}
