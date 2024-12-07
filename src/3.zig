const std = @import("std");

const re = @import("regex.zig");

const Tag = enum { mul, enable, disable };
const Token = union(Tag) {
    mul: struct { u32, u32 },
    enable: struct {},
    disable: struct {},
};

const Input = struct {
    tokens: []const Token,
    matches: []const re.Matches,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.tokens);
        for (self.matches) |m| {
            m.deinit();
        }
        allocator.free(self.matches);
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    const M = struct {
        tag: Tag,
        match: re.Match,

        pub fn compare(_: void, a: @This(), b: @This()) bool {
            return a.match.start < b.match.start;
        }
    };

    var matches = std.ArrayList(M).init(allocator);
    defer matches.deinit();

    var matches_temp = std.ArrayList(re.Matches).init(allocator);

    for ([_]struct { Tag, [:0]const u8 }{
        .{ .mul, "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" },
        .{ .enable, "do\\(\\)" },
        .{ .disable, "don't\\(\\)" },
    }) |pattern| {
        const regex = try re.Regex.init(
            allocator,
            pattern[1],
        );
        defer regex.deinit();

        var it = try regex.match(input);
        try matches_temp.append(it);

        while (it.next()) |match| {
            try matches.append(.{ .tag = pattern[0], .match = match });
        }
    }

    std.mem.sort(M, matches.items, {}, M.compare);

    const tokens = try allocator.alloc(Token, matches.items.len);
    for (matches.items, 0..) |match, i| {
        tokens[i] = switch (match.tag) {
            Tag.mul => Token{ .mul = .{
                try std.fmt.parseInt(u32, match.match.groups[1], 10),
                try std.fmt.parseInt(u32, match.match.groups[2], 10),
            } },
            Tag.enable => Token{ .enable = .{} },
            Tag.disable => Token{ .disable = .{} },
        };
    }

    return .{ .tokens = tokens, .matches = try matches_temp.toOwnedSlice() };
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u32 {
    const parsed = try parseInput(input, allocator);
    defer parsed.deinit(allocator);

    var sum: u32 = 0;
    for (parsed.tokens) |token| {
        switch (token) {
            .mul => |mul| {
                sum += mul[0] * mul[1];
            },
            else => {},
        }
    }

    return sum;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !usize {
    const parsed = try parseInput(input, allocator);
    defer parsed.deinit(allocator);

    var enabled = true;
    var sum: u32 = 0;
    for (parsed.tokens) |token| {
        switch (token) {
            .mul => |mul| {
                if (enabled) {
                    sum += mul[0] * mul[1];
                }
            },
            .disable => {
                enabled = false;
            },
            .enable => {
                enabled = true;
            },
        }
    }

    return sum;
}
