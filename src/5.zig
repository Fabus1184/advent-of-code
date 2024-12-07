const std = @import("std");

const Input = struct {
    rules: []const struct { u32, u32 },
    updates: [][]u32,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.rules);
        for (self.updates) |update| {
            allocator.free(update);
        }
        allocator.free(self.updates);
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    var parts = std.mem.splitSequence(u8, input, "\n\n");
    const part_rules = parts.next() orelse return error.@"missing rules";
    const part_updates = parts.next() orelse return error.@"missing updates";

    var rules = std.ArrayList(struct { u32, u32 }).init(allocator);
    {
        var lines = std.mem.splitSequence(u8, part_rules, "\n");
        while (lines.next()) |line| {
            var ps = std.mem.splitSequence(u8, line, "|");
            const a = try std.fmt.parseInt(u32, ps.next() orelse return error.@"missing a", 10);
            const b = try std.fmt.parseInt(u32, ps.next() orelse return error.@"missing b", 10);
            try rules.append(.{ a, b });
        }
    }

    var updates = std.ArrayList([]u32).init(allocator);
    {
        var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, part_updates, "\n"), "\n");
        while (lines.next()) |line| {
            var update = std.ArrayList(u32).init(allocator);
            var numbers = std.mem.splitSequence(u8, line, ",");
            while (numbers.next()) |part| {
                const n = try std.fmt.parseInt(u32, part, 10);
                try update.append(n);
            }
            try updates.append(try update.toOwnedSlice());
        }
    }

    return .{ .rules = try rules.toOwnedSlice(), .updates = try updates.toOwnedSlice() };
}

fn correct(rules: []const struct { u32, u32 }, update: []const u32) bool {
    for (rules) |rule| {
        const a = std.mem.indexOfScalar(u32, update, rule[0]) orelse continue;
        const b = std.mem.indexOfScalar(u32, update, rule[1]) orelse continue;

        if (a > b) {
            return false;
        }
    }
    return true;
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !usize {
    const parsed = try parseInput(input, allocator);
    defer parsed.deinit(allocator);

    var sum: usize = 0;
    for (parsed.updates) |update| {
        if (correct(parsed.rules, update)) {
            sum += update[update.len / 2];
        }
    }

    return sum;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !usize {
    const parsed = try parseInput(input, allocator);
    defer parsed.deinit(allocator);

    var sum: usize = 0;

    for (parsed.updates) |update| {
        if (correct(parsed.rules, update)) {
            continue;
        }

        while (!correct(parsed.rules, update)) {
            for (parsed.rules) |rule| {
                const a = std.mem.indexOfScalar(u32, update, rule[0]) orelse continue;
                const b = std.mem.indexOfScalar(u32, update, rule[1]) orelse continue;

                if (a > b) {
                    std.mem.swap(u32, &update[a], &update[b]);
                }
            }
        }

        sum += update[update.len / 2];
    }

    return sum;
}
