const std = @import("std");

const Input = struct {
    const Equation = struct {
        result: u64,
        numbers: []const u64,
    };

    equations: []const Equation,

    pub fn deinit(self: Input, allocator: std.mem.Allocator) void {
        for (self.equations) |equation| {
            allocator.free(equation.numbers);
        }
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    var equations = std.ArrayList(Input.Equation).init(allocator);

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        var parts = std.mem.splitSequence(u8, line, ": ");
        const result = try std.fmt.parseInt(u64, parts.next() orelse return error.@"missing result", 10);

        const numbers = parts.next() orelse return error.@"missing numbers";
        var nums = std.ArrayList(u64).init(allocator);
        var it = std.mem.splitSequence(u8, numbers, " ");
        while (it.next()) |num| {
            const n = try std.fmt.parseInt(u64, num, 10);
            try nums.append(n);
        }

        try equations.append(.{
            .result = result,
            .numbers = try nums.toOwnedSlice(),
        });
    }

    return .{
        .equations = try equations.toOwnedSlice(),
    };
}

fn possible(acc: u64, equation: []const u64, result: u64, concat: bool) bool {
    if (equation.len == 0) {
        if (acc == result) {
            return true;
        } else {
            return false;
        }
    }

    if (possible(acc + equation[0], equation[1..], result, concat)) {
        return true;
    } else if (possible(acc * equation[0], equation[1..], result, concat)) {
        return true;
    } else if (concat and possible(std.math.pow(u64, 10, std.math.log10(equation[0]) + 1) * acc + equation[0], equation[1..], result, concat)) {
        return true;
    }

    return false;
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const i = try parseInput(input, allocator);
    defer i.deinit(allocator);

    var sum: u64 = 0;

    for (i.equations) |equation| {
        if (possible(0, equation.numbers, equation.result, false)) {
            sum += equation.result;
        }
    }

    return sum;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !usize {
    const i = try parseInput(input, allocator);
    defer i.deinit(allocator);

    var sum: u64 = 0;

    for (i.equations) |equation| {
        if (possible(0, equation.numbers, equation.result, true)) {
            sum += equation.result;
        }
    }

    return sum;
}
