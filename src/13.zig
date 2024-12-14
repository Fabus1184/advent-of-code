const std = @import("std");
const re = @import("regex.zig");

const Machine = struct {
    a: @Vector(2, u32),
    b: @Vector(2, u32),
    prize: @Vector(2, u32),
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) ![]const Machine {
    var machines = std.ArrayList(Machine).init(allocator);

    const buttonRegex = try re.Regex.init(allocator, "X\\+([0-9]+), Y\\+([0-9]+)");
    defer buttonRegex.deinit();

    const prizeRegex = try re.Regex.init(allocator, "X=([0-9]+), Y=([0-9]+)");
    defer prizeRegex.deinit();

    var it = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n\n");
    while (it.next()) |m| {
        var lines = std.mem.splitScalar(u8, m, '\n');

        const buttonA = (try buttonRegex.match(lines.next().?));
        defer buttonA.deinit();
        const a = .{ try std.fmt.parseInt(u32, buttonA.matches[0].groups[1], 10), try std.fmt.parseInt(u32, buttonA.matches[0].groups[2], 10) };

        const buttonB = (try buttonRegex.match(lines.next().?));
        defer buttonB.deinit();
        const b = .{ try std.fmt.parseInt(u32, buttonB.matches[0].groups[1], 10), try std.fmt.parseInt(u32, buttonB.matches[0].groups[2], 10) };

        const prize = (try prizeRegex.match(lines.next().?));
        defer prize.deinit();
        const p = .{ try std.fmt.parseInt(u32, prize.matches[0].groups[1], 10), try std.fmt.parseInt(u32, prize.matches[0].groups[2], 10) };

        try machines.append(.{ .a = a, .b = b, .prize = p });
    }

    return try machines.toOwnedSlice();
}

fn solveEquations2(comptime T: type, a: [3]T, b: [3]T) ?[2]T {
    const det = a[0] * b[1] - a[1] * b[0];
    const detX = a[2] * b[1] - a[1] * b[2];
    const detY = a[0] * b[2] - a[2] * b[0];

    const x = std.math.divExact(T, detX, det) catch return null;
    const y = std.math.divExact(T, detY, det) catch return null;

    return .{ x, y };
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const machines = try parseInput(input, allocator);
    defer allocator.free(machines);

    var tokens: u64 = 0;

    for (machines) |machine| {
        const res = solveEquations2(
            i64,
            .{ machine.a[0], machine.b[0], machine.prize[0] },
            .{ machine.a[1], machine.b[1], machine.prize[1] },
        );

        if (res) |r| {
            tokens += @as(u64, @intCast(r[0] * 3 + r[1]));
        }
    }

    return tokens;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const machines = try parseInput(input, allocator);
    defer allocator.free(machines);

    var tokens: u64 = 0;

    for (machines) |machine| {
        const res = solveEquations2(
            i64,
            .{ machine.a[0], machine.b[0], @as(i64, machine.prize[0]) + 10000000000000 },
            .{ machine.a[1], machine.b[1], @as(i64, machine.prize[1]) + 10000000000000 },
        );

        if (res) |r| {
            tokens += @as(u64, @intCast(r[0] * 3 + r[1]));
        }
    }

    return tokens;
}
