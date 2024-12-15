const std = @import("std");
const re = @import("regex.zig");

const Robot = struct {
    position: @Vector(2, i32),
    velocity: @Vector(2, i32),
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) ![]Robot {
    const regex = try re.Regex.init(allocator, "p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)");
    defer regex.deinit();

    var robots = std.ArrayList(Robot).init(allocator);

    var lines = std.mem.splitScalar(u8, std.mem.trimRight(u8, input, "\n"), '\n');
    while (lines.next()) |line| {
        const matches = try regex.match(line);
        defer matches.deinit();

        const x = try std.fmt.parseInt(i32, matches.matches[0].groups[1], 10);
        const y = try std.fmt.parseInt(i32, matches.matches[0].groups[2], 10);
        const vx = try std.fmt.parseInt(i32, matches.matches[0].groups[3], 10);
        const vy = try std.fmt.parseInt(i32, matches.matches[0].groups[4], 10);

        try robots.append(.{
            .position = .{ x, y },
            .velocity = .{ vx, vy },
        });
    }

    return robots.toOwnedSlice();
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const robots = try parseInput(input, allocator);
    defer allocator.free(robots);

    const width = 101;
    const height = 103;

    for (0..100) |_| {
        for (robots) |*robot| {
            robot.position[0] = @mod(robot.position[0] + robot.velocity[0], width);
            robot.position[1] = @mod(robot.position[1] + robot.velocity[1], height);
        }
    }

    var q1: usize = 0;
    var q2: usize = 0;
    var q3: usize = 0;
    var q4: usize = 0;

    for (robots) |robot| {
        if (robot.position[0] < width / 2) {
            if (robot.position[1] < height / 2) {
                q1 += 1;
            } else if (robot.position[1] > height / 2) {
                q3 += 1;
            }
        } else if (robot.position[0] > width / 2) {
            if (robot.position[1] < height / 2) {
                q2 += 1;
            } else if (robot.position[1] > height / 2) {
                q4 += 1;
            }
        }
    }

    return q1 * q2 * q3 * q4;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !usize {
    const robots = try parseInput(input, allocator);
    defer allocator.free(robots);

    const width = 101;
    const height = 103;

    var minStdDev = std.math.floatMax(f32);
    var minIndex: usize = 0;

    for (0..10000) |i| {
        for (robots) |*robot| {
            robot.position[0] = @mod(robot.position[0] + robot.velocity[0], width);
            robot.position[1] = @mod(robot.position[1] + robot.velocity[1], height);
        }

        var mean: @Vector(2, i32) = .{ 0, 0 };
        for (robots) |robot| {
            mean += robot.position;
        }
        mean /= @splat(@intCast(robots.len));

        var variance: @Vector(2, i32) = .{ 0, 0 };
        for (robots) |robot| {
            variance += (robot.position - mean) * (robot.position - mean);
        }
        variance /= @splat(@intCast(robots.len));

        const stdDev = @sqrt(@as(f32, @floatFromInt(variance[0] + variance[1])));

        if (stdDev < minStdDev) {
            minStdDev = stdDev;
            minIndex = i;
        }
    }

    return minIndex + 1;
}
