const std = @import("std");

const aoc = @import("aoc.zig");

const d1 = @import("1.zig");
const d2 = @import("2.zig");
const d3 = @import("3.zig");
const d4 = @import("4.zig");
const d5 = @import("5.zig");
const d6 = @import("6.zig");
const d7 = @import("7.zig");
const d8 = @import("8.zig");
const d9 = @import("9.zig");
const d10 = @import("10.zig");
const d11 = @import("11.zig");
const d12 = @import("12.zig");
const d13 = @import("13.zig");

const DAYS = .{
    .{ .day = 1, .part1 = d1.part1, .part2 = d1.part2 },
    .{ .day = 2, .part1 = d2.part1, .part2 = d2.part2 },
    .{ .day = 3, .part1 = d3.part1, .part2 = d3.part2 },
    .{ .day = 4, .part1 = d4.part1, .part2 = d4.part2 },
    .{ .day = 5, .part1 = d5.part1, .part2 = d5.part2 },
    .{ .day = 6, .part1 = d6.part1, .part2 = d6.part2 },
    .{ .day = 7, .part1 = d7.part1, .part2 = d7.part2 },
    .{ .day = 8, .part1 = d8.part1, .part2 = d8.part2 },
    .{ .day = 9, .part1 = d9.part1, .part2 = d9.part2 },
    .{ .day = 10, .part1 = d10.part1, .part2 = d10.part2 },
    .{ .day = 11, .part1 = d11.part1, .part2 = d11.part2 },
    .{ .day = 12, .part1 = d12.part1, .part2 = d12.part2 },
    .{ .day = 13, .part1 = d13.part1, .part2 = d13.part2 },
};

test "test" {
    const allocator = std.testing.allocator;

    const token = std.process.getEnvVarOwned(allocator, "TOKEN") catch {
        std.debug.panic("TOKEN environment variable not set\n", .{});
    };

    var client = try aoc.AocClient.init(allocator, token, 2024);
    defer {
        client.deinit() catch |err| {
            std.debug.panic("Failed to deinit client: {any}\n", .{err});
        };
    }

    inline for (DAYS) |e| {
        const input = try client.getProblemInput(e.day);

        const part1 = try e.part1(input, allocator);
        const part2 = try e.part2(input, allocator);

        std.debug.print("Day {d} Part 1: {d}\n", .{ e.day, part1 });
        std.debug.print("Day {d} Part 2: {d}\n", .{ e.day, part2 });
    }
}

const ANSII_RESET = "\x1b[0m";
const ANSII_CURSIVE = "\x1b[3m";
const ANSII_BOLD = "\x1b[1m";
const ANSII_GREEN = "\x1b[32m";

// ./exe run <day> <part>
// ./exe submit <day> <part>
pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const token = std.process.getEnvVarOwned(allocator, "TOKEN") catch {
        std.debug.panic("TOKEN environment variable not set\n", .{});
    };

    var client = try aoc.AocClient.init(allocator, token, 2024);
    defer {
        client.deinit() catch |err| {
            std.debug.panic("Failed to deinit client: {any}\n", .{err});
        };
    }

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        std.debug.print("Usage: {s} <day>\n", .{args[0]});
        return;
    }

    const command = args[1];
    const day = std.fmt.parseInt(u8, args[2], 10) catch {
        std.debug.panic("Failed to parse day: {s}\n", .{args[2]});
    };
    const part = std.fmt.parseInt(u8, args[3], 10) catch {
        std.debug.panic("Failed to parse part: {s}\n", .{args[3]});
    };

    const input = try client.getProblemInput(day);

    if (std.mem.eql(u8, command, "run")) {
        const now = std.time.microTimestamp();

        const answer = inline for (DAYS) |d| {
            if (d.day == day) {
                if (part == 1) {
                    break try d.part1(input, allocator);
                } else if (part == 2) {
                    break try d.part2(input, allocator);
                } else {
                    std.debug.panic("Invalid part: {d}\n", .{part});
                }
            }
        } else {
            std.debug.panic("Unknown day: {d}\n", .{day});
        };

        std.debug.print("🎄 {s}Day {d} Part {d}{s}: {s}{d}{s}\n", .{ ANSII_CURSIVE, day, part, ANSII_RESET, ANSII_BOLD ++ ANSII_GREEN, answer, ANSII_RESET });

        const elapsed = std.time.microTimestamp() - now;
        std.debug.print("took: {d:.3}ms\n", .{@as(f32, @floatFromInt(elapsed)) / 1000.0});
    } else if (std.mem.eql(u8, command, "submit")) {
        std.debug.panic("Not implemented\n", .{});
    } else {
        std.debug.panic("Unknown command: {s}\n", .{command});
    }
}
