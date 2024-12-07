const std = @import("std");

const aoc = @import("aoc.zig");

const d1 = @import("1.zig");
const d2 = @import("2.zig");
const d3 = @import("3.zig");

const DAYS = .{
    .{ .day = 1, .part1 = d1.part1, .part2 = d1.part2 },
    .{ .day = 2, .part1 = d2.part1, .part2 = d2.part2 },
    .{ .day = 3, .part1 = d3.part1, .part2 = d3.part2 },
};

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

    // ./exe run <day> <part>
    // ./exe submit <day> <part>

    const command = args[1];
    const day = std.fmt.parseInt(u8, args[2], 10) catch {
        std.debug.panic("Failed to parse day: {s}\n", .{args[2]});
    };
    const part = std.fmt.parseInt(u8, args[3], 10) catch {
        std.debug.panic("Failed to parse part: {s}\n", .{args[3]});
    };

    const input = try client.getProblemInput(day);

    if (std.mem.eql(u8, command, "run")) {
        inline for (DAYS) |d| {
            if (d.day == day) {
                if (part == 1) {
                    const output = try d.part1(input, allocator);
                    std.debug.print("{any}\n", .{output});
                    break;
                } else if (part == 2) {
                    const output = try d.part2(input, allocator);
                    std.debug.print("{any}\n", .{output});
                    break;
                } else {
                    std.debug.panic("Invalid part: {d}\n", .{part});
                    break;
                }
            }
        } else {
            std.debug.panic("Unknown day: {d}\n", .{day});
        }
    } else if (std.mem.eql(u8, command, "submit")) {
        std.debug.panic("Not implemented\n", .{});
    } else {
        std.debug.panic("Unknown command: {s}\n", .{command});
    }
}
