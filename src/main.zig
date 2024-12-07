const std = @import("std");

const aoc = @import("aoc.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const token = std.process.getEnvVarOwned(allocator, "TOKEN") catch |err| {
        std.debug.panic("TOKEN environment variable not set.\n", .{err});
    };

    var client = try aoc.AocClient.init(allocator, token, 2024);
    defer client.deinit();

    const input = try client.getProblemInput(2024, 1) catch |err| {
        std.debug.panic("Failed to get input: {}\n", .{err});
    };

    std.debug.print("Input: {}\n", .{input});
}
