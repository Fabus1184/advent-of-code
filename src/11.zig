const std = @import("std");

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !std.ArrayList(u64) {
    var row = std.ArrayList(u64).init(allocator);

    var numbers = std.mem.tokenizeAny(u8, input, " \n");
    while (numbers.next()) |number| {
        const n = try std.fmt.parseInt(u64, number, 10);
        try row.append(n);
    }

    return row;
}

fn iterate(numbers: []const u64, allocator: std.mem.Allocator, limit: usize) !u64 {
    var counts = std.AutoArrayHashMap(u64, u64).init(allocator);
    defer counts.deinit();

    for (numbers) |n| {
        (try counts.getOrPutValue(n, 0)).value_ptr.* += 1;
    }

    for (0..limit) |_| {
        var new = std.AutoArrayHashMap(u64, u64).init(allocator);

        var it = counts.iterator();
        while (it.next()) |e| {
            const value = e.value_ptr.*;
            const key = e.key_ptr.*;

            if (key == 0) {
                (try new.getOrPutValue(1, 0)).value_ptr.* += value;
            } else if ((std.math.log10_int(key) + 1) % 2 == 0) {
                var buf: [32]u8 = undefined;
                const str = try std.fmt.bufPrint(&buf, "{d}", .{key});
                const half = str.len / 2;
                const first = try std.fmt.parseInt(u64, str[0..half], 10);
                const second = try std.fmt.parseInt(u64, str[half..], 10);

                (try new.getOrPutValue(first, 0)).value_ptr.* += value;
                (try new.getOrPutValue(second, 0)).value_ptr.* += value;
            } else {
                (try new.getOrPutValue(key * 2024, 0)).value_ptr.* += value;
            }
        }

        counts.deinit();
        counts = new;
    }

    var sum: u64 = 0;

    for (counts.values()) |v| {
        sum += v;
    }

    return sum;
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const numbers = try parseInput(input, allocator);
    defer numbers.deinit();

    return iterate(numbers.items, allocator, 25);
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const numbers = try parseInput(input, allocator);
    defer numbers.deinit();

    return iterate(numbers.items, allocator, 75);
}
