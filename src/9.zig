const std = @import("std");

const Block = struct {
    fileId: ?u16,
    size: u16,
};

const Input = struct {
    blocks: std.ArrayList(Block),

    pub fn deinit(self: @This()) void {
        self.blocks.deinit();
    }

    pub fn linearize(self: @This(), allocator: std.mem.Allocator) ![]?u16 {
        var size: usize = 0;
        for (self.blocks.items) |block| {
            size += @intCast(block.size);
        }

        const array = try allocator.alloc(?u16, size);
        
        var i: usize = 0;
        for (self.blocks.items) |block| {
            for (0..block.size) |j| {
                if (block.fileId) |f| {
                    array[i + j] = f;
                } else {
                    array[i + j] = null;
                }
            }

            i += @intCast(block.size);
        }

        return array;
    }
};

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    var blocks = std.ArrayList(Block).init(allocator);

    for (std.mem.trimRight(u8, input, "\n"), 0..) |c, i| {
        const n = try std.fmt.parseInt(u16, &[_]u8{c}, 10);

        if (n == 0) {
            continue;
        }

        if (i % 2 == 0) {
            try blocks.append(.{ .fileId = @intCast(i / 2), .size = n });
        } else {
            try blocks.append(.{ .fileId = null, .size = n });
        }
    }

    return .{ .blocks = blocks };
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u64 {
    var disk = try parseInput(input, allocator);
    defer disk.deinit();

    const linear = try disk.linearize(allocator);
    defer allocator.free(linear);

    var modified = true;
    while (modified) {
        modified = false;

        for (0..linear.len) |i| {
            if (linear[i] == null) {
                for (0..linear.len - i) |j| {
                    const j_ = linear.len - j - 1;
                    if (linear[j_] != null) {
                        std.mem.swap(?u16, &linear[i], &linear[j_]);
                        modified = true;
                        break;
                    }
                }
            }
        }
    }

    var sum: u64 = 0;
    for (linear, 0..) |v, i| {
        if (v) |value| {
            sum += @as(u64, i) * @as(u64, value);
        }
    }

    return sum;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u64 {
    var disk = try parseInput(input, allocator);

    var modified = true;
    while (modified) {
        modified = false;

        for (0..disk.blocks.items.len) |i_| {
            const src = disk.blocks.items.len - i_ - 1;
            if (disk.blocks.items[src].fileId == null) {
                continue;
            }

            const dest = for (0..src) |dest| {
                if (disk.blocks.items[dest].fileId == null and disk.blocks.items[dest].size >= disk.blocks.items[src].size) {
                    break dest;
                }
            } else {
                continue;
            };

            const sizeDiff = disk.blocks.items[dest].size - disk.blocks.items[src].size;
            std.mem.swap(?u16, &disk.blocks.items[dest].fileId, &disk.blocks.items[src].fileId);
            disk.blocks.items[dest].size -= sizeDiff;

            modified = true;

            if (sizeDiff > 0) {
                try disk.blocks.insert(dest + 1, .{ .fileId = null, .size = @intCast(sizeDiff) });
                break;
            }
        }
    }

    var sum: u64 = 0;
    var i: u32 = 0;
    for (disk.blocks.items) |block| {
        if (block.fileId) |v| {
            for (0..block.size) |j| {
                sum += @as(u64, i + j) * @as(u64, v);
            }
        }

        i += @intCast(block.size);
    }

    return sum;
}
