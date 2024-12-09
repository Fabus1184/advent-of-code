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

    pub fn checksum(self: *@This()) u64 {
        var sum: u64 = 0;
        var i: u64 = 0;

        for (self.blocks.items) |block| {
            if (block.fileId) |v| {
                for (0..block.size) |j| {
                    sum += @as(u64, i + j) * @as(u64, v);
                }
            }

            i += @intCast(block.size);
        }

        return sum;
    }

    pub fn compact(self: *@This()) !void {
        var modified = true;
        while (modified) {
            modified = false;

            for (0..self.blocks.items.len) |i_| {
                const src = self.blocks.items.len - i_ - 1;
                if (self.blocks.items[src].fileId == null) {
                    continue;
                }

                const dest = for (0..src) |dest| {
                    if (self.blocks.items[dest].fileId == null and self.blocks.items[dest].size >= self.blocks.items[src].size) {
                        break dest;
                    }
                } else {
                    continue;
                };

                const sizeDiff = self.blocks.items[dest].size - self.blocks.items[src].size;
                std.mem.swap(?u16, &self.blocks.items[dest].fileId, &self.blocks.items[src].fileId);
                self.blocks.items[dest].size -= sizeDiff;

                modified = true;

                if (sizeDiff > 0) {
                    try self.blocks.insert(dest + 1, .{ .fileId = null, .size = @intCast(sizeDiff) });
                    break;
                }
            }
        }
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

    var blocks = disk.blocks.items;

    var sum: u64 = 0;
    var offset: u64 = 0;
    while (blocks.len > 0) {
        if (blocks[0].fileId) |v| {
            sum += offset * v;            
        } else  {
            const i = for (0..blocks.len) |i| {
                if (blocks[blocks.len - i - 1].fileId != null) {
                    break blocks.len - i - 1;
                }
            } else break;

            sum += offset * blocks[i].fileId.?;
            
            blocks[i].size -= 1;
            if (blocks[i].size == 0) {
                blocks = blocks[0..i];
            }
        }
        
        blocks[0].size -= 1;
        if (blocks[0].size == 0) {
            blocks = blocks[1..];
        }
        
        offset += 1;
    }

    return sum;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u64 {
    var disk = try parseInput(input, allocator);
    defer disk.deinit();
    
    try disk.compact();

    return disk.checksum();
}
