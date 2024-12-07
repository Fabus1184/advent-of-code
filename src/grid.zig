const std = @import("std");

pub const Direction = enum {
    Up,
    UpRight,
    Right,
    RightDown,
    Down,
    DownLeft,
    Left,
    LeftUp,
    pub fn toVector(self: @This()) @Vector(2, isize) {
        return switch (self) {
            .Up => .{ 0, -1 },
            .UpRight => .{ 1, -1 },
            .Right => .{ 1, 0 },
            .RightDown => .{ 1, 1 },
            .Down => .{ 0, 1 },
            .DownLeft => .{ -1, 1 },
            .Left => .{ -1, 0 },
            .LeftUp => .{ -1, -1 },
        };
    }
};

pub const Directions8 = [_]Direction{
    Direction.Up,
    Direction.UpRight,
    Direction.Right,
    Direction.RightDown,
    Direction.Down,
    Direction.DownLeft,
    Direction.Left,
    Direction.LeftUp,
};

pub const Directions4 = [_]Direction{
    Direction.Up,
    Direction.Right,
    Direction.Down,
    Direction.Left,
};

pub fn NeighborIterator(comptime T: type) type {
    return struct {
        grid: *const Grid(T),
        position: @Vector(2, isize),
        direction: Direction,
        count: usize,

        pub fn next(self: *@This()) ?T {
            if (self.count > 0) {
                const t = self.grid.at(self.position);
                self.position += self.direction.toVector();
                self.count -= 1;
                return t;
            } else {
                return null;
            }
        }
    };
}

pub fn Grid(comptime T: type) type {
    return struct {
        rows: std.ArrayList([]const T),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{ .rows = std.ArrayList([]const T).init(allocator), .allocator = allocator };
        }

        pub fn deinit(self: @This()) void {
            self.rows.deinit();
        }

        pub fn at(self: *const @This(), position: @Vector(2, isize)) ?T {
            if (position[1] < 0 or position[1] >= @as(isize, @intCast(self.rows.items.len))) {
                return null;
            }
            const row = self.rows.items[@intCast(position[1])];
            if (position[0] < 0 or position[0] >= @as(isize, @intCast(row.len))) {
                return null;
            }
            return row[@intCast(position[0])];
        }

        pub fn size(self: *const @This()) @Vector(2, usize) {
            return .{ self.rows.items[0].len, self.rows.items.len };
        }

        pub fn neighbors(self: *const @This(), position: @Vector(2, isize), direction: Direction, count: usize) NeighborIterator(T) {
            return .{
                .grid = self,
                .position = position,
                .direction = direction,
                .count = count,
            };
        }
    };
}
