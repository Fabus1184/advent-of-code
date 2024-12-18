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

    pub fn rotateRight90(self: @This()) Direction {
        return switch (self) {
            .Up => Direction.Right,
            .UpRight => Direction.RightDown,
            .Right => Direction.Down,
            .RightDown => Direction.DownLeft,
            .Down => Direction.Left,
            .DownLeft => Direction.LeftUp,
            .Left => Direction.Up,
            .LeftUp => Direction.UpRight,
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
                const t = self.grid.get(self.position);
                self.position += self.direction.toVector();
                self.count -= 1;
                return t;
            } else {
                return null;
            }
        }
    };
}

pub fn PositionIterator(comptime T: type) type {
    return struct {
        grid: *const Grid(T),
        position: @Vector(2, isize),

        pub fn next(self: *@This()) ?struct { position: @Vector(2, isize), element: T } {
            if (self.grid.get(self.position)) |t| {
                const pos = self.position;

                self.position[0] += 1;
                if (self.position[0] >= @as(isize, @intCast(self.grid.rows.items[0].len))) {
                    self.position[0] = 0;
                    self.position[1] += 1;
                }

                return .{ .position = pos, .element = t };
            } else {
                return null;
            }
        }
    };
}

pub fn Grid(comptime T: type) type {
    return struct {
        rows: std.ArrayList([]T),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{ .rows = std.ArrayList([]T).init(allocator), .allocator = allocator };
        }

        pub fn deinit(self: @This()) void {
            for (self.rows.items) |row| {
                self.allocator.free(row);
            }
            self.rows.deinit();
        }

        pub fn clone(self: *const @This()) !@This() {
            return .{
                .rows = try self.rows.clone(),
                .allocator = self.allocator,
            };
        }

        pub fn fill(self: *@This(), rows: usize, cols: usize, value: T) !void {
            self.rows.clearRetainingCapacity();

            for (0..rows) |_| {
                const row = try self.allocator.alloc(T, cols);
                for (0..cols) |i| {
                    row[i] = value;
                }

                try self.rows.append(row);
            }
        }

        pub fn get(self: *const @This(), position: @Vector(2, isize)) ?T {
            if (position[1] < 0 or position[1] >= @as(isize, @intCast(self.rows.items.len))) {
                return null;
            }
            const row = self.rows.items[@intCast(position[1])];
            if (position[0] < 0 or position[0] >= @as(isize, @intCast(row.len))) {
                return null;
            }
            return row[@intCast(position[0])];
        }

        pub fn at(self: *@This(), position: @Vector(2, isize)) ?*T {
            if (position[1] < 0 or position[1] >= @as(isize, @intCast(self.rows.items.len))) {
                return null;
            }
            const row = self.rows.items[@intCast(position[1])];
            if (position[0] < 0 or position[0] >= @as(isize, @intCast(row.len))) {
                return null;
            }
            return &row[@intCast(position[0])];
        }

        pub fn size(self: *const @This()) @Vector(2, usize) {
            return .{ self.rows.items[0].len, self.rows.items.len };
        }

        pub fn addRow(self: *@This(), row: []const T) !void {
            const r = try self.allocator.alloc(T, row.len);
            @memcpy(r, row);
            try self.rows.append(r);
        }

        pub fn neighbor(self: *const @This(), position: @Vector(2, isize), direction: Direction) ?T {
            return self.get(position + direction.toVector());
        }

        pub fn neighborsIn(self: *const @This(), position: @Vector(2, isize), direction: Direction, count: usize) NeighborIterator(T) {
            return .{
                .grid = self,
                .position = position,
                .direction = direction,
                .count = count,
            };
        }

        pub fn neighbors4(self: *const @This(), position: @Vector(2, isize)) [4]?T {
            var neighbors: [4]?T = undefined;

            for (Directions4, 0..) |direction, i| {
                neighbors[i] = self.get(position + direction.toVector());
            }

            return neighbors;
        }

        pub fn elements(self: *const @This()) PositionIterator(T) {
            return .{ .grid = self, .position = .{ 0, 0 } };
        }

        pub fn printDecimal(self: *const @This()) void {
            for (self.rows.items) |row| {
                for (row) |item| {
                    std.debug.print("{d}", .{item});
                }
                std.debug.print("\n", .{});
            }
        }

        pub fn printBool(self: *const @This(), comptime trueString: []const u8, comptime falseString: []const u8) void {
            for (self.rows.items) |row| {
                for (row) |item| {
                    if (item) {
                        std.debug.print(trueString, .{});
                    } else {
                        std.debug.print(falseString, .{});
                    }
                }
                std.debug.print("\n", .{});
            }
        }

        pub fn printCustom(self: *const @This(), comptime formatString: []const u8) void {
            for (self.rows.items) |row| {
                for (row) |item| {
                    std.debug.print(formatString, .{item});
                }
                std.debug.print("\n", .{});
            }
        }

        pub fn printOptionalAny(self: *const @This(), comptime formatStringPresent: []const u8, comptime formatStringAbsent: []const u8) void {
            for (self.rows.items) |row| {
                for (row) |item| {
                    if (item) |value| {
                        std.debug.print(formatStringPresent, .{value});
                    } else {
                        std.debug.print(formatStringAbsent, .{});
                    }
                }
                std.debug.print("\n", .{});
            }
        }
    };
}
