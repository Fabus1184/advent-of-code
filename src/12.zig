const std = @import("std");

const grid = @import("grid.zig");

const Input = struct {
    grid: grid.Grid(u8),
    shapes: []const Shape,

    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        self.grid.deinit();
        for (self.shapes) |shape| {
            allocator.free(shape.area);
        }
        allocator.free(self.shapes);
    }
};

const Shape = struct {
    plant: u8,
    area: []const @Vector(2, isize),

    fn contains(self: *const @This(), position: @Vector(2, isize)) bool {
        for (self.area) |p| {
            if (p[0] == position[0] and p[1] == position[1]) {
                return true;
            }
        }
        return false;
    }
};

fn floodRegion(g: *grid.Grid(u8), position: @Vector(2, isize), visited: *std.DynamicBitSet, allocator: std.mem.Allocator) ![]@Vector(2, isize) {
    const plant = g.get(position).?;

    var area = std.ArrayList(@Vector(2, isize)).init(allocator);

    var stack = std.ArrayList(@Vector(2, isize)).init(allocator);
    defer stack.deinit();

    try stack.append(position);

    while (stack.popOrNull()) |p| {
        if (g.get(p) != plant or visited.isSet(g.index(p))) {
            continue;
        }

        visited.set(g.index(p));

        try area.append(p);

        for (grid.Directions4) |direction| {
            try stack.append(p + direction.toVector());
        }
    }

    return try area.toOwnedSlice();
}

fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Input {
    var g = grid.Grid(u8).init(allocator);

    var lines = std.mem.splitSequence(u8, std.mem.trimRight(u8, input, "\n"), "\n");
    while (lines.next()) |line| {
        try g.addRow(line);
    }

    var visited = try std.DynamicBitSet.initEmpty(allocator, g.size()[0] * g.size()[1]);
    defer visited.deinit();

    var shapes = std.ArrayList(Shape).init(allocator);
    defer shapes.deinit();

    var it = g.elements();
    while (it.next()) |e| {
        if (visited.isSet(g.index(e.position))) {
            continue;
        }

        const area = try floodRegion(&g, e.position, &visited, allocator);

        try shapes.append(.{
            .plant = e.element,
            .area = area,
        });
    }

    return .{
        .grid = g,
        .shapes = try shapes.toOwnedSlice(),
    };
}

pub fn part1(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const g = try parseInput(input, allocator);
    defer g.deinit(allocator);

    var score: u64 = 0;

    for (g.shapes) |shape| {
        var perimeter: u64 = 0;

        for (shape.area) |position| {
            for (grid.Directions4) |direction| {
                if (g.grid.get(position + direction.toVector()) != shape.plant) {
                    perimeter += 1;
                }
            }
        }

        score += perimeter * shape.area.len;

        std.debug.print("Plant: {c}, Area: {}, Perimeter: {}\n", .{ shape.plant, shape.area.len, perimeter });
    }

    return score;
}

const Top = 0b10000000;
const TopRight = 0b01000000;
const Right = 0b00100000;
const BottomRight = 0b00010000;
const Bottom = 0b00001000;
const BottomLeft = 0b00000100;
const Left = 0b00000010;
const TopLeft = 0b00000001;

fn sidesFromBits(top: bool, topRight: bool, right: bool, bottomRight: bool, bottom: bool, bottomLeft: bool, left: bool, topLeft: bool) u8 {
    const value = @as(u8, @intFromBool(top)) << 7 | @as(u8, @intFromBool(topRight)) << 6 | @as(u8, @intFromBool(right)) << 5 | @as(u8, @intFromBool(bottomRight)) << 4 | @as(u8, @intFromBool(bottom)) << 3 | @as(u8, @intFromBool(bottomLeft)) << 2 | @as(u8, @intFromBool(left)) << 1 | @as(u8, @intFromBool(topLeft));
    return value;
}

pub fn part2(input: []const u8, allocator: std.mem.Allocator) !u64 {
    const g = try parseInput(input, allocator);
    defer g.deinit(allocator);

    var score: u64 = 0;

    for (g.shapes) |shape| {
        var corners: u64 = 0;

        for (0..g.grid.size()[0] + 2) |x| {
            for (0..g.grid.size()[1] + 2) |y| {
                const position = @Vector(2, isize){ @as(isize, @intCast(x)) - 1, @as(isize, @intCast(y)) - 1 };

                if (shape.contains(position)) {
                    continue;
                }

                const right = shape.contains(position + grid.Direction.Right.toVector());
                const rightDown = shape.contains(position + grid.Direction.RightDown.toVector());
                const down = shape.contains(position + grid.Direction.Down.toVector());
                const leftDown = shape.contains(position + grid.Direction.DownLeft.toVector());
                const left = shape.contains(position + grid.Direction.Left.toVector());
                const leftUp = shape.contains(position + grid.Direction.LeftUp.toVector());
                const up = shape.contains(position + grid.Direction.Up.toVector());
                const rightUp = shape.contains(position + grid.Direction.UpRight.toVector());

                const sides = sidesFromBits(up, rightUp, right, rightDown, down, leftDown, left, leftUp);

                // top right corner
                if (sides & (Left | BottomLeft | Bottom) == BottomLeft) {
                    corners += 1;
                }
                // bottom right corner
                if (sides & (Left | TopLeft | Top) == TopLeft) {
                    corners += 1;
                }
                // bottom left corner
                if (sides & (Right | TopRight | Top) == TopRight) {
                    corners += 1;
                }
                // top left corner
                if (sides & (Right | BottomRight | Bottom) == BottomRight) {
                    corners += 1;
                }

                // top right inner corner
                if (sides & (Left | Bottom) == (Left | Bottom)) {
                    corners += 1;
                }
                // bottom right inner corner
                if (sides & (Left | Top) == (Left | Top)) {
                    corners += 1;
                }
                // bottom left inner corner
                if (sides & (Right | Top) == (Right | Top)) {
                    corners += 1;
                }
                // top left inner corner
                if (sides & (Right | Bottom) == (Right | Bottom)) {
                    corners += 1;
                }
            }
        }

        score += corners * shape.area.len;
    }

    return score;
}

test "part2" {
    const input = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE\n";
    try std.testing.expectEqual(1206, part2(input, std.testing.allocator));

    const input2 = "AAAAAA\nAAABBA\nAAABBA\nABBAAA\nABBAAA\nAAAAAA\n";
    try std.testing.expectEqual(368, part2(input2, std.testing.allocator));
}
