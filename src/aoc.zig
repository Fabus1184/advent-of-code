const std = @import("std");

const CACHE_DIR = "aoc-cache";

pub const AocClient = struct {
    year: u32,
    token: []const u8,
    client: std.http.Client,
    allocator: std.mem.Allocator,
    inputs: std.AutoArrayHashMap(u8, []const u8),

    pub fn init(allocator: std.mem.Allocator, token: []const u8, year: u32) !AocClient {
        var inputs = std.AutoArrayHashMap(u8, []const u8).init(allocator);

        const dir = try std.fs.cwd().openDir(CACHE_DIR, .{
            .iterate = true,
        });
        var walk = try dir.walk(allocator);
        defer walk.deinit();

        while (try walk.next()) |entry| {
            const day = try std.fmt.parseInt(u8, entry.basename, 10);
            const input = try dir.readFileAlloc(allocator, entry.path, std.math.maxInt(usize));

            try inputs.put(day, input);
        }

        return AocClient{
            .token = token,
            .client = std.http.Client{ .allocator = allocator },
            .year = year,
            .inputs = inputs,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *AocClient) !void {
        std.fs.cwd().makeDir(CACHE_DIR) catch {};

        const dir = try std.fs.cwd().openDir(CACHE_DIR, .{
            .iterate = true,
        });

        var iter = self.inputs.iterator();
        while (iter.next()) |entry| {
            const path = try std.fmt.allocPrint(self.allocator, "{d}", .{entry.key_ptr.*});
            defer self.allocator.free(path);

            const data: []const u8 = entry.value_ptr.*;

            try dir.writeFile(.{ .sub_path = path, .data = data });
        }

        self.inputs.deinit();
    }

    pub fn getProblemInput(self: *@This(), day: u8) ![]const u8 {
        if (self.inputs.get(day)) |input| {
            return input;
        } else {
            const input = try self.fetchProblemInput(day);
            try self.inputs.put(day, input);
            return input;
        }
    }

    fn fetchProblemInput(self: *@This(), day: u8) ![]const u8 {
        const url = try std.fmt.allocPrint(self.allocator, "https://adventofcode.com/{d}/day/{d}/input", .{ self.year, day });
        defer self.allocator.free(url);

        const uri = try std.Uri.parse(url);

        const session = try std.fmt.allocPrint(self.allocator, "session={s}", .{self.token});
        defer self.allocator.free(session);

        var server_header_buffer = [_]u8{0} ** 1024;

        var request = try self.client.open(.GET, uri, .{
            .extra_headers = &.{.{
                .name = "Cookie",
                .value = session,
            }},
            .server_header_buffer = &server_header_buffer,
        });
        defer request.deinit();

        try request.send();
        try request.finish();
        try request.wait();

        if (request.response.status.class() != .success) {
            std.debug.panic("Failed to get input: {}\n", .{request.response.status});
        }

        var reader = request.reader();
        const body = try reader.readAllAlloc(self.allocator, std.math.maxInt(usize));

        return body;
    }
};