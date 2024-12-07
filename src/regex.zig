const std = @import("std");

const pcre = @cImport({
    @cInclude("pcre.h");
});

pub const Match = struct {
    start: usize,
    end: usize,
    groups: []const []const u8,
};

pub const Matches = struct {
    matches: []const Match,
    index: usize,
    allocator: std.mem.Allocator,

    pub fn next(self: *@This()) ?Match {
        if (self.index >= self.matches.len) {
            return null;
        }

        const match = self.matches[self.index];
        self.index += 1;
        return match;
    }

    pub fn deinit(self: Matches) void {
        for (self.matches) |match| {
            self.allocator.free(match.groups);
        }
        self.allocator.free(self.matches);
    }
};

pub const Regex = struct {
    re: *pcre.pcre,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, pattern: [:0]const u8) !@This() {
        var err_ptr: [*c]const u8 = undefined;
        var err_offset: c_int = 0;

        const re = pcre.pcre_compile(pattern.ptr, 0, &err_ptr, &err_offset, null);
        if (re == null) {
            std.debug.panic("Failed to compile regex: {s}\n", .{err_ptr});
        }

        return .{ .re = re.?, .allocator = allocator };
    }

    pub fn deinit(self: @This()) void {
        pcre.pcre_free.?(self.re);
    }

    pub fn match(self: *const @This(), input: []const u8) !Matches {
        var capture_count: c_int = undefined;
        if (pcre.pcre_fullinfo(self.re, null, pcre.PCRE_INFO_CAPTURECOUNT, &capture_count) != 0) {
            std.debug.panic("Failed to get capture count\n", .{});
        }

        const input_c: [:0]u8 = try self.allocator.allocSentinel(u8, input.len, 0);
        defer self.allocator.free(input_c);
        @memcpy(input_c, input.ptr);

        var start_offset: usize = 0;
        const ovector = try self.allocator.alloc(c_int, (@as(usize, @intCast(capture_count)) + 1) * 3);
        defer self.allocator.free(ovector);

        var matches = std.ArrayList(Match).init(self.allocator);

        while (start_offset < input_c.len) {
            const rc = pcre.pcre_exec(self.re, null, input_c.ptr, @intCast(input_c.len), @intCast(start_offset), 0, ovector.ptr, @intCast(ovector.len));
            if (rc < 0) {
                if (rc == pcre.PCRE_ERROR_NOMATCH) {
                    break;
                } else {
                    std.debug.panic("Failed to match regex: {d}\n", .{rc});
                }
            }

            var groups = try self.allocator.alloc([]const u8, @as(usize, @intCast(capture_count)) + 1);
            for (0..@as(usize, @intCast(capture_count)) + 1) |i| {
                if (ovector[i * 2] == -1) {
                    groups[i] = input[0..0];
                } else {
                    groups[i] = input[@intCast(ovector[i * 2])..@intCast(ovector[i * 2 + 1])];
                }
            }

            try matches.append(.{ .start = @intCast(ovector[0]), .end = @intCast(ovector[1]), .groups = groups });

            start_offset = @intCast(ovector[1]);
        }

        return Matches{ .matches = try matches.toOwnedSlice(), .index = 0, .allocator = self.allocator };
    }
};
