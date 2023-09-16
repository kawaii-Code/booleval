const std = @import("std");

const io = std.io;
const mem = std.mem;
const math = std.math;
const heap = std.heap;
const print = std.debug.print;

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const AutoArrayHashMap = std.AutoArrayHashMap;

const ExpressionNode = union(enum) {
    variable: u8,
    unary_expression: UnaryExpression,
    binary_expression: BinaryExpression,
};

const UnaryExpression = struct {
    type: UnaryExpressionType,
    operand: *const ExpressionNode,
};

const BinaryExpression = struct {
    type: BinaryExpressionType,
    left: *const ExpressionNode,
    right: *const ExpressionNode,
};

const UnaryExpressionType = enum {
    Negation,
};

const BinaryExpressionType = enum {
    And,
    Or,
    Xor,
    Implication,
    Equivalance,
};

const Token = union(enum) {
    lparen,
    rparen,
    comma,
    ident: u8,
    tee, // Funny name for a |- (also called turntail)
    bang,
    arrow,
    tilde,
    ampersand,
    pipe,
    cap,
};

const LexError = error{
    TrailingCharacter,
    InvalidCharacter,
};

const ParseError = error{
    UnexpectedToken,
};

pub fn main() !void {
    var arena = heap.ArenaAllocator.init(heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var variables = AutoArrayHashMap(u8, bool).init(allocator);

    print("> ", .{});
    const input = try readLine(allocator);

    const tokens = try lex(allocator, input);
    const tree = try parse(allocator, tokens.items, &variables);
    try printTruthTable(tree, &variables);
}

fn lex(allocator: Allocator, input: []u8) !ArrayList(Token) {
    var tokens = ArrayList(Token).init(allocator);

    var position: usize = 0;
    while (position < input.len) {
        switch (input[position]) {
            '(' => try tokens.append(Token.lparen),
            ')' => try tokens.append(Token.rparen),
            ',' => try tokens.append(Token.comma),
            '!' => try tokens.append(Token.bang),
            '-' => {
                position += 1;
                if (position == input.len) {
                    return LexError.TrailingCharacter;
                }
                if (input[position] == '>') {
                    try tokens.append(Token.arrow);
                } else {
                    return LexError.InvalidCharacter;
                }
            },
            '&' => try tokens.append(Token.ampersand),
            '|' => {
                if (position + 1 == input.len) {
                    return LexError.TrailingCharacter;
                }

                if (input[position + 1] == '-') {
                    position += 1;
                    try tokens.append(Token.tee);
                } else {
                    try tokens.append(Token.pipe);
                }
            },
            '~' => try tokens.append(Token.tilde),
            '^' => try tokens.append(Token.cap),
            'a'...'z' => |char| try tokens.append(Token{ .ident = char }),
            ' ', '\t', '\r', '\n' => {},
            else => return LexError.InvalidCharacter,
        }

        position += 1;
    }

    return tokens;
}

fn parse(allocator: Allocator, tokens: []const Token, variables: *AutoArrayHashMap(u8, bool)) !*const ExpressionNode {
    var trimmed_tokens = trimOuterParentheses(tokens);

    const position = findLeastPrecedenceNodePosition(trimmed_tokens);
    const least_precedence_token = trimmed_tokens[position];

    switch (least_precedence_token) {
        .ident => |char| {
            if (variables.get(char) == null) {
                try variables.put(char, false);
            }

            const node = try allocator.create(ExpressionNode);
            node.* = ExpressionNode{ .variable = char };
            return node;
        },
        .bang => {
            const right_tokens = tokens[position + 1..];
            const operand = try parse(allocator, right_tokens, variables);

            const node = try allocator.create(ExpressionNode);
            node.* = ExpressionNode{
                .unary_expression = UnaryExpression{
                    .type = UnaryExpressionType.Negation,
                    .operand = operand,
                },
            };
            return node;
        },
        .tilde, .pipe, .ampersand, .arrow, .cap, .tee, .comma => {
            const @"type" = switch (least_precedence_token) {
                .ampersand => BinaryExpressionType.And,
                .pipe => BinaryExpressionType.Or,
                .cap => BinaryExpressionType.Xor,
                .arrow, .tee, .comma => BinaryExpressionType.Implication,
                .tilde => BinaryExpressionType.Equivalance,
                else => unreachable,
            };

            const left_tokens = trimmed_tokens[0..position];
            const right_tokens = trimmed_tokens[position + 1 ..];
            const left = try parse(allocator, left_tokens, variables);
            const right = try parse(allocator, right_tokens, variables);

            const node = try allocator.create(ExpressionNode);
            node.* = ExpressionNode{
                .binary_expression = BinaryExpression{
                    .type = @"type",
                    .left = left,
                    .right = right,
                },
            };
            return node;
        },
        else => return ParseError.UnexpectedToken,
    }
}

fn trimOuterParentheses(tokens: []const Token) []const Token {
    if (tokens[0] != Token.lparen or tokens[tokens.len - 1] != Token.rparen) {
        return tokens;
    }

    var paren_depth: i32 = 0;
    for (tokens[1 .. tokens.len - 1]) |token| {
        if (token == Token.lparen) {
            paren_depth += 1;
        } else if (token == Token.rparen) {
            paren_depth -= 1;
            if (paren_depth < 0) {
                return tokens;
            }
        }
    }

    if (paren_depth == 0) {
        return tokens[1 .. tokens.len - 1];
    }

    return tokens;
}

fn findLeastPrecedenceNodePosition(tokens: []const Token) usize {
    var least_precedence_pos: usize = 0;
    var least_precedence_token: ?Token = null;

    var paren_depth: u32 = 0;
    var inside_paren = false;

    for (tokens, 0..) |token, index| {
        if (token == Token.ident) {
            continue;
        }

        if (token == Token.lparen) {
            paren_depth += 1;
            inside_paren = true;
        } else if (token == Token.rparen) {
            paren_depth -= 1;
            if (paren_depth == 0) {
                inside_paren = false;
                continue;
            }
        }

        if (inside_paren) {
            continue;
        }

        if (hasLessPrecedence(token, least_precedence_token)) {
            least_precedence_token = token;
            least_precedence_pos = index;
        }
    }

    return least_precedence_pos;
}

fn hasLessPrecedence(token: Token, maybe_other: ?Token) bool {
    if (maybe_other == null) {
        return true;
    }

    var other = maybe_other.?;

    switch (token) {
        .comma => return other != Token.comma,
        .tee => return other != Token.comma,
        .arrow, .tilde => return other != Token.tee and other != Token.comma,
        .pipe, .cap => return other == Token.pipe or other == Token.cap or
                              other == Token.ampersand or other == Token.bang,
        .ampersand => return other == Token.ampersand or other == Token.bang,
        .bang => return other == Token.bang,
        else => unreachable,
    }
}

fn printTruthTable(tree: *const ExpressionNode, variables: *AutoArrayHashMap(u8, bool)) !void {
    const count = variables.count();
    const powerOfTwo = math.pow(usize, 2, count);
    const one: usize = 1; // ...

    for (0..powerOfTwo) |i| {
        for (variables.keys(), 0..) |key, j| {
            // What the fuck is going on with bit manipulation
            const a: u6 = @truncate(j);
            const value = (i & (one << a)) != 0;
            try variables.put(key, value);
        }

        const result = evaluate(tree, variables);
        printTruthTableRow(variables, result);
    }
}

fn evaluate(tree: *const ExpressionNode, variables: *AutoArrayHashMap(u8, bool)) bool {
    switch (tree.*) {
        .variable => |char| return variables.get(char).?,
        .unary_expression => |unary| switch (unary.type) {
            .Negation => return !evaluate(unary.operand, variables),
        },
        .binary_expression => |binary| switch (binary.type) {
            .And => return evaluate(binary.left, variables) and evaluate(binary.right, variables),
            .Or => return evaluate(binary.left, variables) or evaluate(binary.right, variables),
            .Xor => return evaluate(binary.left, variables) != evaluate(binary.right, variables),
            .Implication => return !evaluate(binary.left, variables) or evaluate(binary.right, variables),
            .Equivalance => return evaluate(binary.left, variables) == evaluate(binary.right, variables),
        },
    }
}

fn printTruthTableRow(variables: *AutoArrayHashMap(u8, bool), result: bool) void {
    var num: u8 = 0;
    for (variables.keys()) |variable| {
        num = if (variables.get(variable).?) 1 else 0;
        print("{c} = {}, ", .{variable, num});
    }
    num = if (result) 1 else 0;
    print("= {}\n", .{num});
}

fn readLine(allocator: Allocator) ![]u8 {
    const stdin = io.getStdIn().reader();
    var input = ArrayList(u8).init(allocator);
    try stdin.streamUntilDelimiter(input.writer(), '\n', null);
    return try input.toOwnedSlice();
}
