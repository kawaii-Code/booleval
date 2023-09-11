const std = @import("std");
const io = std.io;
const mem = std.mem;
const heap = std.heap;
const print = std.debug.print;

const ExpressionNode = union(enum) {
    variable: usize,
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
    ident: u8,
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

// This is clearly a crutch.
var current_variable_id: usize = 0;

pub fn main() !void {
    var arena = heap.ArenaAllocator.init(heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    var variables = std.AutoHashMap(u8, usize).init(allocator);

    print("> ", .{});
    const input = try readLine(allocator);
    const tokens = try lex(allocator, input);
    const tree = try parse(allocator, tokens.items, &variables);

    var variable_values = try allocator.alloc(bool, variables.count());
    try printTruthTable(tree, variable_values, variables.count());
}

fn lex(allocator: mem.Allocator, input: []u8) !std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);

    var position: usize = 0;
    while (position < input.len) {
        switch (input[position]) {
            '(' => try tokens.append(Token.lparen),
            ')' => try tokens.append(Token.rparen),
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
            '|' => try tokens.append(Token.pipe),
            '~' => try tokens.append(Token.tilde),
            '^' => try tokens.append(Token.cap),
            'a'...'z' => |char| try tokens.append(Token{ .ident = char }),
            ' ', '\t' => {},
            else => return LexError.InvalidCharacter,
        }
        position += 1;
    }

    return tokens;
}

fn parse(allocator: mem.Allocator, tokens: []Token, variables: *std.AutoHashMap(u8, usize)) !*const ExpressionNode {
    var trimmed_tokens = trimOuterParentheses(tokens);

    const position = findLeastPrecedenceNodePosition(trimmed_tokens);
    const least_precedence_token = trimmed_tokens[position];

    const left_tokens = trimmed_tokens[0..position];
    const right_tokens = trimmed_tokens[position + 1 ..];

    switch (least_precedence_token) {
        .ident => |char| {
            if (variables.get(char) == null) {
                try variables.put(char, current_variable_id);
                current_variable_id += 1;
            }

            const node = try allocator.create(ExpressionNode);
            node.* = ExpressionNode{ .variable = variables.get(char).? };
            return node;
        },
        .bang => {
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
        .tilde, .pipe, .ampersand, .arrow, .cap => {
            const @"type" = switch (least_precedence_token) {
                .ampersand => BinaryExpressionType.And,
                .pipe => BinaryExpressionType.Or,
                .cap => BinaryExpressionType.Xor,
                .arrow => BinaryExpressionType.Implication,
                .tilde => BinaryExpressionType.Equivalance,
                else => unreachable,
            };

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

fn trimOuterParentheses(tokens: []Token) []Token {
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

fn findLeastPrecedenceNodePosition(tokens: []Token) usize {
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

fn hasLessPrecedence(token: Token, maybe_compare_with: ?Token) bool {
    if (maybe_compare_with == null) {
        return true;
    }

    var compare_with = maybe_compare_with.?;

    switch (token) {
        .arrow, .tilde => return true,
        .pipe, .cap => return compare_with != Token.arrow or compare_with != Token.tilde,
        .ampersand => return compare_with == Token.ampersand or compare_with == Token.bang,
        .bang => return compare_with == Token.bang,
        else => unreachable,
    }
}

fn printTruthTable(tree: *const ExpressionNode, variables: []bool, variable_count: usize) !void {
    try evaluateRecursively(tree, variables, variable_count, 0);
}

fn evaluateRecursively(tree: *const ExpressionNode, variables: []bool, variable_count: usize, current_variable: usize) !void {
    if (current_variable >= variable_count) {
        return;
    }

    variables[current_variable] = false;
    try evaluateRecursively(tree, variables, variable_count, current_variable + 1);

    const result = evaluate(tree, variables);
    printTruthTableRow(variables, result);

    variables[current_variable] = true;
    try evaluateRecursively(tree, variables, variable_count, current_variable + 1);

    if (current_variable == 0) {
        const result_all_true = evaluate(tree, variables);
        printTruthTableRow(variables, result_all_true);
    }
}

fn evaluate(tree: *const ExpressionNode, variables: []const bool) bool {
    switch (tree.*) {
        .variable => |id| return variables[id],
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

fn printTruthTableRow(variables: []bool, result: bool) void {
    var num: u8 = 0;
    for (variables) |variable| {
        num = if (variable) 1 else 0;
        print("{} ", .{num});
    }
    num = if (result) 1 else 0;
    print("= {}\n", .{num});
}

fn readLine(allocator: mem.Allocator) ![]u8 {
    const stdin = io.getStdIn().reader();
    var input = std.ArrayList(u8).init(allocator);
    try stdin.streamUntilDelimiter(input.writer(), '\n', null);
    return try input.toOwnedSlice();
}
