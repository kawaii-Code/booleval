const std = @import("std");
const io = std.io;
const print = std.debug.print;

const UnaryExpressionType = enum {
    Negation,
};

const BinaryExpressionType = enum {
    And,
    Or,
    Implication,
    Equivalance,
};

const ExpressionNode = union(enum) {
    variable_id: usize,
    unary_expression: UnaryExpressionType,
    binary_expression: BinaryExpressionType,
};

const ExpressionTree = struct {
    value: ExpressionNode,
    left: ?*const ExpressionTree,
    right: ?*const ExpressionTree,
};

const Token = union(enum) {
    lparen,
    rparen,
    ident: u8,
    u_not,
    b_arrow,
    b_eq,
    b_and,
    b_or,
};

const TokenList = std.ArrayList(Token);

const LexError = error{
    EndOfInput,
    UnknownSymbol,
};

const ParseError = error{
    Test,
};

fn lex(input: []u8, allocator: std.mem.Allocator) !TokenList {
    var tokens = TokenList.init(allocator);

    var position: usize = 0;
    while (position < input.len) {
        switch (input[position]) {
            '(' => try tokens.append(Token.lparen),
            ')' => try tokens.append(Token.rparen),
            '!' => try tokens.append(Token.u_not),
            '-' => {
                position += 1;
                if (position == input.len) {
                    return LexError.EndOfInput;
                }
                if (input[position] == '>') {
                    try tokens.append(Token.b_arrow);
                } else {
                    return LexError.UnknownSymbol;
                }
            },
            '&' => try tokens.append(Token.b_and),
            '|' => try tokens.append(Token.b_or),
            '~' => try tokens.append(Token.b_eq),
            'a'...'z' => |char| try tokens.append(Token{ .ident = char }),
            ' ', '\t' => {},
            else => return LexError.UnknownSymbol,
        }
        position += 1;
    }

    return tokens;
}

fn has_less_precedence(compare_against: ?Token, than: Token) bool {
    if (compare_against == null) {
        return true;
    }

    var initial = compare_against.?;

    switch (than) {
        .b_arrow, .b_eq => return initial != Token.b_arrow and initial != Token.b_eq,
        .b_or => return initial == Token.b_and or initial == Token.u_not,
        .b_and => return initial == Token.u_not,
        .u_not => return false,
        else => unreachable,
    }
}

fn find_last_operator(tokens: []Token) usize {
    var result: usize = 0;
    var result_token: ?Token = null;

    var depth: u32 = 0;
    var inside_paren = false;

    for (tokens, 0..) |token, index| {
        if (token == Token.ident) {
            continue;
        }

        if (token == Token.lparen) {
            depth += 1;
            inside_paren = true;
        } else if (token == Token.rparen) {
            depth -= 1;
            if (depth == 0) {
                inside_paren = false;
            }
        }

        if (inside_paren) {
            continue;
        }

        if (has_less_precedence(result_token, token)) {
            result_token = token;
            result = index;
        }
    }

    return result;
}

var current_variable_id: usize = 0;

fn parse(tokens: []Token, variables: *std.AutoHashMap(u8, usize)) !?ExpressionTree {
    var tokens_new = tokens;
    if (tokens_new[0] == Token.lparen and tokens_new[tokens_new.len - 1] == Token.rparen) {
        tokens_new = tokens_new[1 .. tokens_new.len - 1];
    }

    const position = find_last_operator(tokens_new);

    const left_tokens = tokens_new[0..position];
    const right_tokens = tokens_new[position + 1 ..];

    const current = tokens_new[position];

    switch (current) {
        .ident => |char| {
            if (variables.get(char) == null) {
                try variables.put(char, current_variable_id);
                current_variable_id += 1;
            }

            return ExpressionTree{
                .value = ExpressionNode{ .variable_id = variables.get(char).? },
                .left = null,
                .right = null,
            };
        },
        .u_not => return ExpressionTree{
            .value = ExpressionNode{ .unary_expression = UnaryExpressionType.Negation },
            .left = &(try parse(left_tokens, variables)).?,
            .right = null,
        },
        .b_eq, .b_or, .b_and, .b_arrow => return ExpressionTree{
            .value = ExpressionNode{
                .binary_expression = switch (current) {
                    .b_and => BinaryExpressionType.And,
                    .b_or => BinaryExpressionType.Or,
                    .b_eq => BinaryExpressionType.Equivalance,
                    .b_arrow => BinaryExpressionType.Implication,
                    else => unreachable,
                },
            },
            .left = &(try parse(left_tokens, variables)).?,
            .right = &(try parse(right_tokens, variables)).?,
        },
        else => return ParseError.Test,
    }
}

fn evaluate(tree: *const ExpressionTree, variables: []const bool) bool {
    switch (tree.value) {
        .variable_id => |id| return variables[id],
        .unary_expression => return !evaluate(tree.left.?, variables),
        .binary_expression => |binary_type| switch (binary_type) {
            .And => return evaluate(tree.left.?, variables) and evaluate(tree.right.?, variables),
            .Or => return evaluate(tree.left.?, variables) or evaluate(tree.right.?, variables),
            .Implication => return !evaluate(tree.left.?, variables) or evaluate(tree.right.?, variables),
            .Equivalance => return evaluate(tree.left.?, variables) == evaluate(tree.right.?, variables),
        },
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const stdin = io.getStdIn().reader();
    const stdout = io.getStdOut().writer();

    print("> ", .{});
    const input = try stdin.readUntilDelimiterAlloc(allocator, '\n', 1024);

    var tokens = try lex(input, allocator);
    const tokens_slice = try tokens.toOwnedSlice();
    defer allocator.free(tokens_slice);
    printList(tokens_slice);

    var map = std.AutoHashMap(u8, usize).init(allocator);
    defer map.deinit();

    const tree = try parse(tokens_slice, &map);

    const variable_values: [2]bool = .{ true, false };
    const value = evaluate(&tree.?, &variable_values);

    try stdout.print("Result = {}\n", .{value});
}

fn printList(collection: anytype) void {
    for (collection) |item| {
        print("{}\n", .{item});
    }
}
