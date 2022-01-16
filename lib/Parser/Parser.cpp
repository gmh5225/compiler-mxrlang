#include "Parser.h"

using namespace mxrlang;

// If the next token matches the expected, advance the token stream.
bool Parser::match(TokenKind kind) {
    if (check(kind)) {
        advance();
        return true;
    }

    return false;
}

// Whether the next token matches the expected.
bool Parser::check(TokenKind kind) {
    if (isAtEnd())
        return false;
    return peek().getKind() == kind;
}

// Advance the token stream.
Token& Parser::advance() {
    if (!isAtEnd())
        current++;
    return previous();
}

// Whether the current token signalizes the end of the token stream.
bool Parser::isAtEnd() {
    return peek().getKind() == TokenKind::eof;
}

// Return the next token, but don't advance the stream.
Token& Parser::peek() {
    return *current;
}

// Return the previous token.
Token& Parser::previous() {
    TokenIterator help = --current;
    current++;

    return *help;
}

// Check whether the next token matches the expected and advance the stream
// if it does. Conversely, throw an error.
Token& Parser::consume(DiagID diagID, TokenKind kind) {
    if (check(kind))
        return advance();

    throw error(peek(), diagID);
}

template <typename... Ts>
Token& Parser::consume(DiagID diagID, TokenKind kind, Ts... kinds) {
    if (check(kind))
        return advance();

    return consume(diagID, kinds...);
}

// Discard the (possibly) erroneous tokens until we see one of the
// synchronization tokens. Called after the parser reports an error.
void Parser::synchronize() {
    advance();

    while (!isAtEnd()) {
        // Synchronize at the end of statement.
        if (previous().getKind() == TokenKind::semicolon)
            return;

        switch (peek().getKind()) {
        default:
            ;
        }

        advance();
    }
}

// Report an error and throw an exception.
Parser::ParserError Parser::error(const Token& tok, DiagID id) {
    diag.report(tok.getLocation(), id);
    return ParserError();
}

FunStmt* Parser::funDeclaration() {
    Stmts stmts;
    Token& funToken = peek();
    // For now, just parse the whole file, because the whole file is a
    // singular function.
    while (!isAtEnd())
        stmts.emplace_back(declaration());

    return new FunStmt("main", std::move(stmts),
                       funToken.getLocation());
}

Stmt* Parser::declaration() {
    try {
        if (match(TokenKind::kw_VAR))
            return varDeclaration();
        return statement();
    } catch (ParserError& e) {
        synchronize();
        return nullptr;
    }
}

Stmt* Parser::statement() {
    if (match(TokenKind::kw_RETURN))
        return returnStmt();
    else
        return exprStmt();
}

Stmt* Parser::returnStmt() {
    Expr* retExpr = nullptr;

    if (!check(TokenKind::semicolon))
        retExpr = expression();

    consume(DiagID::err_expect_semicol, TokenKind::semicolon);
    return new ReturnStmt(retExpr, previous().getLocation());
}

Stmt* Parser::varDeclaration() {
    // Must consume a type.
    const Token& typeTok = consume(DiagID::err_expect_type,
                                   TokenKind::kw_INT, TokenKind::kw_BOOL);
    Type* varType = Type::getTypeFromToken(typeTok);

    const Token& name = consume(DiagID::err_expect_var_name,
                                TokenKind::identifier);

    Expr* initializer = nullptr;
    if (match(TokenKind::colonequal))
        initializer = expression();

    consume(DiagID::err_expect_semicol, TokenKind::semicolon);
    return new VarStmt(name.getIdentifier(), initializer, varType,
                       name.getLocation());
}

Stmt* Parser::exprStmt() {
    Expr* expr = expression();
    consume(DiagID::err_expect_semicol, TokenKind::semicolon);

    return new ExprStmt(expr, previous().getLocation());
}

Expr* Parser::expression() {
    return assignment();
}

Expr* Parser::assignment() {
    auto* expr = primary();

    if (match(TokenKind::colonequal)) {
        auto* source = expression();

        auto* assign = expr->makeAssignExpr(source);
        if (assign)
            return assign;
        throw error(peek(), DiagID::err_invalid_assign_target);
    }

    return expr;
}

Expr* Parser::primary() {
    if (match(TokenKind::kw_TRUE) ||
        match(TokenKind::kw_FALSE)) {
        bool value =
                previous().getKind() == TokenKind::kw_TRUE ? true : false;
        return new BoolLiteralExpr(value, previous().getLocation());
    } if (match(TokenKind::integer_literal))
        return new IntLiteralExpr(previous().getLiteralData(),
                                  previous().getLocation());
    if (match(TokenKind::identifier))
        return new VarExpr(previous().getIdentifier(),
                           previous().getLocation());

    throw error(peek(), DiagID::err_expect_expr);
}

// Parse the token stream and return the root of the AST.
ModuleStmt* Parser::parse() {
    // Currently, only a single module exists, which contains a single
    // implicit "main" function.
    Token& moduleToken = peek();
    Stmts stmts{funDeclaration()};

    ModuleStmt* moduleStmt = new ModuleStmt("main", std::move(stmts),
                                            moduleToken.getLocation());
    return moduleStmt;
}
