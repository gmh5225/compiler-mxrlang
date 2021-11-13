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
Token& Parser::consume(TokenKind kind, DiagID diagID) {
    if (check(kind))
        return advance();

    throw error(peek(), diagID);
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
    // For now, just parse the whole file, because the whole file is a singular
    // function.
    while (!isAtEnd())
        stmts.emplace_back(declaration());

    return new FunStmt("main", std::move(stmts));
}

Stmt* Parser::declaration() {
    try {
        return varDeclaration();
    } catch (ParserError& e) {
        synchronize();
        return nullptr;
    }
}

Stmt* Parser::varDeclaration() {
    consume(TokenKind::kw_VAR, DiagID::err_expect_kw_var);
    const Token& name = consume(TokenKind::identifier,
                                DiagID::err_expect_var_name);

    Expr* initializer = nullptr;
    if (match(TokenKind::colonequal))
        initializer = expression();

    consume(TokenKind::semicolon, DiagID::err_expect_semicol);
    return new VarStmt(name.getIdentifier(), initializer);
}

Expr* Parser::expression() {
    return primary();
}

Expr* Parser::primary() {
    if (match(TokenKind::integer_literal))
        return new LiteralExpr(previous().getLiteralData());

    throw error(peek(), DiagID::err_expect_expr);
}

// Parse the token stream and return the root of the AST.
ModuleStmt* Parser::parse() {
    // Currently, only a single module exists, which contains a single implicit
    // "main" funcion.
    Stmts stmts{funDeclaration()};

    ModuleStmt* moduleStmt = new ModuleStmt("main", std::move(stmts));
    return moduleStmt;
}
