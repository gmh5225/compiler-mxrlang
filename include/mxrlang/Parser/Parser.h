#ifndef PARSER_H
#define PARSER_H

#include "Diag.h"
#include "Token.h"
#include "Tree.h"

namespace mxrlang {

class Parser {
    using TokenIterator = Tokens::iterator;

    // Custom parser exception class.
    class ParserError : public std::exception {};

    // Stream of tokens acquired from the lexer.
    Tokens& tokens;
    // Currently processed token.
    TokenIterator current;

    // Whether we have already seen an assignment in the current
    // expression statement parse.
    bool seenAssignment;

    Diag& diag;

    // If the next token matches the expected, advance the token stream.
    bool match(TokenKind kind);
    // Whether the next token matches the expected.
    bool check(TokenKind kind);
    // Advance the token stream.
    Token& advance();
    // Whether the current token signalizes the end of the token stream.
    bool isAtEnd();
    // Return the next token, but don't advance the stream.
    Token& peek();
    // Return the previous token.
    Token& previous();
    // Check whether the next token matches the expected and advance the stream
    // if it does. Conversely, throw an error.
    Token& consume(DiagID diagID, TokenKind kind);

    template <typename... Ts>
    Token& consume(DiagID diagID, TokenKind kind, Ts... kinds);

    // Discard the (possibly) erroneous tokens until we see one of the
    // synchronization tokens. Called after the parser reports an error.
    void synchronize();

    // Report an error and throw an exception.
    ParserError error(const Token& tok, DiagID diagID);

    // Productions.
    FunStmt* funDeclaration();
    Stmt* declaration();
    Stmt* exprStmt();
    Stmt* returnStmt();
    Stmt* statement();
    Stmt* varDeclaration();

    Expr* expression();
    Expr* assignment();
    Expr* primary();

public:
    Parser(Tokens& tokens, Diag& diag)
        : tokens(tokens), current(tokens.begin()), diag(diag) {}

    // Parse the token stream and return the root of the AST.
    ModuleStmt* parse();
};

}

#endif // PARSER_H
