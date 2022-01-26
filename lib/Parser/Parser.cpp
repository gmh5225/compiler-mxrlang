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
        case TokenKind::kw_ELSE:
        case TokenKind::kw_IF:
        case TokenKind::kw_PRINT:
        case TokenKind::kw_THEN:
        case TokenKind::kw_VAR:
            return;
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
    // For now, just parse the whole file, beause the whole file is a
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
    if (match(TokenKind::kw_IF))
        return ifStmt();
    else if (match(TokenKind::kw_PRINT))
        return printStmt();
    else if (match(TokenKind::kw_RETURN))
        return returnStmt();
    else
        return exprStmt();
}

Stmt* Parser::ifStmt() {
    // Parse the IF condition.
    Stmts thenStmts;
    Stmts elseStmts;
    Expr* cond = expression();

    consume(DiagID::err_expect_then, TokenKind::kw_THEN);

    // Parse the statements in the THEN block.
    while (!(match(TokenKind::kw_ELSE) || match(TokenKind::kw_FI)))
        thenStmts.push_back(declaration());

    // Parse the statements in the ELSE block.
    if (previous().getKind() == TokenKind::kw_ELSE) {
        while (!match(TokenKind::kw_FI)) {
            elseStmts.push_back(declaration());
        }
    }

    return new IfStmt(cond, std::move(thenStmts), std::move(elseStmts),
                      previous().getLocation());
}

Stmt* Parser::printStmt() {
    Expr* printExpr = expression();

    consume(DiagID::err_expect_semicol, TokenKind::semicolon);
    return new PrintStmt(printExpr, previous().getLocation());
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
    auto* expr = logicalOr();

    if (match(TokenKind::colonequal)) {
        auto* source = expression();

        auto* assign = expr->makeAssignExpr(source);
        if (assign)
            return assign;
        throw error(peek(), DiagID::err_invalid_assign_target);
    }

    return expr;
}

Expr* Parser::logicalOr() {
    auto* expr = logicalAnd();

    while (match(TokenKind::logicor)) {
        auto opString = std::string(previous().getData());
        auto* right = logicalAnd();
        expr = new BinaryLogicalExpr(
            BinaryLogicalExpr::BinaryLogicalExprKind::Or, expr, right,
            opString, previous().getLocation());
    }

    return expr;
}

Expr* Parser::logicalAnd() {
    auto* expr = equality();

    while (match(TokenKind::logicand)) {
        auto opString = std::string(previous().getData());
        auto* right = equality();
        expr = new BinaryLogicalExpr(
            BinaryLogicalExpr::BinaryLogicalExprKind::And, expr, right,
            opString, previous().getLocation());
    }

    return expr;
}

Expr* Parser::equality() {
    auto* expr = comparison();

    while (match(TokenKind::equal) || match(TokenKind::noteq)) {
        auto opString = std::string(previous().getData());
        auto kind = previous().getKind() == TokenKind::equal ?
            BinaryLogicalExpr::BinaryLogicalExprKind::Eq :
            BinaryLogicalExpr::BinaryLogicalExprKind::NotEq;

        auto* right = comparison();
        expr = new BinaryLogicalExpr(kind, expr, right, opString,
                                     previous().getLocation());
    }

    return expr;
}

Expr* Parser::comparison() {
    auto* expr = addSub();

    while (match(TokenKind::greater) ||
           match(TokenKind::greatereq) ||
           match(TokenKind::less) ||
           match(TokenKind::lesseq)) {
        auto opString = std::string(previous().getData());
        BinaryLogicalExpr::BinaryLogicalExprKind kind;
        switch (previous().getKind()) {
        case TokenKind::greater:
            kind = BinaryLogicalExpr::BinaryLogicalExprKind::Greater;
            break;
        case TokenKind::greatereq:
            kind = BinaryLogicalExpr::BinaryLogicalExprKind::GreaterEq;
            break;
        case TokenKind::less:
            kind = BinaryLogicalExpr::BinaryLogicalExprKind::Less;
            break;
        case TokenKind::lesseq:
            kind = BinaryLogicalExpr::BinaryLogicalExprKind::LessEq;
            break;
        default:
            llvm_unreachable("Wrong binary logical operator.");
        }

        auto* right = addSub();
        expr = new BinaryLogicalExpr(kind, expr, right, opString,
                                     previous().getLocation());
    }

    return expr;
}

Expr* Parser::addSub() {
    auto* expr = mulDiv();

    while (match(TokenKind::plus) || match(TokenKind::minus)) {
        auto opString = std::string(previous().getData());
        auto kind = previous().getKind() == TokenKind::plus ?
            BinaryArithExpr::BinaryArithExprKind::Add :
            BinaryArithExpr::BinaryArithExprKind::Sub;

        auto* right = mulDiv();
        expr = new BinaryArithExpr(kind, expr, right, opString,
                                   previous().getLocation());
    }

    return expr;
}

Expr* Parser::mulDiv() {
    auto* expr = unary();

    while (match(TokenKind::star) || match(TokenKind::slash)) {
        auto opString = std::string(previous().getData());
        auto kind = previous().getKind() == TokenKind::star ?
            BinaryArithExpr::BinaryArithExprKind::Mul :
            BinaryArithExpr::BinaryArithExprKind::Div;

        auto* right = unary();
        expr = new BinaryArithExpr(kind, expr, right, opString,
                                   previous().getLocation());
    }

    return expr;
}

Expr* Parser::unary() {
    if (match(TokenKind::bang) || match(TokenKind::minus)) {
        auto opString = std::string(previous().getData());
        auto kind = previous().getKind() == TokenKind::bang ?
            UnaryExpr::UnaryExprKind::NegLogic :
            UnaryExpr::UnaryExprKind::NegArith;

        auto* expr = primary();
        return new UnaryExpr(kind, expr, opString,
                             previous().getLocation());
    }

    return primary();
}

Expr* Parser::primary() {
    if (match(TokenKind::kw_TRUE) ||
        match(TokenKind::kw_FALSE)) {
        bool value =
                previous().getKind() == TokenKind::kw_TRUE ? true : false;
        return new BoolLiteralExpr(value, previous().getLocation());
    } else if (match(TokenKind::openpar)) {
        auto* expr = expression();
        consume(DiagID::err_expect_closedpar, TokenKind::closedpar);
        return new GroupingExpr(expr, previous().getLocation());
    } else if (match(TokenKind::integer_literal))
        return new IntLiteralExpr(previous().getLiteralData(),
                                  previous().getLocation());
    else if (match(TokenKind::identifier))
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
