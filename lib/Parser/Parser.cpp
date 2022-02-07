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
Token& Parser::consume(std::initializer_list<TokenKind> kinds,
                       DiagID diagID, std::string args...) {
    for (auto kind = kinds.begin(); kind != kinds.end(); kind++) {
        if (check(*kind))
            return advance();
    }

    auto perror = error(peek(), diagID, std::move(args));
    throw perror;
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
        case TokenKind::kw_FI:
        case TokenKind::kw_FUN:
        case TokenKind::kw_IF:
        case TokenKind::kw_NUF:
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
Parser::ParserError Parser::error(const Token& tok, DiagID id,
                                  std::string args...) {
    diag.report(tok.getLocation(), id, args);
    auto perror = ParserError();
    return perror;
}

// Helper which creates a VarStmt while parsing variable declarations,
// or function declaration arguments.
VarStmt* Parser::parseSingleVar(bool isFunArg) {
    // Parse the variable name.
    const Token& name = consume({TokenKind::identifier}, DiagID::err_expect,
                                "identifer"s);

    consume({TokenKind::colon}, DiagID::err_expect, ":"s);

    // Parse the type.
    const Token& typeTok = consume({TokenKind::kw_INT, TokenKind::kw_BOOL},
                                   DiagID::err_expect, "type");
    Type* varType = Type::getTypeFromToken(typeTok);

    // Parse the initializer, if it exists.
    Expr* initializer = nullptr;
    if (!isFunArg && match(TokenKind::colonequal))
        initializer = expression();

    return new VarStmt(name.getIdentifier(), initializer, varType,
                       name.getLocation());
}

FunStmt* Parser::funDeclaration() {
    Stmts stmts;
    Token& funToken = consume({TokenKind::kw_FUN}, DiagID::err_expect,
                              "FUN"s);
    Token& funName = consume({TokenKind::identifier}, DiagID::err_expect,
                             "identifier"s);

    // Parse function arguments.
    FunDeclArgs args;
    consume({TokenKind::openpar}, DiagID::err_expect, ")"s);

    while (!match(TokenKind::closedpar) && !isAtEnd()) {
        // Store the argument as VarStmt.
        args.push_back(parseSingleVar(true));

        bool seenComma = match(TokenKind::comma);
        if (!seenComma && (peek().getKind() != TokenKind::closedpar))
            throw error(peek(), DiagID::err_expect, ",");
        if (seenComma && (peek().getKind() == TokenKind::closedpar))
            throw error(peek(), DiagID::err_expect, "function argument");
    }

    // Parse the return type.
    consume({TokenKind::colon}, DiagID::err_expect, ":"s);
    const Token& typeTok = consume({TokenKind::kw_INT, TokenKind::kw_BOOL},
                                   DiagID::err_expect, "type");
    Type* retType = Type::getTypeFromToken(typeTok);

    while (!match(TokenKind::kw_NUF) && !isAtEnd())
        stmts.emplace_back(declaration());

    return new FunStmt(funName.getIdentifier(), retType, std::move(args),
                       std::move(stmts), funToken.getLocation());
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

    consume({TokenKind::kw_THEN}, DiagID::err_expect, "THEN"s);

    // Parse the statements in the THEN block.
    while (!(match(TokenKind::kw_ELSE) || match(TokenKind::kw_FI)) &&
           !isAtEnd())
        thenStmts.push_back(declaration());

    // Parse the statements in the ELSE block.
    if (previous().getKind() == TokenKind::kw_ELSE) {
        while (!match(TokenKind::kw_FI) && !isAtEnd()) {
            elseStmts.push_back(declaration());
        }
    }

    return new IfStmt(cond, std::move(thenStmts), std::move(elseStmts),
                      previous().getLocation());
}

Stmt* Parser::printStmt() {
    Expr* printExpr = expression();

    consume({TokenKind::semicolon}, DiagID::err_expect, ";"s);
    return new PrintStmt(printExpr, previous().getLocation());
}

Stmt* Parser::returnStmt() {
    Expr* retExpr = nullptr;

    if (!check(TokenKind::semicolon))
        retExpr = expression();

    consume({TokenKind::semicolon}, DiagID::err_expect, ";"s);
    return new ReturnStmt(retExpr, previous().getLocation());
}

Stmt* Parser::varDeclaration() {
    auto* varStmt = parseSingleVar(false);
    consume({TokenKind::semicolon}, DiagID::err_expect, ";"s);

    return varStmt;
}

Stmt* Parser::exprStmt() {
    Expr* expr = expression();
    consume({TokenKind::semicolon}, DiagID::err_expect, ";"s);

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
        throw error(peek(), DiagID::err_invalid_assign_target, ""s);
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
        consume({TokenKind::closedpar}, DiagID::err_expect, ")"s);
        return new GroupingExpr(expr, previous().getLocation());
    } else if (match(TokenKind::integer_literal))
        return new IntLiteralExpr(previous().getLiteralData(),
                                  previous().getLocation());
    else if (match(TokenKind::identifier))
        return identifier();

    throw error(peek(), DiagID::err_expect, "expression"s);
}

Expr* Parser::identifier() {
    Token& name = previous();

    // If we see '(', this is a function call.
    if (match(TokenKind::openpar)) {
        FunCallArgs args;
        while (!match(TokenKind::closedpar) && !isAtEnd()) {
            // Parse the argument as an expression.
            args.push_back(expression());

            bool seenComma = match(TokenKind::comma);
            if (!seenComma && (peek().getKind() != TokenKind::closedpar))
                throw error(peek(), DiagID::err_expect, ",");
            if (seenComma && (peek().getKind() == TokenKind::closedpar))
                throw error(peek(), DiagID::err_expect, "expression");
        }

        return new CallExpr(name.getData(), std::move(args),
                            name.getLocation());
    } else {
        // Otherwise, it's a variable access.
        return new VarExpr(name.getData(), name.getLocation());
    }
}

// Parse the token stream and return the root of the AST.
ModuleStmt* Parser::parse() {
    Token& moduleToken = peek();
    Stmts funStmts;

    // Parse all functions inside the file.
    while (!isAtEnd()) {
        // Catch any errors when parsing module functions.
        try {
            funStmts.push_back(funDeclaration());
        } catch (ParserError& e) {
            return nullptr;
        }
    }

    ModuleStmt* moduleStmt = new ModuleStmt("main", std::move(funStmts),
                                            moduleToken.getLocation());
    return moduleStmt;
}
