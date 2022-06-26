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
Token &Parser::advance() {
  if (!isAtEnd())
    current++;
  return previous();
}

// Whether the current token signalizes the end of the token stream.
bool Parser::isAtEnd() { return peek().getKind() == TokenKind::eof; }

// Return the next token, but don't advance the stream.
Token &Parser::peek() { return *current; }

// Return the previous token.
Token &Parser::previous() {
  TokenIterator help = --current;
  current++;

  return *help;
}

// Check whether the next token matches the expected and advance the stream
// if it does. Conversely, throw an error.
Token &Parser::consume(std::initializer_list<TokenKind> kinds, DiagID diagID,
                       std::string args...) {
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
    case TokenKind::kw_ELIHW:
    case TokenKind::kw_NUF:
    case TokenKind::kw_PRINT:
    case TokenKind::kw_SCAN:
    case TokenKind::kw_THEN:
    case TokenKind::kw_WHILE:
    case TokenKind::kw_VAR:
      return;
    default:;
    }

    advance();
  }
}

// Report an error and throw an exception.
Parser::ParserError Parser::error(const Token &tok, DiagID id,
                                  std::string args...) {
  diag.report(tok.getLocation(), id, args);
  auto perror = ParserError();
  return perror;
}

// Helper which creates a VarStmt while parsing variable declarations,
// or function declaration arguments.
VarDecl *Parser::parseSingleVar(bool isFunArg) {
  // Parse the variable name.
  const Token &name =
      consume({TokenKind::identifier}, DiagID::err_expect, "identifer"s);

  consume({TokenKind::colon}, DiagID::err_expect, ":"s);

  // Parse the type.
  auto *varType = parseType();

  // Parse the initializer, if it exists.
  Expr *initializer = nullptr;
  if (!isFunArg && match(TokenKind::colonequal))
    initializer = expression();

  // Decay the array type if this is a function argument.
  if (isFunArg && varType->getTypeKind() == Type::TypeKind::Array)
    varType = llvm::dyn_cast<ArrayType>(varType)->decay();

  return new VarDecl(name.getData(), initializer, varType,
                     /* global= */ false, name.getLocation());
}

// Parse a type declaration.
Type *Parser::parseType() {
  const Token &typeTok = consume({TokenKind::kw_INT, TokenKind::kw_BOOL},
                                 DiagID::err_expect, "type");
  auto *type = Type::getTypeFromToken(typeTok);

  while (match(TokenKind::star))
    type = new PointerType(type);

  Exprs elNums;
  while (match(TokenKind::openbracket)) {
    auto *elNumExpr = primary();
    if (!llvm::isa<IntLiteralExpr>(elNumExpr))
      throw error(previous(), DiagID::err_array_size_not_int, ""s);
    elNums.push_back(elNumExpr);

    consume({TokenKind::closedbracket}, DiagID::err_expect, "]"s);
  }

  for (auto it = elNums.rbegin(); it != elNums.rend(); ++it)
    type =
        new ArrayType(type, llvm::dyn_cast<IntLiteralExpr>(*it)->getRawValue());

  return type;
}

// FIXME: We currently allow parsing internal functions,
// although they are not implemented.
Node *Parser::declaration() {
  try {
    if (match(TokenKind::kw_VAR))
      return varDeclaration();
    else if (match(TokenKind::kw_FUN))
      return funDeclaration();
    return statement();
  } catch (ParserError &e) {
    synchronize();
    return nullptr;
  }
}

Decl *Parser::funDeclaration() {
  Token &funToken = previous();
  Token &funName =
      consume({TokenKind::identifier}, DiagID::err_expect, "identifier"s);

  // Parse the return type.
  consume({TokenKind::colon}, DiagID::err_expect, ":"s);
  auto *retType = parseType();

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

  if (previous().isNot(TokenKind::closedpar))
    throw error(previous(), DiagID::err_expect, ")");

  // Parse the function body
  Nodes body;
  while (!match(TokenKind::kw_NUF) && !isAtEnd())
    body.emplace_back(declaration());

  if (previous().isNot(TokenKind::kw_NUF))
    throw error(previous(), DiagID::err_expect,
                "NUF at the end of function definition");

  return new FunDecl(funName.getData(), retType, std::move(args),
                     std::move(body), funToken.getLocation());
}

Decl *Parser::varDeclaration() {
  auto *varDecl = parseSingleVar(false);
  consume({TokenKind::semicolon}, DiagID::err_expect, ";"s);

  return varDecl;
}

Stmt *Parser::statement() {
  if (match(TokenKind::kw_IF))
    return ifStmt();
  else if (match(TokenKind::kw_PRINT))
    return printStmt();
  else if (match(TokenKind::kw_RETURN))
    return returnStmt();
  else if (match(TokenKind::kw_SCAN))
    return scanStmt();
  else if (match(TokenKind::kw_WHILE))
    return whileStmt();
  else
    return exprStmt();
}

Stmt *Parser::exprStmt() {
  Expr *expr = expression();
  consume({TokenKind::semicolon}, DiagID::err_expect, ";"s);

  return new ExprStmt(expr, expr->getLoc());
}

Stmt *Parser::ifStmt() {
  auto loc = previous().getLocation();
  Nodes thenBody;
  Nodes elseBody;
  // Parse the IF condition.
  Expr *cond = expression();

  consume({TokenKind::kw_THEN}, DiagID::err_expect, "THEN"s);

  // Parse the statements in the THEN block.
  while (!(match(TokenKind::kw_ELSE) || match(TokenKind::kw_FI)) && !isAtEnd())
    thenBody.push_back(declaration());

  if (!previous().isOneOf(TokenKind::kw_ELSE, TokenKind::kw_FI))
    throw error(previous(), DiagID::err_expect,
                "FI at the end of IF statement");

  // Parse the statements in the ELSE block.
  if (previous().getKind() == TokenKind::kw_ELSE) {
    while (!match(TokenKind::kw_FI) && !isAtEnd()) {
      elseBody.push_back(declaration());
    }

    if (previous().isNot(TokenKind::kw_FI))
      throw error(previous(), DiagID::err_expect,
                  "FI at the end of IF statement");
  }

  return new IfStmt(cond, std::move(thenBody), std::move(elseBody), loc);
}

Stmt *Parser::whileStmt() {
  auto loc = previous().getLocation();
  Nodes body;
  // Parse the WHILE condition.
  Expr *cond = expression();

  consume({TokenKind::kw_DO}, DiagID::err_expect, "DO"s);

  // Parse the body.
  while (!match(TokenKind::kw_ELIHW) && !isAtEnd())
    body.push_back(declaration());

  if (previous().isNot(TokenKind::kw_ELIHW))
    throw error(previous(), DiagID::err_expect,
                "ELIHW at the end of WHILE statement");

  return new WhileStmt(cond, std::move(body), loc);
}

Stmt *Parser::printStmt() {
  auto loc = previous().getLocation();
  Expr *printExpr = expression();

  consume({TokenKind::semicolon}, DiagID::err_expect, ";"s);
  return new PrintStmt(printExpr, loc);
}

Stmt *Parser::returnStmt() {
  auto loc = previous().getLocation();
  Expr *retExpr = nullptr;

  if (!check(TokenKind::semicolon))
    retExpr = expression();

  consume({TokenKind::semicolon}, DiagID::err_expect, ";"s);
  return new ReturnStmt(retExpr, loc);
}

Stmt *Parser::scanStmt() {
  auto loc = previous().getLocation();
  Expr *scanExpr = expression();

  if (!llvm::isa<LoadExpr>(scanExpr))
    throw error(peek(), DiagID::err_expect, "variable"s);

  consume({TokenKind::semicolon}, DiagID::err_expect, ";"s);
  return new ScanStmt(scanExpr, loc);
}

Expr *Parser::expression() { return assignment(); }

Expr *Parser::assignment() {
  auto *expr = logicalOr();

  if (match(TokenKind::colonequal)) {
    auto *source = logicalOr();
    expr = new AssignExpr(expr, source, expr->getLoc());
  }

  return expr;
}

Expr *Parser::logicalOr() {
  auto *expr = logicalAnd();

  while (match(TokenKind::logicor)) {
    auto opString = previous().getData();
    auto *right = logicalAnd();
    expr = new BinaryLogicalExpr(BinaryLogicalExpr::BinaryLogicalExprKind::Or,
                                 expr, right, opString, expr->getLoc());
  }

  return expr;
}

Expr *Parser::logicalAnd() {
  auto *expr = equality();

  while (match(TokenKind::logicand)) {
    auto opString = previous().getData();
    auto *right = equality();
    expr = new BinaryLogicalExpr(BinaryLogicalExpr::BinaryLogicalExprKind::And,
                                 expr, right, opString, expr->getLoc());
  }

  return expr;
}

Expr *Parser::equality() {
  auto *expr = comparison();

  while (match(TokenKind::equal) || match(TokenKind::noteq)) {
    auto opString = previous().getData();
    BinaryLogicalExpr::BinaryLogicalExprKind kind;
    switch (previous().getKind()) {
    case TokenKind::equal:
      kind = BinaryLogicalExpr::BinaryLogicalExprKind::Eq;
      break;
    case TokenKind::noteq:
      kind = BinaryLogicalExpr::BinaryLogicalExprKind::NotEq;
      break;
    default:
      llvm_unreachable("Wrong binary comparison operator.");
    }

    auto *right = comparison();
    expr = new BinaryLogicalExpr(kind, expr, right, opString, expr->getLoc());
  }

  return expr;
}

Expr *Parser::comparison() {
  auto *expr = addSub();

  while (match(TokenKind::greater) || match(TokenKind::greatereq) ||
         match(TokenKind::less) || match(TokenKind::lesseq)) {
    auto opString = previous().getData();
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
      llvm_unreachable("Wrong binary comparison operator.");
    }

    auto *right = addSub();
    expr = new BinaryLogicalExpr(kind, expr, right, opString, expr->getLoc());
  }

  return expr;
}

Expr *Parser::addSub() {
  auto *expr = mulDiv();

  while (match(TokenKind::plus) || match(TokenKind::minus)) {
    auto opString = previous().getData();
    BinaryArithExpr::BinaryArithExprKind kind;
    switch (previous().getKind()) {
    case TokenKind::plus:
      kind = BinaryArithExpr::BinaryArithExprKind::Add;
      break;
    case TokenKind::minus:
      kind = BinaryArithExpr::BinaryArithExprKind::Sub;
      break;
    default:
      llvm_unreachable("Wrong binary arithmetic operator.");
    }

    auto *right = mulDiv();
    expr = new BinaryArithExpr(kind, expr, right, opString, expr->getLoc());
  }

  return expr;
}

Expr *Parser::mulDiv() {
  auto *expr = unary();

  while (match(TokenKind::star) || match(TokenKind::slash)) {
    auto opString = previous().getData();
    BinaryArithExpr::BinaryArithExprKind kind;
    switch (previous().getKind()) {
    case TokenKind::star:
      kind = BinaryArithExpr::BinaryArithExprKind::Mul;
      break;
    case TokenKind::slash:
      kind = BinaryArithExpr::BinaryArithExprKind::Div;
      break;
    default:
      llvm_unreachable("Wrong binary arithmetic operator.");
    }

    auto *right = unary();
    expr = new BinaryArithExpr(kind, expr, right, opString, expr->getLoc());
  }

  return expr;
}

Expr *Parser::unary() {
  if (match(TokenKind::bang) || match(TokenKind::minus)) {
    auto opString = previous().getData();
    UnaryExpr::UnaryExprKind kind;
    switch (previous().getKind()) {
    case TokenKind::bang:
      kind = UnaryExpr::UnaryExprKind::NegLogic;
      break;
    case TokenKind::minus:
      kind = UnaryExpr::UnaryExprKind::NegArith;
      break;
    default:
      llvm_unreachable("Wrong unary operator.");
    }

    auto *expr = primary();
    return new UnaryExpr(kind, expr, opString, expr->getLoc());
  }

  if (match(TokenKind::ampersand) || match(TokenKind::star)) {
    auto opString = previous().getData();
    PointerOpExpr::PointerOpKind kind;
    switch (previous().getKind()) {
    case TokenKind::ampersand:
      kind = PointerOpExpr::PointerOpKind::AddressOf;
      break;
    case TokenKind::star:
      kind = PointerOpExpr::PointerOpKind::Dereference;
      break;
    default:
      llvm_unreachable("Wrong pointer operation.");
    }

    Expr *expr = primary();
    expr = new PointerOpExpr(kind, expr, opString, expr->getLoc());
    if (kind == PointerOpExpr::PointerOpKind::Dereference)
      // Always perform the load after dereferencing. Semantic check will remove
      // the load if we are dereferencing for writing.
      expr = new LoadExpr(expr, expr->getLoc());

    return expr;
  }

  return primary();
}

Expr *Parser::primary() {
  if (match(TokenKind::kw_TRUE) || match(TokenKind::kw_FALSE)) {
    bool value = previous().getKind() == TokenKind::kw_TRUE ? true : false;
    return new BoolLiteralExpr(value, previous().getLocation());
  } else if (match(TokenKind::openpar)) {
    auto *expr = expression();
    consume({TokenKind::closedpar}, DiagID::err_expect, ")"s);
    return new GroupingExpr(expr, previous().getLocation());
  } else if (match(TokenKind::integer_literal))
    return new IntLiteralExpr(previous().getData(), previous().getLocation());
  else if (match(TokenKind::identifier))
    return identifier();

  throw error(peek(), DiagID::err_expect, "expression"s);
}

Expr *Parser::identifier() {
  Token &name = previous();
  Expr *expr = new VarExpr(name.getData(), name.getLocation());

  // If we see '(', this is a function call.
  if (match(TokenKind::openpar))
    return funCall(name);
  else if (match(TokenKind::openbracket)) {
    // If we see '[' this is array indexing.
    expr = arrayAccess(expr);

    // Always perform the load after array access. Semantic check will remove
    // the load if we are accessing for writing.
    return new LoadExpr(expr, name.getLocation());
  } else {
    // Otherwise, it's a variable access.
    //
    // Always load the variable for now. Semantic check will remove redundant
    // loads.
    return new LoadExpr(expr, name.getLocation());
  }
}

Expr *Parser::funCall(const Token &name) {
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

  if (previous().isNot(TokenKind::closedpar))
    throw error(previous(), DiagID::err_expect, ")");

  return new CallExpr(name.getData(), std::move(args), name.getLocation());
}

Expr *Parser::arrayAccess(Expr *var) {
  // Create as many array accesses as we have []'s.
  Exprs elements;
  do {
    auto *element = logicalOr();
    consume({TokenKind::closedbracket}, DiagID::err_expect, "]"s);
    elements.push_back(element);
  } while (match(TokenKind::openbracket));

  for (auto it = elements.rbegin(); it != elements.rend(); ++it)
    var = new ArrayAccessExpr(var, *it, previous().getLocation());

  return var;
}

// Parse the token stream and return the root of the AST.
ModuleDecl *Parser::parse() {
  Token &moduleToken = peek();
  Decls decls;

  while (!isAtEnd()) {
    auto *decl = llvm::dyn_cast_or_null<Decl>(declaration());
    if (!decl)
      return nullptr;
    decls.push_back(decl);

    // This is a global variable, so mark it as global.
    if (auto *globalVarDecl = llvm::dyn_cast<VarDecl>(decl))
      globalVarDecl->setGlobal(true);
  }

  ModuleDecl *moduleStmt =
      new ModuleDecl("main", std::move(decls), moduleToken.getLocation());
  return moduleStmt;
}
