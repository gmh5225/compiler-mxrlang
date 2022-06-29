#include "llvm/ADT/SmallString.h"

#include "ASTPrinter.h"

using namespace mxrlang;

// Helper function which prints a binary operator.
// (binop type (leftExpr) (rightExpr))
template <typename BinExpr>
void ASTPrinter::printBinary(const llvm::StringRef &op, const BinExpr binExpr) {
  out() << "(" + op.str() + " " + binExpr->getType()->toString() + " ";
  evaluate(binExpr->getLeft());
  out() << " ";
  evaluate(binExpr->getRight());
  out() << ")";
}

// Helper funcion which prints out a variable declaration or a function
// declaration argument.
void ASTPrinter::printVar(const VarDecl *stmt) {
  out() << stmt->getName().str() + " " + stmt->getType()->toString();
}

// ([elem] (expr))
void ASTPrinter::visit(ArrayAccessExpr *expr) {
  out() << "([";
  evaluate(expr->getElement());
  out() << "] (";
  evaluate(expr->getArray());
  out() << ")";
}

// { val1, val2, val3, ... }
void ASTPrinter::visit(ArrayInitExpr *expr) {
  out() << "{ ";
  for (auto *val : expr->getVals()) {
    evaluate(val);
    out() << " ";
  }
  out() << "}";
}

// (= (dest, dest, dest, ....) (source))
void ASTPrinter::visit(AssignExpr *expr) {
  out() << "(= ";
  evaluate(expr->getDest());
  out() << " ";
  evaluate(expr->getSource());
  out() << ")";
}

void ASTPrinter::visit(BinaryArithExpr *expr) {
  printBinary(expr->getOpString(), expr);
}

void ASTPrinter::visit(BinaryLogicalExpr *expr) {
  printBinary(expr->getOpString(), expr);
}

// (true/false bool)
void ASTPrinter::visit(BoolLiteralExpr *expr) {
  std::string value = expr->getValue() ? "true" : "false";
  out() << "(" + value + " " + expr->getType()->toString() + ")";
}

// (call funName (arg1) (arg2) ... (argn))
void ASTPrinter::visit(CallExpr *expr) {
  out() << "(call " + expr->getName().str();
  for (auto *arg : expr->getArgs()) {
    out() << " ";
    evaluate(arg);
  }
  out() << ")";
}

// (intLiteral int)
void ASTPrinter::visit(IntLiteralExpr *expr) {
  // Convert APSInt to string.
  llvm::SmallString<30> literal;
  expr->getValue().toString(literal);

  out() << "(" + std::string(literal) + " " + expr->getType()->toString() + ")";
}

// (load var)
void ASTPrinter::visit(LoadExpr *expr) {
  out() << "(load ";
  evaluate(expr->getExpr());
  out() << ")";
}

// (op (expr))
void ASTPrinter::visit(PointerOpExpr *expr) {
  out() << "(" + expr->getOpString().str() + " ";
  evaluate(expr->getExpr());
  out() << ")";
}

// (op (expr))
void ASTPrinter::visit(UnaryExpr *expr) {
  out() << "(" + expr->getOpString().str() + " ";
  evaluate(expr->getExpr());
  out() << ")";
}

// (varName varType)
void ASTPrinter::visit(VarExpr *expr) {
  out() << "(" + expr->getName().str() + " " + expr->getType()->toString() +
               ")";
}

void ASTPrinter::visit(ExprStmt *stmt) {
  out() << indent;
  evaluate(stmt->getExpr());
  out() << "\n";
}

// (if (conditionExpr))
//     (stmt1)
//     ...
//     (stmtn)
// (else)
//     (stmt1)
//     ...
//     (stmtn)
void ASTPrinter::visit(IfStmt *stmt) {
  out() << indent + "(if ";
  increaseIndent();
  evaluate(stmt->getCond());
  out() << ")\n";

  for (auto *thenStmt : stmt->getThenBody())
    evaluate(thenStmt);

  if (!stmt->getElseBody().empty()) {
    decreaseIndent();
    out() << indent + "(else)\n";
    increaseIndent();
    for (auto *elseStmt : stmt->getElseBody())
      evaluate(elseStmt);
  }

  decreaseIndent();
}

// (print (printExpr))
void ASTPrinter::visit(PrintStmt *stmt) {
  out() << indent + "(print ";
  evaluate(stmt->getPrintExpr());
  out() << ")\n";
}

// (return (returnExpr))
void ASTPrinter::visit(ReturnStmt *stmt) {
  out() << indent + "(return ";

  // If there is a return value, print it.
  if (stmt->getRetExpr())
    evaluate(stmt->getRetExpr());

  out() << ")\n";
}

// (scan (scanVar))
void ASTPrinter::visit(ScanStmt *stmt) {
  out() << indent + "(scan ";
  evaluate(stmt->getScanVar());
  out() << ")\n";
}

// (while (conditionExpr))
//     (stmt1)
//     ...
//     (stmtn)
void ASTPrinter::visit(WhileStmt *stmt) {
  out() << indent + "(while ";
  increaseIndent();
  evaluate(stmt->getCond());
  out() << ")\n";

  for (auto *s : stmt->getBody())
    evaluate(s);

  decreaseIndent();
}

// (fun funName retType (arg1) (arg2))
//     (stmt1)
//     (stmt2)
//     ...
//     (stmtn)
void ASTPrinter::visit(FunDecl *decl) {
  out() << "(fun " + decl->getName().str() + " " +
               decl->getRetType()->toString();
  // Print out the function arguments (VarStmt type).
  for (auto *arg : decl->getArgs()) {
    out() << " (";
    printVar(arg);
    out() << ")";
  }
  out() << ")\n";

  // Print out the function body.
  increaseIndent();
  for (auto *st : decl->getBody())
    evaluate(st);
  decreaseIndent();
}

void ASTPrinter::visit(ModuleDecl *decl) {
  for (auto *dec : decl->getBody()) {
    evaluate(dec);
  }
}

// (var varName varType (initializerExpr))
void ASTPrinter::visit(VarDecl *decl) {
  // Print the operation.
  out() << indent + "(var ";
  printVar(decl);

  // If there is an initializer, print it.
  if (decl->getInitializer()) {
    out() << " ";
    evaluate(decl->getInitializer());
  }

  out() << ")\n";
}
