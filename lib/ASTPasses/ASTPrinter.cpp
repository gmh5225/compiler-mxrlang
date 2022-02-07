#include "llvm/ADT/SmallString.h"

#include "ASTPrinter.h"

using namespace mxrlang;

template <typename BinExpr>
void ASTPrinter::printBinary(std::string& op, BinExpr binExpr) {
    result += "(" + op + " " + binExpr->getType()->toString() + " ";
    evaluate(binExpr->getLeft());
    result += " ";
    evaluate(binExpr->getRight());
    result += ")";
}

void ASTPrinter::printVar(VarStmt* stmt) {
    result += stmt->getName().str() + " " + stmt->getType()->toString();
}

void ASTPrinter::visit(AssignExpr* expr) {
    result += "(= ";
    evaluate(expr->getDest());
    result += " ";
    evaluate(expr->getSource());
    result += ")";
}

void ASTPrinter::visit(BinaryArithExpr* expr) {
    printBinary(expr->getOpString(), expr);
}

void ASTPrinter::visit(BinaryLogicalExpr* expr) {
    printBinary(expr->getOpString(), expr);
}

void ASTPrinter::visit(BoolLiteralExpr* expr) {
    std::string value = expr->getValue() ? "true" : "false";
    result += "(" + value + " " + expr->getType()->toString() + ")";
}

void ASTPrinter::visit(CallExpr* expr) {
    result += "(call " + expr->getName().str();
    for (auto arg : expr->getArgs()) {
        result += " ";
        evaluate(arg);
    }
    result += ")";
}

void ASTPrinter::visit(GroupingExpr* expr) {
    result += "(group " + expr->getType()->toString() + " ";
    evaluate(expr->getExpr());
    result += ")";
}

void ASTPrinter::visit(IntLiteralExpr* expr) {
    llvm::SmallString<30> literal;
    expr->getValue().toString(literal);

    result += "(" + std::string(literal) + " " +
              expr->getType()->toString() + ")";
}

void ASTPrinter::visit(UnaryExpr* expr) {
    result += "(" + expr->getOpString() + " ";
    evaluate(expr->getExpr());
    result += ")";
}

void ASTPrinter::visit(VarExpr* expr) {
    result += "(" + expr->getName().str() + " " +
              expr->getType()->toString() + ")";
}

void ASTPrinter::visit(ExprStmt* stmt) {
    result += indent;
    evaluate(stmt->getExpr());
    result += "\n";
}

void ASTPrinter::visit(FunStmt* stmt) {
    result += "(fun " + stmt->getName().str() + " " +
              stmt->getType()->toString();
    for (auto arg : stmt->getArgs()) {
        result += " (";
        printVar(arg);
        result += ")";
    }
    result += ")\n";

    increaseIndent();
    for (auto st : stmt->getBody())
        evaluate(st);
    decreaseIndent();
}

void ASTPrinter::visit(IfStmt* stmt) {
    result += indent + "(if ";
    increaseIndent();
    evaluate(stmt->getCond());
    result += ")\n";

    for (auto thenStmt : stmt->getThenStmts())
        evaluate(thenStmt);

    if (!stmt->getElseStmts().empty()) {
        decreaseIndent();
        result += indent + "(else)\n";
        increaseIndent();
        for (auto elseStmt : stmt->getElseStmts())
            evaluate(elseStmt);
    }

    decreaseIndent();
}

void ASTPrinter::visit(ModuleStmt* stmt) {
    for (auto st : stmt->getBody()) {
        assert(llvm::isa<FunStmt>(st) && "Currently only function declarations"
               " allowed in module.");

        evaluate(st);
    }
}

void ASTPrinter::visit(PrintStmt* stmt) {
    result += indent + "(print ";

    evaluate(stmt->getPrintExpr());

    result += ")\n";
}

void ASTPrinter::visit(ReturnStmt* stmt) {
    result += indent + "(return ";

    // If there is a return value, print it.
    if (stmt->getRetExpr())
        evaluate(stmt->getRetExpr());

    result += ")\n";
}

void ASTPrinter::visit(VarStmt* stmt) {
    // Print the operation.
    result += indent + "(var ";

    printVar(stmt);

    // If there is an initializer, print it.
    if (stmt->getInitializer()) {
        result += " ";
        evaluate(stmt->getInitializer());
    }

    result += ")\n";
}
