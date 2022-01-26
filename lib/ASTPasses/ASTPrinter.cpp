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
    result += indent + "Statement " + std::to_string(currStmt++) + "\n"
              + indent;
    evaluate(stmt->getExpr());
    result += "\n";
}

void ASTPrinter::visit(FunStmt* stmt) {
    for (auto st : stmt->getBody())
        evaluate(st);
}

void ASTPrinter::visit(IfStmt* stmt) {
    result += indent + "Statement " + std::to_string(currStmt++) + "\n";

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
    result += indent + "Statement " + std::to_string(currStmt++) + "\n";
    result += indent + "(print ";

    evaluate(stmt->getPrintExpr());

    result += ")\n";
}

void ASTPrinter::visit(ReturnStmt* stmt) {
    result += indent + "Statement " + std::to_string(currStmt++) + "\n";

    result += indent + "(return ";

    // If there is a return value, print it.
    if (stmt->getRetExpr())
        evaluate(stmt->getRetExpr());

    result += ")\n";
}

void ASTPrinter::visit(VarStmt* stmt) {
    result += indent + "Statement " + std::to_string(currStmt++) + "\n";

    // Print the operation.
    result += indent + "(var ";

    // Print the name.
    result += stmt->getName().str() + " ";

    // Print the type.
    result += stmt->getType()->toString();

    // If there is an initializer, print it.
    if (stmt->getInitializer()) {
        result += " ";
        evaluate(stmt->getInitializer());
    }

    result += ")\n";
}
