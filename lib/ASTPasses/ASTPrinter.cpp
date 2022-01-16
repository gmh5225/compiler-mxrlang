#include "llvm/ADT/SmallString.h"

#include "ASTPrinter.h"

using namespace mxrlang;

void ASTPrinter::visit(AssignExpr* expr) {
    result += "(= ";
    evaluate(expr->getDest());
    result += " ";
    evaluate(expr->getSource());
    result += ")";
}

void ASTPrinter::visit(BoolLiteralExpr* expr) {
    std::string value = expr->getValue() ? "true" : "false";
    result += "(" + value + " " + expr->getType()->toString() + ")";
}

void ASTPrinter::visit(IntLiteralExpr* expr) {
    llvm::SmallString<30> literal;
    expr->getValue().toString(literal);

    result += "(" + std::string(literal) + " " +
              expr->getType()->toString() + ")";
}

void ASTPrinter::visit(VarExpr* expr) {
    result += "(" + expr->getName().str() + " " +
              expr->getType()->toString() + ")";
}

void ASTPrinter::visit(ModuleStmt* stmt) {
    for (auto st : stmt->getBody()) {
        assert(llvm::isa<FunStmt>(st) && "Currently only function declarations"
               " allowed in module.");

        evaluate(st);
    }
}

void ASTPrinter::visit(FunStmt* stmt) {
    for (auto st : stmt->getBody())
        evaluate(st);
}

void ASTPrinter::visit(ExprStmt* stmt) {
    result += "Statement " + std::to_string(currStmt++) + "\n";
    evaluate(stmt->getExpr());
    result += "\n";
}

void ASTPrinter::visit(ReturnStmt* stmt) {
    result += "Statement " + std::to_string(currStmt++) + "\n";

    result += "(return ";

    // If there is a return value, print it.
    if (stmt->getRetExpr())
        evaluate(stmt->getRetExpr());

    result += ")\n";
}

void ASTPrinter::visit(VarStmt* stmt) {
    result += "Statement " + std::to_string(currStmt++) + "\n";

    // Print the operation.
    result += "(var ";

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
