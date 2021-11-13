#include "llvm/ADT/SmallString.h"

#include "ASTPrinter.h"

using namespace mxrlang;

void ASTPrinter::visit(ModuleStmt* stmt) {
    for (auto st : stmt->getBody()) {
        assert(llvm::isa<FunStmt>(st) && "Currently only function declarations"
               " allowed in module.");

        evaluate(st);
    }
}

void ASTPrinter::visit(FunStmt* stmt) {
    for (auto st : stmt->getBody()) {
        assert(llvm::isa<VarStmt>(st) && "Currently only variable declarations"
               " allowed in module.");

        evaluate(st);
    }
}

void ASTPrinter::visit(VarStmt* stmt) {
    result += "Statement " + std::to_string(currStmt++) + "\n";

    // Print the operation.
    result += "(var ";

    // Print the name.
    result += stmt->getName().str();

    // If there is an initializer, print it.
    if (stmt->getInitializer()) {
        result += " ";
        evaluate(stmt->getInitializer());
    }

    result += ")\n";
}

void ASTPrinter::visit(LiteralExpr* expr) {
    llvm::SmallString<30> literal;
    expr->getValue().toString(literal);

    result += std::string(literal);
}
