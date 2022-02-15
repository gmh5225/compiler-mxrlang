#include "llvm/ADT/SmallString.h"

#include "ASTPrinter.h"

using namespace mxrlang;

// Helper function which prints a binary operator.
// (binop type (leftExpr) (rightExpr))
template <typename BinExpr>
void ASTPrinter::printBinary(std::string& op, BinExpr binExpr) {
    out() << "(" + op + " " + binExpr->getType()->toString() + " ";
    evaluate(binExpr->getLeft());
    out() << " ";
    evaluate(binExpr->getRight());
    out() << ")";
}

// Helper funcion which prints out a variable declaration or a function
// declaration argument.
void ASTPrinter::printVar(VarStmt* stmt) {
    out() << stmt->getName().str() + " " + stmt->getType()->toString();
}

// (= (dest) (source))
void ASTPrinter::visit(AssignExpr* expr) {
    out() << "(= ";
    evaluate(expr->getDest());
    out() << " ";
    evaluate(expr->getSource());
    out() << ")";
}

void ASTPrinter::visit(BinaryArithExpr* expr) {
    printBinary(expr->getOpString(), expr);
}

void ASTPrinter::visit(BinaryLogicalExpr* expr) {
    printBinary(expr->getOpString(), expr);
}

// (true/false bool)
void ASTPrinter::visit(BoolLiteralExpr* expr) {
    std::string value = expr->getValue() ? "true" : "false";
    out() << "(" + value + " " + expr->getType()->toString() + ")";
}

// (call funName (arg1) (arg2) ... (argn))
void ASTPrinter::visit(CallExpr* expr) {
    out() << "(call " + expr->getName().str();
    for (auto* arg : expr->getArgs()) {
        out() << " ";
        evaluate(arg);
    }
    out() << ")";
}

// (group (expr))
void ASTPrinter::visit(GroupingExpr* expr) {
    out() << "(group " + expr->getType()->toString() + " ";
    evaluate(expr->getExpr());
    out() << ")";
}

// (intLiteral int)
void ASTPrinter::visit(IntLiteralExpr* expr) {
    // Convert APSInt to string.
    llvm::SmallString<30> literal;
    expr->getValue().toString(literal);

    out() << "(" + std::string(literal) + " " +
              expr->getType()->toString() + ")";
}

// (op (expr))
void ASTPrinter::visit(UnaryExpr* expr) {
    out() << "(" + expr->getOpString() + " ";
    evaluate(expr->getExpr());
    out() << ")";
}

// (varName varType)
void ASTPrinter::visit(VarExpr* expr) {
    out() << "(" + expr->getName().str() + " " +
             expr->getType()->toString() + ")";
}

void ASTPrinter::visit(ExprStmt* stmt) {
    out() << indent;
    evaluate(stmt->getExpr());
    out() << "\n";
}

// (fun funName retType (arg1) (arg2))
//     (stmt1)
//     (stmt2)
//     ...
//     (stmtn)
void ASTPrinter::visit(FunStmt* stmt) {
    out() << "(fun " + stmt->getName().str() + " " +
             stmt->getType()->toString();
    // Print out the function arguments (VarStmt type).
    for (auto* arg : stmt->getArgs()) {
        out() << " (";
        printVar(arg);
        out() << ")";
    }
    out() << ")\n";

    // Print out the function body.
    increaseIndent();
    for (auto* st : stmt->getBody())
        evaluate(st);
    decreaseIndent();
}

// (if (conditionExpr))
//     (stmt1)
//     ...
//     (stmtn)
// (else)
//     (stmt1)
//     ...
//     (stmtn)
void ASTPrinter::visit(IfStmt* stmt) {
    out() << indent + "(if ";
    increaseIndent();
    evaluate(stmt->getCond());
    out() << ")\n";

    for (auto* thenStmt : stmt->getThenStmts())
        evaluate(thenStmt);

    if (!stmt->getElseStmts().empty()) {
        decreaseIndent();
        out() << indent + "(else)\n";
        increaseIndent();
        for (auto* elseStmt : stmt->getElseStmts())
            evaluate(elseStmt);
    }

    decreaseIndent();
}

void ASTPrinter::visit(ModuleStmt* stmt) {
    for (auto* st : stmt->getBody()) {
        assert(llvm::isa<FunStmt>(st) && "Currently only function"
               "declarations allowed in module.");

        evaluate(st);
    }
}

// (print (printExpr))
void ASTPrinter::visit(PrintStmt* stmt) {
    out() << indent + "(print ";
    evaluate(stmt->getPrintExpr());
    out() << ")\n";
}

// (return (returnExpr))
void ASTPrinter::visit(ReturnStmt* stmt) {
    out() << indent + "(return ";

    // If there is a return value, print it.
    if (stmt->getRetExpr())
        evaluate(stmt->getRetExpr());

    out() << ")\n";
}

// (var varName varType (initializerExpr))
void ASTPrinter::visit(VarStmt* stmt) {
    // Print the operation.
    out() << indent + "(var ";
    printVar(stmt);

    // If there is an initializer, print it.
    if (stmt->getInitializer()) {
        out() << " ";
        evaluate(stmt->getInitializer());
    }

    out() << ")\n";
}
