#ifndef ASTPRINTER_H
#define ASTPRINTER_H

#include "Tree.h"

namespace mxrlang {

class ASTPrinter : public Visitor {
    // Expression visitor methods
    void visit(AssignExpr* expr) override;
    void visit(BinaryArithExpr* expr) override;
    void visit(BinaryLogicalExpr* expr) override;
    void visit(BoolLiteralExpr* expr) override;
    void visit(CallExpr* expr) override;
    void visit(GroupingExpr* expr) override;
    void visit(IntLiteralExpr* expr) override;
    void visit(UnaryExpr* expr) override;
    void visit(VarExpr* expr) override;

    // Statement visitor methods
    void visit(ExprStmt* stmt) override;
    void visit(IfStmt* stmt) override;
    void visit(PrintStmt* stmt) override;
    void visit(ReturnStmt* stmt) override;
    void visit(ScanStmt* stmt) override;
    void visit(UntilStmt* stmt) override;

    // Declaration visitor methods
    void visit(FunDecl* decl) override;
    void visit(ModuleDecl* decl) override;
    void visit(VarDecl* decl) override;

    // Controls the level of indentation during the print.
    // E.g. when entering IF stmt, push back a "\t", and pop it when
    // exiting the IF stmt.
    std::string indent = "";

    void increaseIndent() { indent.append("\t"); }
    void decreaseIndent() { indent.resize(indent.size() - 1); }

    // Helper function which prints a binary operator.
    template <typename BinExpr>
    void printBinary(const llvm::StringRef& op, BinExpr binExpr);

    // Helper function which prints a variable declaration or a function
    // declaration argument.
    void printVar(VarDecl* stmt);

    // Wrapper arout llvm::outs().
    llvm::raw_fd_ostream& out() { return llvm::outs(); }

    // Helper function for evaluating an expression or a statement.
    template <typename T>
    void evaluate(const T expr) {
        expr->accept(this);
    }

public:
    // Runner.
    void run(ModuleDecl* moduleDecl) {
        llvm::outs() << "----------- AST dump --------------\n";
        evaluate(moduleDecl);
        llvm::outs() << "-----------------------------------\n\n\n";
    }
};

}

#endif // ASTPRINTER_H
