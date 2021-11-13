#ifndef ASTPRINTER_H
#define ASTPRINTER_H

#include "Tree.h"

namespace mxrlang {

class ASTPrinter : public ExprVisitor,
                   public StmtVisitor {
    std::string result;
    uint16_t currStmt = 1;

    // Expression visitor methods
    void visit(LiteralExpr* expr) override;

    // Statement visitor methods
    void visit(FunStmt* stmt) override;
    void visit(ModuleStmt* stmt) override;
    void visit(VarStmt* stmt) override;

    // Helper function for evaluating an expression or a statement.
    template <typename T>
    void evaluate(const T expr) {
        expr->accept(this);
    }

public:

    // Runner.
    void run(ModuleStmt* moduleDecl) {
        llvm::outs() << "----------- AST dump --------------\n";
        result.clear();
        evaluate(moduleDecl);
        llvm::outs() << result;
        llvm::outs() << "-----------------------------------\n\n\n";
    }
};

}

#endif // ASTPRINTER_H
