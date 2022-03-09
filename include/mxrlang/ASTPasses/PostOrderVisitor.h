#ifndef POSTORDERVISITOR_H
#define POSTORDERVISITOR_H

#include "Tree.h"

namespace mxrlang {

class PostOrderVisitor : public Visitor {
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

    // A vector containing AST nodes in post-order traversal.
    Nodes postOrder;

    // Declaration of the module to be visited.
    ModuleDecl* moduleDecl;

    // Helper function for evaluating an expression or a statement.
    template <typename T>
    void evaluate(const T expr) {
        expr->accept(this);
    }

public:
    PostOrderVisitor(ModuleDecl* moduleDecl)
        : moduleDecl(moduleDecl) {}

    Nodes& getPostOrder() {
        if (postOrder.empty())
            evaluate(moduleDecl);
        return postOrder;
    }
};

}

#endif // POSTORDERVISITOR_H
