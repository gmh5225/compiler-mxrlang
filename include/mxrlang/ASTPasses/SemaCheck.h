#ifndef SEMACHECK_H
#define SEMACHECK_H

#include "Diag.h"
#include "ScopeMgr.h"

namespace mxrlang {

class SemaCheck : public Visitor {
    friend class ScopeMgr<SemaCheck, Decl>;
    using SemaCheckScopeMgr = ScopeMgr<SemaCheck, Decl>;

    Environment<Decl>* env = nullptr;

    Diag& diag;

    // Flags whether we've seen a return statement in a function.
    bool seenReturn = false;

    // Currently checked function.
    FunDecl* currFun = nullptr;

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

    void visit(FunDecl* decl) override;
    void visit(ModuleDecl* decl) override;
    void visit(VarDecl* decl) override;

    // Helper function for evaluating an expression or a statement.
    template <typename T>
    void evaluate(const T expr) {
        expr->accept(this);
    }

public:
    SemaCheck(Diag& diag) : diag(diag) {}

    // Runner.
    void run(ModuleDecl* moduleDecl) {
        evaluate(moduleDecl);
    }
};

} // namespace mxrlang

#endif // SEMACHECK_H
