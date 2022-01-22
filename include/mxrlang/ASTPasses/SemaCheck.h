#ifndef SEMACHECK_H
#define SEMACHECK_H

#include "Diag.h"
#include "ScopeMgr.h"

namespace mxrlang {

class SemaCheck : public ExprVisitor,
                  public StmtVisitor {
    friend class ScopeMgr<SemaCheck, Decl>;
    using SemaCheckScopeMgr = ScopeMgr<SemaCheck, Decl>;

    Environment<Decl>* env = nullptr;

    Diag& diag;

    // Flags whether we've seen a return statement in a function.
    bool seenReturn = false;

    // Expression visitor methods
    void visit(AssignExpr* expr) override;
    void visit(BinaryArithExpr* expr) override;
    void visit(BoolLiteralExpr* expr) override;
    void visit(GroupingExpr* expr) override;
    void visit(IntLiteralExpr* expr) override;
    void visit(VarExpr* expr) override;

    // Statement visitor methods
    void visit(ExprStmt* stmt) override;
    void visit(FunStmt* stmt) override;
    void visit(IfStmt* stmt) override;
    void visit(ModuleStmt* stmt) override;
    void visit(PrintStmt* stmt) override;
    void visit(ReturnStmt* stmt) override;
    void visit(VarStmt* stmt) override;

    // Helper function for evaluating an expression or a statement.
    template <typename T>
    void evaluate(const T expr) {
        expr->accept(this);
    }

public:
    SemaCheck(Diag& diag) : diag(diag) {}

    // Runner.
    void run(ModuleStmt* moduleDecl) {
        evaluate(moduleDecl);
    }
};

} // namespace mxrlang

#endif // SEMACHECK_H
