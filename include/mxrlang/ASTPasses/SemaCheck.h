#ifndef SEMACHECK_H
#define SEMACHECK_H

#include "Diag.h"
#include "Environment.h"
#include "ScopeMgr.h"

namespace mxrlang {

class SemaCheck : public ExprVisitor,
                  public StmtVisitor {
    friend class ScopeMgr<SemaCheck, Decl>;
    using SemaCheckScopeMgr = ScopeMgr<SemaCheck, Decl>;

    Environment<Decl>* env = nullptr;

    Diag& diag;

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
    SemaCheck(Diag& diag) : diag(diag) {}

    // Runner.
    void run(ModuleStmt* moduleDecl) {
        evaluate(moduleDecl);
    }
};

} // namespace mxrlang

#endif // SEMACHECK_H
