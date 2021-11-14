#include "SemaCheck.h"

using namespace mxrlang;

void SemaCheck::visit(LiteralExpr* expr) {}

void SemaCheck::visit(ModuleStmt* stmt) {
    SemaCheckScopeMgr scopeMgr(*this);
    for (auto st : stmt->getBody())
        evaluate(st);
}

void SemaCheck::visit(FunStmt* stmt) {
    SemaCheckScopeMgr scopeMgr(*this);
    for (auto st : stmt->getBody())
        evaluate(st);
}

void SemaCheck::visit(VarStmt* stmt) {
    if (!env->insert(stmt, stmt->getName()))
        diag.report(stmt->getLoc(), DiagID::err_var_redefine);
}
