#include "SemaCheck.h"

using namespace mxrlang;

void SemaCheck::visit(LiteralExpr* expr) {}

void SemaCheck::visit(VarExpr* expr) {
    // Report an error if we cannot find this declaration.
    if (!env->find(expr->getName()))
        diag.report(expr->getLoc(), DiagID::err_var_undefined);
}

void SemaCheck::visit(ModuleStmt* stmt) {
    SemaCheckScopeMgr scopeMgr(*this);
    for (auto st : stmt->getBody())
        evaluate(st);
}

void SemaCheck::visit(FunStmt* stmt) {
    SemaCheckScopeMgr scopeMgr(*this);

    // Reset the seenReturn flag at the beginning of each function.
    seenReturn = false;

    for (auto st : stmt->getBody())
        evaluate(st);

    // Every function must have a return statement.
    if (!seenReturn)
        diag.report(stmt->getLoc(), DiagID::err_no_return);
}

void SemaCheck::visit(ReturnStmt* stmt) {
    seenReturn = true;

    if (!stmt->getRetExpr())
        diag.report(stmt->getLoc(), DiagID::err_ret_val_undefined);
    else
        evaluate(stmt->getRetExpr());
}

void SemaCheck::visit(VarStmt* stmt) {
    // Report an error if this is a redefinition.
    if (!env->insert(stmt, stmt->getName()))
        diag.report(stmt->getLoc(), DiagID::err_var_redefine);
}
