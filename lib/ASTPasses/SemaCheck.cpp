#include "SemaCheck.h"

using namespace mxrlang;

void SemaCheck::visit(BoolLiteralExpr* expr) {}

void SemaCheck::visit(IntLiteralExpr* expr) {}

void SemaCheck::visit(VarExpr* expr) {
    // Report an error if we cannot find this declaration.
    auto* varDecl = env->find(expr->getName());
    if (!varDecl)
        diag.report(expr->getLoc(), DiagID::err_var_undefined);

    // Match the type of the VarExpr with that of the actual variable
    // declaration.
    expr->setType(varDecl->getType());
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

    // FIXME: Currently only INT expected as return type.
    if (stmt->getRetExpr()->getType() != Type::getIntType())
        diag.report(stmt->getLoc(), DiagID::err_ret_type_mismatch);
}

void SemaCheck::visit(VarStmt* stmt) {
    // Report an error if this is a redefinition.
    if (!env->insert(stmt, stmt->getName()))
        diag.report(stmt->getLoc(), DiagID::err_var_redefine);

    if (stmt->getInitializer())
        evaluate(stmt->getInitializer());

    // Initializer must have a compatible type.
    if (stmt->getInitializer() &&
        (stmt->getType() != stmt->getInitializer()->getType()))
        diag.report(stmt->getLoc(), DiagID::err_incompatible_types);
}
