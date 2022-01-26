#include "SemaCheck.h"

using namespace mxrlang;

void SemaCheck::visit(AssignExpr* expr) {
    evaluate(expr->getDest());
    evaluate(expr->getSource());

    if (expr->getDest()->getType() != expr->getSource()->getType())
        diag.report(expr->getLoc(), DiagID::err_ret_type_mismatch);

    expr->setType(expr->getDest()->getType());
}

void SemaCheck::visit(BinaryArithExpr* expr) {
    evaluate(expr->getLeft());
    evaluate(expr->getRight());

    auto* leftTy = expr->getLeft()->getType();
    auto* rightTy = expr->getRight()->getType();
    if ((leftTy != Type::getIntType()) ||
        (rightTy != Type::getIntType()) ||
        (leftTy != rightTy))
        diag.report(expr->getLoc(), DiagID::err_arith_type);

    expr->setType(leftTy);
}

void SemaCheck::visit(BinaryLogicalExpr* expr) {
    evaluate(expr->getLeft());
    evaluate(expr->getRight());

    auto* leftTy = expr->getLeft()->getType();
    auto* rightTy = expr->getRight()->getType();
    auto kind = expr->getBinaryKind();
    if ((kind == BinaryLogicalExpr::BinaryLogicalExprKind::And) ||
        (kind == BinaryLogicalExpr::BinaryLogicalExprKind::Or)) {
        if ((leftTy != Type::getBoolType()) ||
            (rightTy != Type::getBoolType()) ||
            (leftTy != rightTy))
            diag.report(expr->getLoc(), DiagID::err_logic_type);

        expr->setType(leftTy);
    } else if ((kind == BinaryLogicalExpr::BinaryLogicalExprKind::Eq) ||
               (kind == BinaryLogicalExpr::BinaryLogicalExprKind::NotEq)) {
        if (leftTy != rightTy)
            diag.report(expr->getLoc(), DiagID::err_incompatible_types);

        expr->setType(Type::getBoolType());
    } else {
        if ((leftTy != Type::getIntType()) ||
            (rightTy != Type::getIntType()) ||
            (leftTy != rightTy))
            diag.report(expr->getLoc(), DiagID::err_arith_type);

        expr->setType(Type::getBoolType());
    }
}

void SemaCheck::visit(BoolLiteralExpr* expr) {}

void SemaCheck::visit(GroupingExpr* expr) {
    evaluate(expr->getExpr());

    expr->setType(expr->getExpr()->getType());
}

void SemaCheck::visit(IntLiteralExpr* expr) {}

void SemaCheck::visit(UnaryExpr* expr) {
    evaluate(expr->getExpr());

    auto exprTy = expr->getExpr()->getType();
    auto kind = expr->getUnaryKind();
    if (kind == UnaryExpr::UnaryExprKind::NegArith) {
        if (exprTy != Type::getIntType())
            diag.report(expr->getLoc(), DiagID::err_arith_type);
    } else {
        if (exprTy != Type::getBoolType())
            diag.report(expr->getLoc(), DiagID::err_logic_type);
    }

    expr->setType(exprTy);
}

void SemaCheck::visit(VarExpr* expr) {
    // Report an error if we cannot find this declaration.
    auto* varDecl = env->find(expr->getName());
    if (!varDecl) {
        diag.report(expr->getLoc(), DiagID::err_var_undefined);
        return;
    }

    // Match the type of the VarExpr with that of the actual variable
    // declaration.
    expr->setType(varDecl->getType());
}

void SemaCheck::visit(ExprStmt* stmt) { evaluate(stmt->getExpr()); }

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

void SemaCheck::visit(IfStmt* stmt) {
    evaluate(stmt->getCond());
    if (!(stmt->getCond()->getType() == Type::getBoolType()))
        diag.report(stmt->getLoc(), DiagID::err_cond_not_bool);

    // Use RAII to manage the lifetime of scopes.
    {
        SemaCheckScopeMgr ScopeMgr(*this);
        for (auto* thenStmt : stmt->getThenStmts()) {
            evaluate(thenStmt);
        }
    }

    {
        SemaCheckScopeMgr ScopeMgr(*this);
        for (auto* elseStmt : stmt->getElseStmts()) {
            evaluate(elseStmt);
        }
    }
}

void SemaCheck::visit(ModuleStmt* stmt) {
    SemaCheckScopeMgr scopeMgr(*this);
    for (auto st : stmt->getBody())
        evaluate(st);
}

void SemaCheck::visit(PrintStmt* stmt) {
    evaluate(stmt->getPrintExpr());
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
