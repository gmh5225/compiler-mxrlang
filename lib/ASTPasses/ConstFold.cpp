#include "ConstFold.h"

using namespace mxrlang;

void ConstFold::visit(AssignExpr* expr) {
    evaluate(expr->getSource());
    if (swap) {
        delete expr->getSource();
        expr->setSource(swap);
        swap = nullptr;
    }
}

void ConstFold::visit(BinaryArithExpr* expr) {
    // Evaluate the left and right side of the expression.
    evaluate(expr->getLeft());
    if (swap) {
        delete expr->getLeft();
        expr->setLeft(swap);
        swap = nullptr;
    }

    evaluate(expr->getRight());
    if (swap) {
        delete expr->getRight();
        expr->setRight(swap);
        swap = nullptr;
    }

    // Check whether both operands are literals.
    bool isConst = llvm::isa<IntLiteralExpr>(expr->getLeft()) &&
                   llvm::isa<IntLiteralExpr>(expr->getRight());

    if (!isConst)
        return;

    // If they are literals, perform the folding.
    auto* left = llvm::dyn_cast<IntLiteralExpr>(expr->getLeft());
    auto* right = llvm::dyn_cast<IntLiteralExpr>(expr->getRight());
    llvm::APSInt newApsInt;

    switch (expr->getBinaryKind()) {
    case BinaryArithExpr::BinaryArithExprKind::Add:
        newApsInt = left->getValue() + right->getValue();
        break;
    case BinaryArithExpr::BinaryArithExprKind::Div:
        newApsInt = left->getValue() / right->getValue();
        break;
    case BinaryArithExpr::BinaryArithExprKind::Mul:
        newApsInt = left->getValue() * right->getValue();
        break;
    case BinaryArithExpr::BinaryArithExprKind::Sub:
        newApsInt = left->getValue() - right->getValue();
        break;
    default:
        llvm_unreachable("Unexpected binary arithmetic expression kind.");
    }

    swap = new IntLiteralExpr(newApsInt, expr->getLoc(), expr->getParent());
}

void ConstFold::visit(BinaryLogicalExpr* expr) {
    // Evaluate the left and right side of the expression.
    evaluate(expr->getLeft());
    if (swap) {
        delete expr->getLeft();
        expr->setLeft(swap);
        swap = nullptr;
    }

    evaluate(expr->getRight());
    if (swap) {
        delete expr->getRight();
        expr->setRight(swap);
        swap = nullptr;
    }

    // If they are literals, perform the folding.
    auto* leftInt = llvm::dyn_cast<IntLiteralExpr>(expr->getLeft());
    auto* rightInt = llvm::dyn_cast<IntLiteralExpr>(expr->getRight());
    auto* leftBool = llvm::dyn_cast<BoolLiteralExpr>(expr->getLeft());
    auto* rightBool = llvm::dyn_cast<BoolLiteralExpr>(expr->getRight());
    bool isConstBool = leftBool != nullptr &&
                       rightBool != nullptr;
    bool isConstInt = leftInt != nullptr &&
                      rightInt != nullptr;

    switch (expr->getBinaryKind()) {
    case BinaryLogicalExpr::BinaryLogicalExprKind::And:
    case BinaryLogicalExpr::BinaryLogicalExprKind::Or: {
        if (!isConstBool)
            return;

        bool newValue;
        if (expr->getBinaryKind() == BinaryLogicalExpr::BinaryLogicalExprKind::And)
            newValue = leftBool->getValue() && rightBool->getValue();
        else
            newValue = leftBool->getValue() || rightBool->getValue();

        swap = new BoolLiteralExpr(newValue, expr->getLoc(), expr->getParent());
        break;
    }
    case BinaryLogicalExpr::BinaryLogicalExprKind::Greater:
    case BinaryLogicalExpr::BinaryLogicalExprKind::GreaterEq:
    case BinaryLogicalExpr::BinaryLogicalExprKind::Less:
    case BinaryLogicalExpr::BinaryLogicalExprKind::LessEq: {
        if (!isConstInt)
            return;

        bool newValue;
        if (expr->getBinaryKind() ==
                BinaryLogicalExpr::BinaryLogicalExprKind::Greater)
            newValue = leftInt->getValue() > rightInt->getValue();
        else if (expr->getBinaryKind() ==
                 BinaryLogicalExpr::BinaryLogicalExprKind::GreaterEq)
            newValue = leftInt->getValue() >= rightInt->getValue();
        else if (expr->getBinaryKind() ==
                 BinaryLogicalExpr::BinaryLogicalExprKind::Less)
            newValue = leftInt->getValue() < rightInt->getValue();
        else
            newValue = leftInt->getValue() <= rightInt->getValue();

        swap = new BoolLiteralExpr(newValue, expr->getLoc(), expr->getParent());
        break;
    }
    case BinaryLogicalExpr::BinaryLogicalExprKind::Eq:
    case BinaryLogicalExpr::BinaryLogicalExprKind::NotEq: {
        if (!isConstBool && !isConstInt)
            return;

        bool newValue;
        if (isConstBool)
            newValue = leftBool->getValue() == rightBool->getValue();
        else
            newValue = leftInt->getValue() == rightInt->getValue();

        newValue = expr->getBinaryKind() ==
            BinaryLogicalExpr::BinaryLogicalExprKind::NotEq ? !newValue : newValue;

        swap = new BoolLiteralExpr(newValue, expr->getLoc(), expr->getParent());
        break;
    }
    default:
        llvm_unreachable("Unexpected binary logical expression kind.");
    }
}

void ConstFold::visit(BoolLiteralExpr* expr) {}

void ConstFold::visit(CallExpr* expr) {
    for (auto it = expr->getArgs().begin(); it != expr->getArgs().end(); ++it) {
        evaluate(*it);
        if (swap) {
            delete *it;
            *it = swap;
            swap = nullptr;
        }
    }
}

void ConstFold::visit(GroupingExpr* expr) {
    evaluate(expr->getExpr());
    if (swap) {
        delete expr->getExpr();
        expr->setExpr(swap);
        swap = nullptr;
    }
}

void ConstFold::visit(IntLiteralExpr* expr) {}

void ConstFold::visit(PointerOpExpr* expr) {}

void ConstFold::visit(UnaryExpr* expr) {
    evaluate(expr->getExpr());
    if (swap) {
        delete expr->getExpr();
        expr->setExpr(swap);
        swap = nullptr;
    }
}

void ConstFold::visit(VarExpr* expr) {}

void ConstFold::visit(ExprStmt* stmt) {
    evaluate(stmt->getExpr());
    if (swap) {
        delete stmt->getExpr();
        stmt->setExpr(swap);
        swap = nullptr;
    }
}

void ConstFold::visit(IfStmt* stmt) {
    evaluate(stmt->getCond());
    if (swap) {
        delete stmt->getCond();
        stmt->setCond(swap);
        swap = nullptr;
    }

    for (auto it = stmt->getThenBody().begin();
              it != stmt->getThenBody().end();
              ++it) {
        evaluate(*it);
        if (swap) {
            delete *it;
            *it = swap;
            swap = nullptr;
        }
    }

    for (auto it = stmt->getElseBody().begin();
              it != stmt->getElseBody().end();
              ++it) {
        evaluate(*it);
        if (swap) {
            delete *it;
            *it = swap;
            swap = nullptr;
        }
    }
}

void ConstFold::visit(PrintStmt* stmt) {
    evaluate(stmt->getPrintExpr());
    if (swap) {
        delete stmt->getPrintExpr();
        stmt->setPrintExpr(swap);
        swap = nullptr;
    }
}

void ConstFold::visit(ReturnStmt* stmt) {
    evaluate(stmt->getRetExpr());
    if (swap) {
        delete stmt->getRetExpr();
        stmt->setRetExpr(swap);
        swap = nullptr;
    }
}

void ConstFold::visit(ScanStmt* stmt) {}

void ConstFold::visit(UntilStmt* stmt) {
    evaluate(stmt->getCond());
    if (swap) {
        delete stmt->getCond();
        stmt->setCond(swap);
        swap = nullptr;
    }

    for (auto it = stmt->getBody().begin();
              it != stmt->getBody().end();
              ++it) {
        evaluate(*it);
        if (swap) {
            delete *it;
            *it = swap;
            swap = nullptr;
        }
    }
}

void ConstFold::visit(FunDecl* decl) {
    for (auto it = decl->getBody().begin();
              it != decl->getBody().end();
              ++it) {
        evaluate(*it);
        if (swap) {
            delete *it;
            *it = swap;
            swap = nullptr;
        }
    }
}

void ConstFold::visit(ModuleDecl* decl) {
    for (auto* d : decl->getBody())
        evaluate(d);
}

void ConstFold::visit(VarDecl* decl) {
    if (decl->getInitializer()) {
        evaluate(decl->getInitializer());
        if (swap) {
            delete decl->getInitializer();
            decl->setInitializer(swap);
            swap = nullptr;
        }
    }
}
