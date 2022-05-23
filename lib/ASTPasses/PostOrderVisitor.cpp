#include "PostOrderVisitor.h"

using namespace mxrlang;

void PostOrderVisitor::visit(AssignExpr *expr) {
  evaluate(expr->getDest());
  evaluate(expr->getSource());

  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(BinaryArithExpr *expr) {
  evaluate(expr->getLeft());
  evaluate(expr->getRight());

  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(BinaryLogicalExpr *expr) {
  evaluate(expr->getLeft());
  evaluate(expr->getRight());

  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(BoolLiteralExpr *expr) {
  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(CallExpr *expr) {
  for (auto *arg : expr->getArgs())
    evaluate(arg);

  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(GroupingExpr *expr) {
  evaluate(expr->getExpr());
  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(IntLiteralExpr *expr) {
  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(LoadExpr *expr) {
  evaluate(expr->getExpr());
  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(PointerOpExpr *expr) {
  evaluate(expr->getExpr());
  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(UnaryExpr *expr) {
  evaluate(expr->getExpr());
  postOrder.emplace_back(expr);
}

void PostOrderVisitor::visit(VarExpr *expr) { postOrder.emplace_back(expr); }

void PostOrderVisitor::visit(ExprStmt *stmt) {
  evaluate(stmt->getExpr());
  postOrder.emplace_back(stmt);
}

void PostOrderVisitor::visit(IfStmt *stmt) {
  evaluate(stmt->getCond());

  for (auto *node : stmt->getThenBody())
    evaluate(node);

  for (auto *node : stmt->getElseBody())
    evaluate(node);

  postOrder.emplace_back(stmt);
}

void PostOrderVisitor::visit(PrintStmt *stmt) {
  evaluate(stmt->getPrintExpr());
  postOrder.emplace_back(stmt);
}

void PostOrderVisitor::visit(ReturnStmt *stmt) {
  evaluate(stmt->getRetExpr());
  postOrder.emplace_back(stmt);
}

void PostOrderVisitor::visit(ScanStmt *stmt) {
  evaluate(stmt->getScanVar());
  postOrder.emplace_back(stmt);
}

void PostOrderVisitor::visit(UntilStmt *stmt) {
  evaluate(stmt->getCond());

  for (auto *node : stmt->getBody())
    evaluate(node);

  postOrder.emplace_back(stmt);
}

void PostOrderVisitor::visit(FunDecl *decl) {
  for (auto *arg : decl->getArgs())
    evaluate(arg);

  for (auto *node : decl->getBody())
    evaluate(node);

  postOrder.emplace_back(decl);
}

void PostOrderVisitor::visit(ModuleDecl *decl) {
  for (auto *node : decl->getBody())
    evaluate(node);

  postOrder.emplace_back(decl);
}

void PostOrderVisitor::visit(VarDecl *decl) {
  if (decl->getInitializer())
    evaluate(decl->getInitializer());

  postOrder.emplace_back(decl);
}
