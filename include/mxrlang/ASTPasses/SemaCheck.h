#ifndef SEMACHECK_H
#define SEMACHECK_H

#include "Diag.h"
#include "ScopeMgr.h"

#define MAX_SEMANTIC_ERRS 5

namespace mxrlang {

class SemaCheck : public Visitor {
  friend class ScopeMgr<SemaCheck, Decl>;
  using SemaCheckScopeMgr = ScopeMgr<SemaCheck, Decl>;

  // Custom semantic check exception class.
  class SemaError : public std::exception {};

  Environment<Decl> *env = nullptr;

  Diag &diag;

  // Flags whether we've seen a return statement in a function.
  bool seenReturn = false;

  // Currently checked function.
  FunDecl *currFun = nullptr;

  // Expression visitor methods
  void visit(ArrayAccessExpr *expr) override;
  void visit(ArrayInitExpr *expr) override;
  void visit(AssignExpr *expr) override;
  void visit(BinaryArithExpr *expr) override;
  void visit(BinaryLogicalExpr *expr) override;
  void visit(CallExpr *expr) override;
  void visit(GroupingExpr *expr) override;
  void visit(LoadExpr *expr) override;
  void visit(PointerOpExpr *expr) override;
  void visit(UnaryExpr *expr) override;
  void visit(VarExpr *expr) override;

  // Statement visitor methods
  void visit(ExprStmt *stmt) override;
  void visit(IfStmt *stmt) override;
  void visit(PrintStmt *stmt) override;
  void visit(ReturnStmt *stmt) override;
  void visit(ScanStmt *stmt) override;
  void visit(WhileStmt *stmt) override;

  void visit(FunDecl *decl) override;
  void visit(ModuleDecl *decl) override;
  void visit(VarDecl *decl) override;

  // Report an error and throw an exception if we exceed a certain number
  // of reported errors.
  void error(llvm::SMLoc loc, DiagID diagID);

  // Check whether an expression is a valid assignment destination.
  // This is a recursive function, so we can access the expression through
  // ArrayAccess of PointerOp(Deref).
  bool isValidAssignDest(Expr *expr, bool arrayAccessOrDeref);

  // Helper function for evaluating an expression or a statement.
  template <typename T> void evaluate(const T expr) { expr->accept(this); }

public:
  SemaCheck(Diag &diag) : diag(diag) {}

  // Runner.
  void run(ModuleDecl *moduleDecl) { evaluate(moduleDecl); }
};

} // namespace mxrlang

#endif // SEMACHECK_H
