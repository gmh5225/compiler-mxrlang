#include "SemaCheck.h"

using namespace mxrlang;

void SemaCheck::visit(ArrayAccessExpr *expr) {
  // We don't need to load an array before accessing it
  //
  // ArrayAccessExpr -> LoadExpr -> VarExpr
  // |
  // v
  // ArrayAccessExpr -> VarExpr
  //
  // After accessing, we will load the value if needed.
  if (auto *loadExpr = llvm::dyn_cast<LoadExpr>(expr->getArray())) {
    expr->setArray(loadExpr->getExpr());
    delete loadExpr;
  }

  // Element must be an INT.
  evaluate(expr->getElement());
  if (!Type::checkTypesMatching(expr->getElement()->getType(),
                                Type::getIntType()))
    diag.report(expr->getLoc(), DiagID::err_array_access_not_int);

  // We can only access array variables or other ArrayAccessExpr's...
  if (!llvm::isa<VarExpr>(expr->getArray()) &&
      !llvm::isa<ArrayAccessExpr>(expr->getArray()))
    diag.report(expr->getLoc(), DiagID::err_array_access_not_array);
  evaluate(expr->getArray());

  // ... of array type.
  ArrayType *type = llvm::dyn_cast<ArrayType>(expr->getArray()->getType());
  if (!type)
    diag.report(expr->getLoc(), DiagID::err_array_access_not_array);

  expr->setType(type->getArrayType());
}

void SemaCheck::visit(AssignExpr *expr) {
  evaluate(expr->getSource());

  // If the destination is LoadExpr, remove it from the AST, since we do not
  // load the values we are assigning to.
  if (auto *loadExpr = llvm::dyn_cast<LoadExpr>(expr->getDest())) {
    expr->setDest(loadExpr->getExpr());
    delete loadExpr;
  }

  // Evaluating assignment destination.
  if (!expr->getDest()->isValidAssignDest())
    diag.report(expr->getLoc(), DiagID::err_invalid_assign_target);

  evaluate(expr->getDest());

  if (!Type::checkTypesMatching(expr->getDest()->getType(),
                                expr->getSource()->getType()))
    diag.report(expr->getLoc(), DiagID::err_incompatible_types);

  expr->setType(expr->getDest()->getType());
}

void SemaCheck::visit(BinaryArithExpr *expr) {
  evaluate(expr->getLeft());
  evaluate(expr->getRight());

  auto *leftTy = expr->getLeft()->getType();
  auto *rightTy = expr->getRight()->getType();
  if (!Type::checkTypesMatching(leftTy, Type::getIntType()) ||
      !Type::checkTypesMatching(rightTy, Type::getIntType()) ||
      !Type::checkTypesMatching(leftTy, rightTy))
    diag.report(expr->getLoc(), DiagID::err_arith_type);

  expr->setType(leftTy);
}

void SemaCheck::visit(BinaryLogicalExpr *expr) {
  evaluate(expr->getLeft());
  evaluate(expr->getRight());

  auto *leftTy = expr->getLeft()->getType();
  auto *rightTy = expr->getRight()->getType();
  auto kind = expr->getBinaryKind();
  if ((kind == BinaryLogicalExpr::BinaryLogicalExprKind::And) ||
      (kind == BinaryLogicalExpr::BinaryLogicalExprKind::Or)) {
    if (!Type::checkTypesMatching(leftTy, Type::getBoolType()) ||
        !Type::checkTypesMatching(rightTy, Type::getBoolType()) ||
        !Type::checkTypesMatching(leftTy, rightTy))
      diag.report(expr->getLoc(), DiagID::err_logic_type);

    expr->setType(leftTy);
  } else if ((kind == BinaryLogicalExpr::BinaryLogicalExprKind::Eq) ||
             (kind == BinaryLogicalExpr::BinaryLogicalExprKind::NotEq)) {
    if (!Type::checkTypesMatching(leftTy, rightTy))
      diag.report(expr->getLoc(), DiagID::err_incompatible_types);

    expr->setType(Type::getBoolType());
  } else {
    if (!Type::checkTypesMatching(leftTy, Type::getIntType()) ||
        !Type::checkTypesMatching(rightTy, Type::getIntType()) ||
        !Type::checkTypesMatching(leftTy, rightTy))
      diag.report(expr->getLoc(), DiagID::err_arith_type);

    expr->setType(Type::getBoolType());
  }
}

void SemaCheck::visit(BoolLiteralExpr *expr) {}

void SemaCheck::visit(CallExpr *expr) {
  // Function should be declared at the module level.
  auto *funDecl = env->find(expr->getName());
  if (!funDecl) {
    diag.report(expr->getLoc(), DiagID::err_fun_undefined);
    return;
  }

  FunDecl *funDeclCast = llvm::dyn_cast<FunDecl>(funDecl);
  assert(funDeclCast && "This must be a FunDecl.");

  // Function call and declaration must have a matching number of
  // arguments.
  if (funDeclCast->getArgs().size() != expr->getArgs().size()) {
    diag.report(expr->getLoc(), DiagID::err_arg_num_mismatch);
    return;
  }

  // Argument types must match.
  for (size_t argNum = 0; argNum < expr->getArgs().size(); argNum++) {
    auto *callArg = expr->getArgs().at(argNum);
    auto *declArg = funDeclCast->getArgs().at(argNum);

    // Process the argument.
    evaluate(callArg);

    if (!Type::checkTypesMatching(callArg->getType(), declArg->getType()))
      diag.report(expr->getLoc(), DiagID::err_arg_type_mismatch);
  }

  expr->setType(funDeclCast->getRetType());
}

void SemaCheck::visit(GroupingExpr *expr) {
  evaluate(expr->getExpr());

  expr->setType(expr->getExpr()->getType());
}

void SemaCheck::visit(IntLiteralExpr *expr) {}

void SemaCheck::visit(LoadExpr *expr) {
  evaluate(expr->getExpr());
  expr->setType(expr->getExpr()->getType());
}

void SemaCheck::visit(PointerOpExpr *expr) {
  // We don't need to load a variable before dereferencing it/taking its
  // address.
  //
  // PointerOpExpr -> LoadExpr -> VarExpr
  // |
  // v
  // PointerOpExpr -> VarExpr
  //
  // After dereferencing, we will load the value if needed.
  if (auto *loadExpr = llvm::dyn_cast<LoadExpr>(expr->getExpr())) {
    expr->setExpr(loadExpr->getExpr());
    delete loadExpr;
  }

  auto *e = expr->getExpr();
  auto kind = expr->getPointerOpKind();

  if (kind == PointerOpExpr::PointerOpKind::AddressOf) {
    // We can only take the address of a VarExpr.
    auto *var = llvm::dyn_cast<VarExpr>(e);
    if (!var)
      diag.report(expr->getLoc(), DiagID::err_addrof_target_not_var);

    evaluate(var);

    auto *exprTy = var->getType();
    expr->setType(new PointerType(exprTy));
  } else {
    // We can only dereference a VarExpr (through LoadExpr)...
    auto *var = llvm::dyn_cast<VarExpr>(e);
    if (!var)
      diag.report(expr->getLoc(), DiagID::err_deref_target_not_ptr_var);

    evaluate(var);

    // ... of pointer type.
    auto *exprTy = llvm::dyn_cast<PointerType>(var->getType());
    if (!exprTy)
      diag.report(expr->getLoc(), DiagID::err_deref_target_not_ptr_var);

    expr->setType(exprTy->getPointeeType());
  }
}

void SemaCheck::visit(UnaryExpr *expr) {
  evaluate(expr->getExpr());

  auto exprTy = expr->getExpr()->getType();
  auto kind = expr->getUnaryKind();
  if (kind == UnaryExpr::UnaryExprKind::NegArith) {
    if (!Type::checkTypesMatching(exprTy, Type::getIntType()))
      diag.report(expr->getLoc(), DiagID::err_arith_type);
  } else {
    if (!Type::checkTypesMatching(exprTy, Type::getBoolType()))
      diag.report(expr->getLoc(), DiagID::err_logic_type);
  }

  expr->setType(exprTy);
}

void SemaCheck::visit(VarExpr *expr) {
  // Report an error if we cannot find this declaration.
  auto *varDecl = env->find(expr->getName());
  if (!varDecl) {
    diag.report(expr->getLoc(), DiagID::err_var_undefined);
    return;
  }

  // Match the type of the VarExpr with that of the actual variable
  // declaration.
  auto *varDeclCast = llvm::dyn_cast<VarDecl>(varDecl);
  assert(varDeclCast && "This must be a VarDecl");
  expr->setType(varDeclCast->getType());
}

void SemaCheck::visit(ExprStmt *stmt) { evaluate(stmt->getExpr()); }

void SemaCheck::visit(IfStmt *stmt) {
  evaluate(stmt->getCond());
  if (!Type::checkTypesMatching(stmt->getCond()->getType(),
                                Type::getBoolType()))
    diag.report(stmt->getLoc(), DiagID::err_cond_not_bool);

  // Use RAII to manage the lifetime of scopes.
  {
    SemaCheckScopeMgr ScopeMgr(*this);
    for (auto *thenStmt : stmt->getThenBody())
      evaluate(thenStmt);
  }

  {
    SemaCheckScopeMgr ScopeMgr(*this);
    for (auto *elseStmt : stmt->getElseBody())
      evaluate(elseStmt);
  }
}

void SemaCheck::visit(PrintStmt *stmt) { evaluate(stmt->getPrintExpr()); }

void SemaCheck::visit(ReturnStmt *stmt) {
  seenReturn = true;

  if (!stmt->getRetExpr())
    diag.report(stmt->getLoc(), DiagID::err_ret_val_undefined);
  else
    evaluate(stmt->getRetExpr());

  assert(!Type::checkTypesMatching(stmt->getRetExpr()->getType(),
                                   Type::getNoneType()));
  if (!Type::checkTypesMatching(currFun->getRetType(),
                                stmt->getRetExpr()->getType()))
    diag.report(stmt->getLoc(), DiagID::err_ret_type_mismatch);
}

void SemaCheck::visit(ScanStmt *stmt) {
  evaluate(stmt->getScanVar());

  // Don't load an expression here, because the expression being scanned
  // is essentially being assigned to.
  if (auto *loadExpr = llvm::dyn_cast<LoadExpr>(stmt->getScanVar())) {
    stmt->setScanVar(loadExpr->getExpr());
    delete loadExpr;
  }
}

void SemaCheck::visit(UntilStmt *stmt) {
  evaluate(stmt->getCond());
  if (!Type::checkTypesMatching(stmt->getCond()->getType(),
                                Type::getBoolType()))
    diag.report(stmt->getLoc(), DiagID::err_cond_not_bool);

  // Use RAII to manage the lifetime of scopes.
  {
    SemaCheckScopeMgr ScopeMgr(*this);
    for (auto *s : stmt->getBody())
      evaluate(s);
  }
}

void SemaCheck::visit(FunDecl *decl) {
  SemaCheckScopeMgr scopeMgr(*this);

  currFun = decl;

  // Reset the seenReturn flag at the beginning of each function.
  seenReturn = false;

  // Register the arguments.
  for (auto arg : decl->getArgs())
    evaluate(arg);

  for (auto st : decl->getBody())
    evaluate(st);

  // Every function must have a return statement.
  if (!seenReturn)
    diag.report(decl->getLoc(), DiagID::err_no_return);
}

void SemaCheck::visit(ModuleDecl *decl) {
  SemaCheckScopeMgr scopeMgr(*this);
  // Forward declare everything.
  for (auto dec : decl->getBody()) {
    // Report an error if this is a redefinition.
    if (!env->insert(dec, dec->getName())) {
      DiagID errId = llvm::isa<FunDecl>(dec) ? DiagID::err_fun_redefine
                                             : DiagID::err_var_redefine;
      diag.report(dec->getLoc(), errId);
    }
  }

  for (auto dec : decl->getBody())
    evaluate(dec);
}

void SemaCheck::visit(VarDecl *decl) {
  // First check the initializer, in case the variable is referencing itself.
  if (decl->getInitializer())
    evaluate(decl->getInitializer());

  // Report an error if this is a redefinition.
  // Only do this for locals, as globals will be forward declared at the
  // module level.
  if (!decl->isGlobal()) {
    if (!env->insert(decl, decl->getName()))
      diag.report(decl->getLoc(), DiagID::err_var_redefine);
  }

  // Initializer must have a compatible type.
  if (decl->getInitializer() &&
      !Type::checkTypesMatching(decl->getType(),
                                decl->getInitializer()->getType()))
    diag.report(decl->getLoc(), DiagID::err_incompatible_types);

  // If this is a global variable, the initializer must be an INT or BOOL
  // literal.
  if (decl->isGlobal()) {
    if (Type::checkTypesMatching(decl->getType(), Type::getIntType()) &&
        !llvm::isa<IntLiteralExpr>(decl->getInitializer()))
      diag.report(decl->getLoc(), DiagID::err_global_init_not_lit);

    if (Type::checkTypesMatching(decl->getType(), Type::getBoolType()) &&
        !llvm::isa<BoolLiteralExpr>(decl->getInitializer()))
      diag.report(decl->getLoc(), DiagID::err_global_init_not_lit);
  }
}
