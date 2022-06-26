#include "SemaCheck.h"

using namespace mxrlang;

// Report an error and throw an exception if we exceed a certain number
// of reported errors.
void SemaCheck::error(llvm::SMLoc loc, DiagID diagID) {
  diag.report(loc, diagID);
  if (diag.getNumErrs() > MAX_SEMANTIC_ERRS)
    throw SemaError();
}

// Check whether an expression is a valid assignment destination.
// This is a recursive function, so we can access the expression through
// ArrayAccess of PointerOp(Deref).
bool SemaCheck::isValidAssignDest(Expr *expr, bool arrayAccessOrDeref) {
  if (arrayAccessOrDeref) {
    if (llvm::isa<VarExpr>(expr) &&
        (expr->getType()->getTypeKind() == Type::TypeKind::Array ||
         expr->getType()->getTypeKind() == Type::TypeKind::Pointer))
      return true;
  } else {
    // Cannot assign to array variable, if we are not accessing through
    // ArrayAccess.
    if (llvm::isa<VarExpr>(expr) &&
        expr->getType()->getTypeKind() != Type::TypeKind::Array)
      return true;
  }

  if (auto *loadExpr = llvm::dyn_cast<LoadExpr>(expr))
    return isValidAssignDest(loadExpr->getExpr(), arrayAccessOrDeref);

  if (auto *groupingExpr = llvm::dyn_cast<GroupingExpr>(expr))
    return isValidAssignDest(groupingExpr->getExpr(), arrayAccessOrDeref);

  if (auto *arrayAccess = llvm::dyn_cast<ArrayAccessExpr>(expr))
    return isValidAssignDest(arrayAccess->getArray(), true);

  if (auto *pointerOp = llvm::dyn_cast<PointerOpExpr>(expr)) {
    if (pointerOp->getPointerOpKind() ==
        PointerOpExpr::PointerOpKind::Dereference)
      return isValidAssignDest(pointerOp->getExpr(), true);
  }

  return false;
}

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
    error(expr->getLoc(), DiagID::err_array_access_not_int);

  // We can only access expressions of array or pointer type.
  evaluate(expr->getArray());
  if (expr->getArray()->getType()->getTypeKind() == Type::TypeKind::Array ||
      expr->getArray()->getType()->getTypeKind() == Type::TypeKind::Pointer)
    expr->setType(expr->getArray()->getType()->getSubtype());
  else
    error(expr->getLoc(), DiagID::err_array_access_not_array);
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
  evaluate(expr->getDest());
  if (!isValidAssignDest(expr->getDest(), false)) {
    error(expr->getLoc(), DiagID::err_invalid_assign_target);
    return;
  }

  if (!Type::checkTypesMatching(expr->getDest()->getType(),
                                expr->getSource()->getType())) {
    error(expr->getLoc(), DiagID::err_incompatible_types);
    return;
  }

  expr->setType(expr->getDest()->getType());
}

void SemaCheck::visit(BinaryArithExpr *expr) {
  evaluate(expr->getLeft());
  evaluate(expr->getRight());

  auto *leftTy = expr->getLeft()->getType();
  auto *rightTy = expr->getRight()->getType();
  if (!Type::checkTypesMatching(leftTy, Type::getIntType()) ||
      !Type::checkTypesMatching(rightTy, Type::getIntType()) ||
      !Type::checkTypesMatching(leftTy, rightTy)) {
    error(expr->getLoc(), DiagID::err_arith_type);
    return;
  }

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
        !Type::checkTypesMatching(leftTy, rightTy)) {
      error(expr->getLoc(), DiagID::err_logic_type);
      return;
    }

    expr->setType(leftTy);
  } else if ((kind == BinaryLogicalExpr::BinaryLogicalExprKind::Eq) ||
             (kind == BinaryLogicalExpr::BinaryLogicalExprKind::NotEq)) {
    if (!Type::checkTypesMatching(leftTy, rightTy)) {
      error(expr->getLoc(), DiagID::err_incompatible_types);
      return;
    }

    expr->setType(Type::getBoolType());
  } else {
    if (!Type::checkTypesMatching(leftTy, Type::getIntType()) ||
        !Type::checkTypesMatching(rightTy, Type::getIntType()) ||
        !Type::checkTypesMatching(leftTy, rightTy)) {
      error(expr->getLoc(), DiagID::err_arith_type);
      return;
    }

    expr->setType(Type::getBoolType());
  }
}


void SemaCheck::visit(CallExpr *expr) {
  // Function should be declared at the module level.
  auto *funDecl = env->find(expr->getName());
  if (!funDecl) {
    error(expr->getLoc(), DiagID::err_fun_undefined);
    return;
  }

  FunDecl *funDeclCast = llvm::dyn_cast<FunDecl>(funDecl);
  assert(funDeclCast && "This must be a FunDecl.");

  // Function call and declaration must have a matching number of
  // arguments.
  if (funDeclCast->getArgs().size() != expr->getArgs().size()) {
    error(expr->getLoc(), DiagID::err_arg_num_mismatch);
    return;
  }

  // Argument types must match.
  for (size_t argNum = 0; argNum < expr->getArgs().size(); argNum++) {
    auto *callArg = expr->getArgs().at(argNum);
    auto *declArg = funDeclCast->getArgs().at(argNum);

    // Process the argument.
    evaluate(callArg);

    if (!Type::checkTypesMatching(callArg->getType(), declArg->getType()))
      error(expr->getLoc(), DiagID::err_arg_type_mismatch);
  }

  expr->setType(funDeclCast->getRetType());
}

void SemaCheck::visit(GroupingExpr *expr) {
  evaluate(expr->getExpr());
  expr->setType(expr->getExpr()->getType());
}


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
  evaluate(e);
  auto kind = expr->getPointerOpKind();

  if (kind == PointerOpExpr::PointerOpKind::AddressOf) {
    if (!e->canTakeAddressOf()) {
      error(expr->getLoc(), DiagID::err_addrof_target_not_mem);
      return;
    }

    auto *exprTy = e->getType();
    expr->setType(new PointerType(exprTy));
  } else {
    // We can only dereference expressions of pointer type.
    if (e->getType()->getTypeKind() != Type::TypeKind::Pointer) {
      error(expr->getLoc(), DiagID::err_deref_target_not_ptr_mem);
      return;
    }

    expr->setType(e->getType()->getSubtype());
  }
}

void SemaCheck::visit(UnaryExpr *expr) {
  evaluate(expr->getExpr());

  auto exprTy = expr->getExpr()->getType();
  auto kind = expr->getUnaryKind();
  if (kind == UnaryExpr::UnaryExprKind::NegArith) {
    if (!Type::checkTypesMatching(exprTy, Type::getIntType())) {
      error(expr->getLoc(), DiagID::err_arith_type);
      return;
    }
  } else {
    if (!Type::checkTypesMatching(exprTy, Type::getBoolType())) {
      error(expr->getLoc(), DiagID::err_logic_type);
      return;
    }
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
    error(stmt->getLoc(), DiagID::err_cond_not_bool);

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

  if (!stmt->getRetExpr()) {
    error(stmt->getLoc(), DiagID::err_ret_val_undefined);
    return;
  } else
    evaluate(stmt->getRetExpr());

  if (!Type::checkTypesMatching(currFun->getRetType(),
                                stmt->getRetExpr()->getType()))
    error(stmt->getLoc(), DiagID::err_ret_type_mismatch);
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

void SemaCheck::visit(WhileStmt *stmt) {
  evaluate(stmt->getCond());
  if (!Type::checkTypesMatching(stmt->getCond()->getType(),
                                Type::getBoolType()))
    error(stmt->getLoc(), DiagID::err_cond_not_bool);

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

  // Return value must not be of array type.
  if (decl->getRetType()->getTypeKind() == Type::TypeKind::Array)
    error(decl->getLoc(), DiagID::err_ret_type_array);

  // Every function must have a return statement.
  if (!seenReturn)
    error(decl->getLoc(), DiagID::err_no_return);
}

void SemaCheck::visit(ModuleDecl *decl) {
  try {
    SemaCheckScopeMgr scopeMgr(*this);
    // Forward declare everything.
    for (auto dec : decl->getBody()) {
      // Report an error if this is a redefinition.
      if (!env->insert(dec, dec->getName())) {
        DiagID errId = llvm::isa<FunDecl>(dec) ? DiagID::err_fun_redefine
                                               : DiagID::err_var_redefine;
        error(dec->getLoc(), errId);
      }
    }

    for (auto dec : decl->getBody())
      evaluate(dec);
  } catch (SemaError &e) {
    llvm::errs() << "Exceeding maximum number of semantic errors. Aborting "
                    "compilation...";
    return;
  }
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
      error(decl->getLoc(), DiagID::err_var_redefine);
  }

  // Initializer must have a compatible type.
  if (decl->getInitializer() &&
      !Type::checkTypesMatching(decl->getType(),
                                decl->getInitializer()->getType()))
    error(decl->getLoc(), DiagID::err_incompatible_types);

  // If this is a global variable, the initializer must be an INT or BOOL
  // literal.
  if (decl->isGlobal()) {
    if (Type::checkTypesMatching(decl->getType(), Type::getIntType()) &&
        !llvm::isa<IntLiteralExpr>(decl->getInitializer()))
      error(decl->getLoc(), DiagID::err_global_init_not_lit);

    if (Type::checkTypesMatching(decl->getType(), Type::getBoolType()) &&
        !llvm::isa<BoolLiteralExpr>(decl->getInitializer()))
      error(decl->getLoc(), DiagID::err_global_init_not_lit);
  }
}
