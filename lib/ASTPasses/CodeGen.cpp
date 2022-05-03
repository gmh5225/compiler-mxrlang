#include "CodeGen.h"

using namespace mxrlang;

void CodeGen::createPrintScanFunctions() {
    llvm::ArrayRef<llvm::Type*> argTys = {llvm::Type::getInt8PtrTy(ctx)};
    auto* funTy = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx),
                                          argTys, true);

    printFun = llvm::Function::Create(funTy,
                                      llvm::GlobalValue::ExternalLinkage,
                                      "printf", module.get());
    scanFun = llvm::Function::Create(funTy,
                                    llvm::GlobalValue::ExternalLinkage,
                                    "__isoc99_scanf", module.get());

    printFormatStr = builder.CreateGlobalStringPtr(llvm::StringRef("%lld\n"),
                                                   "formatstr", 0, module.get());
    scanFormatStr = builder.CreateGlobalStringPtr(llvm::StringRef("%lld"),
                                                  "formatstr", 0, module.get());
}

llvm::FunctionType* CodeGen::createFunctionType(FunDecl* decl) {
    auto* retTy = decl->getRetType()->getLLVMType(ctx);

    // Convert argument types to LLVM types.
    std::vector<llvm::Type*> args;
    for (auto arg : decl->getArgs())
        args.push_back(arg->getType()->getLLVMType(ctx));

    return llvm::FunctionType::get(retTy, args, /*isVarArg*/ false);
}

llvm::Function *CodeGen::createFunction(FunDecl* decl,
                                        llvm::FunctionType* type) {
    return llvm::Function::Create(type, llvm::GlobalValue::ExternalLinkage,
                                  decl->getName(), module.get());
}

void CodeGen::visit(AssignExpr* expr) {
    evaluate(expr->getSource());
    auto* source = interResult;

    for (auto* dest : expr->getDests()) {
        evaluate(dest);
        auto* destVal = interResult;
        builder.CreateStore(source, destVal);
    }
}

void CodeGen::visit(BinaryArithExpr *expr) {
    llvm::Value* left = nullptr;
    llvm::Value* right = nullptr;

    evaluate(expr->getLeft());
    left = interResult;
    evaluate(expr->getRight());
    right = interResult;

    switch (expr->getBinaryKind()) {
    case BinaryArithExpr::BinaryArithExprKind::Add:
        interResult = builder.CreateAdd(left, right, "add");
        break;
    case BinaryArithExpr::BinaryArithExprKind::Div:
        interResult = builder.CreateSDiv(left, right, "sdiv");
        break;
    case BinaryArithExpr::BinaryArithExprKind::Mul:
        interResult = builder.CreateMul(left, right, "mul");
        break;
    case BinaryArithExpr::BinaryArithExprKind::Sub:
        interResult = builder.CreateSub(left, right, "sub");
        break;
    default:
        llvm_unreachable("Unexpected binary arithmetic expression kind.");
    }
}

void CodeGen::visit(BinaryLogicalExpr* expr) {
    llvm::Value* left = nullptr;
    llvm::Value* right = nullptr;

    evaluate(expr->getLeft());
    left = interResult;
    evaluate(expr->getRight());
    right = interResult;

    switch (expr->getBinaryKind()) {
    case BinaryLogicalExpr::BinaryLogicalExprKind::And:
        interResult = builder.CreateAnd(left, right, "and");
        break;
    case BinaryLogicalExpr::BinaryLogicalExprKind::Eq:
        interResult = builder.CreateICmpEQ(left, right, "eq");
        break;
    case BinaryLogicalExpr::BinaryLogicalExprKind::Greater:
        interResult = builder.CreateICmpSGT(left, right, "greater");
        break;
    case BinaryLogicalExpr::BinaryLogicalExprKind::GreaterEq:
        interResult = builder.CreateICmpSGE(left, right, "greatereq");
        break;
    case BinaryLogicalExpr::BinaryLogicalExprKind::Less:
        interResult = builder.CreateICmpSLT(left, right, "less");
        break;
    case BinaryLogicalExpr::BinaryLogicalExprKind::LessEq:
        interResult = builder.CreateICmpSLE(left, right, "lesseq");
        break;
    case BinaryLogicalExpr::BinaryLogicalExprKind::NotEq:
        interResult = builder.CreateICmpNE(left, right, "noteq");
        break;
    case BinaryLogicalExpr::BinaryLogicalExprKind::Or:
        interResult = builder.CreateOr(left, right, "or");
        break;
    default:
        llvm_unreachable("Unexpected binary logical expression kind.");
    }
}

void CodeGen::visit(BoolLiteralExpr* expr) {
    auto* lit = llvm::ConstantInt::get(expr->getType()->getLLVMType(ctx),
                                       expr->getValue());
    interResult = lit;
}

void CodeGen::visit(CallExpr* expr) {
    llvm::Function* callee =
        llvm::dyn_cast<llvm::Function>(env->find(expr->getName()));

    std::vector<llvm::Value*> args;
    for (auto arg : expr->getArgs()) {
        evaluate(arg);
        args.push_back(interResult);
    }

    interResult = builder.CreateCall(callee, args, "calltmp");
}

void CodeGen::visit(GroupingExpr* expr) {
    evaluate(expr->getExpr());
}

void CodeGen::visit(IntLiteralExpr* expr) {
    auto* lit = llvm::ConstantInt::get(expr->getType()->getLLVMType(ctx),
                                       expr->getValue());
    interResult = lit;
}

void CodeGen::visit(PointerOpExpr* expr) {
    auto kind = expr->getPointerOpKind();
    if (kind == PointerOpExpr::PointerOpKind::AddressOf) {
        // "&" returns the pointer to the variable, but since allocas ARE
        // variable pointers, we just need to get the corresponding alloca.
        evaluate(expr->getExpr());
    } else {
        evaluate(expr->getExpr());
        auto* pointerTy =
            llvm::dyn_cast<PointerType>(expr->getExpr()->getType());
        assert(pointerTy && "Dereferencing a non-pointer type.");

        interResult = builder.CreateLoad(pointerTy->getLLVMType(ctx),
                                         interResult);
    }
}

void CodeGen::visit(UnaryExpr* expr) {
    evaluate(expr->getExpr());

    switch (expr->getUnaryKind()) {
    case UnaryExpr::UnaryExprKind::NegArith:
        interResult = builder.CreateNeg(interResult, "neg");
        break;
    case UnaryExpr::UnaryExprKind::NegLogic:
        interResult = builder.CreateNot(interResult, "not");
        break;
    default:
        llvm_unreachable("Unexpected unary expression kind.");
    }
}

void CodeGen::visit(VarExpr* expr) {
    auto* valAlloca = env->find(expr->getName());
    assert(valAlloca && "Undefined alloca");

    if (expr->getVarExprKind() == VarExpr::VarExprKind::Read)
        interResult = builder.CreateLoad(expr->getType()->getLLVMType(ctx),
                                         valAlloca,
                                         expr->getName());
    else
        interResult = valAlloca;
}

void CodeGen::visit(ExprStmt* stmt) {
    evaluate(stmt->getExpr());
}

void CodeGen::visit(IfStmt* stmt) {
    // Evalute the condition Value.
    evaluate(stmt->getCond());
    auto* cond = interResult;

    // Create the BBs. ElseBB is MergeBB if is no ELSE block.
    auto* thenBB = llvm::BasicBlock::Create(ctx, "then", currFun);
    auto* mergeBB = llvm::BasicBlock::Create(ctx, "merge");
    auto* elseBB = stmt->getElseBody().empty() ? mergeBB
                                               : llvm::BasicBlock::Create(ctx,
                                                                          "else");

    // Create a conditional branch.
    builder.CreateCondBr(cond, thenBB, elseBB);

    // Emit the THEN block.
    setCurrBB(thenBB);
    // Use RAII to manage the lifetime of scopes.
    {
        ValueScopeMgr scopeMgr(*this);
        for (auto thenStmt : stmt->getThenBody())
            evaluate(thenStmt);
    }
    builder.CreateBr(mergeBB);

    // Emit the ELSE block.
    if (!stmt->getElseBody().empty()) {
        currFun->getBasicBlockList().push_back(elseBB);
        setCurrBB(elseBB);
        // Use RAII to manage the lifetime of scopes.
        {
            ValueScopeMgr scopeMgr(*this);
            for (auto elseStmt : stmt->getElseBody())
                evaluate(elseStmt);
        }
        builder.CreateBr(mergeBB);
    }

    // Emit the merge block.
    currFun->getBasicBlockList().push_back(mergeBB);
    setCurrBB(mergeBB);
}

void CodeGen::visit(PrintStmt* stmt) {
    evaluate(stmt->getPrintExpr());

    builder.CreateCall(printFun, {printFormatStr, interResult}, "print");
}

void CodeGen::visit(ReturnStmt* stmt) {
    evaluate(stmt->getRetExpr());

    builder.CreateRet(interResult);
}

void CodeGen::visit(ScanStmt* stmt) {
    // Get the alloca for the scanned variable and pass it as a scanf parameter.
    auto* valAlloca = env->find(stmt->getScanVar()->getName());

    builder.CreateCall(scanFun, {scanFormatStr, valAlloca}, "scan");
}

void CodeGen::visit(UntilStmt* stmt) {
    // Create the BBs.
    auto* condBB = llvm::BasicBlock::Create(ctx, "cond", currFun);
    auto* bodyBB = llvm::BasicBlock::Create(ctx, "body");
    auto* mergeBB = llvm::BasicBlock::Create(ctx, "merge");

    // To make the current BB valid, we must insert an unconditional branch
    // to the condition BB.
    builder.CreateBr(condBB);

    // Emit the condition BB. We will always return to this BB after the body
    // finishes exection.
    setCurrBB(condBB);
    evaluate(stmt->getCond());
    auto* cond = interResult;
    builder.CreateCondBr(cond, bodyBB, mergeBB);

    // Emit the body block.
    currFun->getBasicBlockList().push_back(bodyBB);
    setCurrBB(bodyBB);
    // Use RAII to manage the lifetime of scopes.
    {
        ValueScopeMgr scopeMgr(*this);
        for (auto s : stmt->getBody())
            evaluate(s);
    }
    // Branch to the condition BB.
    builder.CreateBr(condBB);

    // Emit the merge block.
    currFun->getBasicBlockList().push_back(mergeBB);
    setCurrBB(mergeBB);
}

void CodeGen::visit(FunDecl* decl) {
    llvm::Function* fun =
        llvm::dyn_cast<llvm::Function>(env->find(decl->getName()));
    currFun = fun;

    // Create the entry BB.
    llvm::BasicBlock* entryBB = llvm::BasicBlock::Create(ctx, "entry", fun);
    setCurrBB(entryBB);

    ValueScopeMgr scopeMgr(*this);
    // Record the function arguments.
    auto declArg = decl->getArgs().begin();
    auto llvmArg = fun->args().begin();
    for (; declArg != decl->getArgs().end(); ++declArg, ++llvmArg) {
        // Create alloca for this argument and store it;
        llvm::IRBuilder<> tmpBuilder(&currFun->getEntryBlock(),
                                     currFun->getEntryBlock().begin());
        auto* alloca = tmpBuilder.CreateAlloca(llvmArg->getType(),
                                               0, (*declArg)->getName());
        tmpBuilder.CreateStore(llvmArg, alloca);
        env->insert(alloca, (*declArg)->getName());
    }

    for (auto funDecl : decl->getBody())
        evaluate(funDecl);
}

void CodeGen::visit(ModuleDecl* decl) {
    ValueScopeMgr scopeMgr(*this);
    // Create a built-in PRINT/SCAN functions.
    createPrintScanFunctions();

    for (auto dec : decl->getBody()) {
        // Forward declare all module functions.
        if (auto* funDecl = llvm::dyn_cast<FunDecl>(dec)) {
            auto* funTy = createFunctionType(funDecl);
            auto* fun = createFunction(funDecl, funTy);
            env->insert(fun, funDecl->getName());
        // Emit all global variables.
        } else if (llvm::isa<VarDecl>(dec))
            evaluate(dec);
        else
            llvm_unreachable("Unknown declaration inside a module.");
    }

    // Now generate code for all module functions.
    for (auto dec : decl->getBody()) {
        if (llvm::isa<FunDecl>(dec))
            evaluate(dec);
    }
}

void CodeGen::visit(VarDecl* decl) {
    if (decl->isGlobal()) {
        // Create a global variable, set the linkage to private...
        module->getOrInsertGlobal(decl->getName(),
                                  decl->getType()->getLLVMType(ctx));
        auto* globalVar = module->getNamedGlobal(decl->getName());
        globalVar->setLinkage(llvm::GlobalValue::PrivateLinkage);
        // ... and register it in the scope manager.
        env->insert(globalVar, decl->getName());

        // If the initializer exists, it will be either an INT or a BOOL literal.
        // It's safe to to dyn_cast here because the check is done in SemaCheck.
        if (decl->getInitializer()) {
            evaluate(decl->getInitializer());
            globalVar->setInitializer(llvm::dyn_cast<llvm::Constant>(interResult));
        }
    } else {
        // Create an alloca for this variable in the entry BB of the current
        // function...
        llvm::IRBuilder<> tmpBuilder(&currFun->getEntryBlock(),
                                     currFun->getEntryBlock().begin());
        auto* alloca = tmpBuilder.CreateAlloca(decl->getType()->getLLVMType(ctx),
                                               0, decl->getName());
        // ... and register it in the scope menager.
        env->insert(alloca, decl->getName());

        // Generate the code for the variable initializer (if it exists),
        // and store the result in the alloca.
        if (decl->getInitializer()) {
            evaluate(decl->getInitializer());
            builder.CreateStore(interResult, alloca);
        }
    }
}
