#include "CodeGen.h"

using namespace mxrlang;

void CodeGen::createPrintFunction() {
    llvm::ArrayRef<llvm::Type*> argTys = {llvm::Type::getInt8PtrTy(ctx)};
    auto* funTy = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx),
                                          argTys, true);

    printFun = llvm::Function::Create(funTy,
                                      llvm::GlobalValue::ExternalLinkage,
                                      "printf", module.get());

    formatStr = builder.CreateGlobalStringPtr(llvm::StringRef("%lld\n"),
                                              "formatstr", 0, module.get());
}

llvm::Type* CodeGen::convertTypeToLLVMType(Type* type) {
    if (type == Type::getIntType())
        return llvm::Type::getInt64Ty(ctx);
    else if (type == Type::getBoolType())
        return llvm::Type::getInt1Ty(ctx);

    llvm_unreachable("Unknown type.");
}

llvm::FunctionType* CodeGen::createFunctionType(FunDecl* decl) {
    auto* retTy = convertTypeToLLVMType(decl->getRetType());

    // Convert argument types to LLVM types.
    std::vector<llvm::Type*> args;
    for (auto arg : decl->getArgs())
        args.push_back(convertTypeToLLVMType(arg->getType()));

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

    llvm::Value* dest = nullptr;
    if (llvm::isa<VarExpr>(expr->getDest())) {
        auto* destVar = llvm::dyn_cast<VarExpr>(expr->getDest());
        dest = env->find(destVar->getName());
        assert(dest && "Undefined alloca");
    } else
        llvm_unreachable("Invalid assign destination.");

    builder.CreateStore(source, dest);

    // Create a load which is the "result" of the assigne expression,
    // and can be used further in code. If not used, it will be cleaned
    // up by DCE.
    interResult = builder.CreateLoad(convertTypeToLLVMType(expr->getType()),
                                     dest);
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
    auto* lit = llvm::ConstantInt::get(
                convertTypeToLLVMType(expr->getType()), expr->getValue());
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
    auto* lit = llvm::ConstantInt::get(
                convertTypeToLLVMType(expr->getType()), expr->getValue());
    interResult = lit;
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

    interResult = builder.CreateLoad(convertTypeToLLVMType(expr->getType()),
                                     valAlloca, expr->getName());
}

void CodeGen::visit(ExprStmt* stmt) {
    evaluate(stmt->getExpr());
}

void CodeGen::visit(IfStmt* stmt) {
    // Evalute the condition Value.
    evaluate(stmt->getCond());
    auto* cond = interResult;

    // Create the BBs. ElseBB is MergeBB if is no ELSE block.
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(ctx, "then",
                                                        currFun);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx, "merge");
    llvm::BasicBlock* elseBB = stmt->getElseBody().empty() ?
        mergeBB : llvm::BasicBlock::Create(ctx, "else");

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
    auto* printExpr = stmt->getPrintExpr();
    evaluate(printExpr);

    builder.CreateCall(printFun, {formatStr, interResult}, "print");
}

void CodeGen::visit(ReturnStmt* stmt) {
    evaluate(stmt->getRetExpr());

    builder.CreateRet(interResult);
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
    // Create a built-in PRINT function.
    createPrintFunction();

    // Forward declare all module functions.
    for (auto funDecl : decl->getBody()) {
        assert(llvm::isa<FunDecl>(funDecl));
        FunDecl* funDeclCast = llvm::dyn_cast<FunDecl>(funDecl);
        auto* funTy = createFunctionType(funDeclCast);
        auto* fun = createFunction(funDeclCast, funTy);

        env->insert(fun, funDeclCast->getName());
    }

    // Now generate code for all module functions.
    for (auto funDecl : decl->getBody())
        evaluate(funDecl);
}

void CodeGen::visit(VarDecl* decl) {
    // Create an alloca for this variable in the entry BB of the current
    // function...
    llvm::IRBuilder<> tmpBuilder(&currFun->getEntryBlock(),
                                 currFun->getEntryBlock().begin());
    auto* alloca = tmpBuilder.CreateAlloca(
        convertTypeToLLVMType(decl->getType()), 0, decl->getName());
    // ... and register it in the scope menager.
    env->insert(alloca, decl->getName());

    // Generate the code for the variable initializer (if it exists),
    // and store the result in the alloca.
    if (decl->getInitializer()) {
        evaluate(decl->getInitializer());
        builder.CreateStore(interResult, alloca);
    }
}
