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

llvm::FunctionType *CodeGen::createFunctionType(FunStmt* stmt) {
    // FIXME: Currently only possible to return an Int64 value.
    auto* resultTy = convertTypeToLLVMType(stmt->getType());
    return llvm::FunctionType::get(resultTy, /*isVarArg*/ false);
}

llvm::Function *CodeGen::createFunction(FunStmt* stmt,
                                        llvm::FunctionType* type) {
    return llvm::Function::Create(type, llvm::GlobalValue::ExternalLinkage,
                                  stmt->getName(), module.get());
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
        llvm_unreachable("Invalid binary arithmetic operator.");
    }
}

void CodeGen::visit(BoolLiteralExpr* expr) {
    auto* lit = llvm::ConstantInt::get(
                convertTypeToLLVMType(expr->getType()), expr->getValue());
    interResult = lit;
}

void CodeGen::visit(GroupingExpr* expr) {
    evaluate(expr->getExpr());
}

void CodeGen::visit(IntLiteralExpr* expr) {
    auto* lit = llvm::ConstantInt::get(
                convertTypeToLLVMType(expr->getType()), expr->getValue());
    interResult = lit;
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

void CodeGen::visit(FunStmt* stmt) {
    auto* funTy = createFunctionType(stmt);
    auto* fun = createFunction(stmt, funTy);
    currFun = fun;

    // Currently has only one BB.
    llvm::BasicBlock* entryBB = llvm::BasicBlock::Create(ctx, "entry", fun);
    setCurrBB(entryBB);

    AllocaScopeMgr scopeMgr(*this);
    for (auto funStmt : stmt->getBody())
        evaluate(funStmt);
}

void CodeGen::visit(IfStmt* stmt) {
    // Evalute the condition Value.
    evaluate(stmt->getCond());
    auto* cond = interResult;

    // Create the BBs. ElseBB is MergeBB if is no ELSE block.
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(ctx, "then",
                                                        currFun);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx, "merge");
    llvm::BasicBlock* elseBB = stmt->getElseStmts().empty() ?
        mergeBB : llvm::BasicBlock::Create(ctx, "else");

    // Create a conditional branch.
    builder.CreateCondBr(cond, thenBB, elseBB);

    // Emit the THEN block.
    setCurrBB(thenBB);
    // Use RAII to manage the lifetime of scopes.
    {
        AllocaScopeMgr scopeMgr(*this);
        for (auto thenStmt : stmt->getThenStmts()) {
            evaluate(thenStmt);
        }
    }
    builder.CreateBr(mergeBB);

    // Emit the ELSE block.
    if (!stmt->getElseStmts().empty()) {
        currFun->getBasicBlockList().push_back(elseBB);
        setCurrBB(elseBB);
        // Use RAII to manage the lifetime of scopes.
        {
            AllocaScopeMgr scopeMgr(*this);
            for (auto elseStmt : stmt->getElseStmts()) {
                evaluate(elseStmt);
            }
        }
        builder.CreateBr(mergeBB);
    }

    // Emit the merge block.
    currFun->getBasicBlockList().push_back(mergeBB);
    setCurrBB(mergeBB);
}

void CodeGen::visit(ModuleStmt* stmt) {
    AllocaScopeMgr scopeMgr(*this);
    createPrintFunction();

    // FIXME: Currently only "main" function exists, which is implicitly
    // declared.
    assert(stmt->getBody().size() == 1);
    evaluate(stmt->getBody().at(0));
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

void CodeGen::visit(VarStmt* stmt) {
    // Create an alloca for this variable in the entry BB of the current
    // function...
    llvm::IRBuilder<> tmpBuilder(&currFun->getEntryBlock(),
                                 currFun->getEntryBlock().begin());
    auto* alloca = tmpBuilder.CreateAlloca(
        convertTypeToLLVMType(stmt->getType()), 0, stmt->getName());
    // ... and register it in the scope menager.
    env->insert(alloca, stmt->getName());

    // Generate the code for the variable initializer (if it exists),
    // and store the result in the alloca.
    if (stmt->getInitializer()) {
        evaluate(stmt->getInitializer());
        builder.CreateStore(interResult, alloca);
    }
}
