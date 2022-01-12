#include "CodeGen.h"

using namespace mxrlang;

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

void CodeGen::visit(BoolLiteralExpr* expr) {
    auto* lit = llvm::ConstantInt::get(
                convertTypeToLLVMType(expr->getType()), expr->getValue());
    interResult = lit;
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

void CodeGen::visit(FunStmt* stmt) {
    auto* funTy = createFunctionType(stmt);
    auto* fun = createFunction(stmt, funTy);
    currFun = fun;

    // Currently has only one BB.
    llvm::BasicBlock* entryBB = llvm::BasicBlock::Create(ctx, "entry", fun);
    builder.SetInsertPoint(entryBB);
    currBB = entryBB;

    AllocaScopeMgr scopeMgr(*this);
    for (auto funStmt : stmt->getBody())
        evaluate(funStmt);
}

void CodeGen::visit(ModuleStmt* stmt) {
    AllocaScopeMgr scopeMgr(*this);

    // FIXME: Currently only "main" function exists, which is implicitly
    // declared.
    assert(stmt->getBody().size() == 1);
    evaluate(stmt->getBody().at(0));
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
