#ifndef CODEGEN_H
#define CODEGEN_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Target/TargetMachine.h"
#include <memory>

#include "Diag.h"
#include "ScopeMgr.h"

namespace mxrlang {

class CodeGen : public ExprVisitor,
                public StmtVisitor {
    friend class ScopeMgr<CodeGen, llvm::AllocaInst>;
    using AllocaScopeMgr = ScopeMgr<CodeGen, llvm::AllocaInst>;

    // Environment holding local variable allocas.
    Environment<llvm::AllocaInst>* env = nullptr;

    llvm::LLVMContext ctx;
    llvm::TargetMachine* TM;
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;

    // Function which we are currently generating.
    llvm::Function* currFun = nullptr;

    // BB in which we are currently inserting.
    llvm::BasicBlock* currBB = nullptr;

    // Name of the module.
    std::string fileName;

    // Diagnostics manager.
    Diag& diag;

    // Intermediate result of the code gen.
    llvm::Value* interResult;

    // Expression visitor methods
    void visit(LiteralExpr* expr) override;
    void visit(VarExpr* expr) override;

    // Statement visitor methods
    void visit(FunStmt* stmt) override;
    void visit(ModuleStmt* stmt) override;
    void visit(ReturnStmt* stmt) override;
    void visit(VarStmt* stmt) override;

    // Helper function for evaluating an expression or a statement.
    template <typename T>
    void evaluate(const T expr) {
        expr->accept(this);
    }

    llvm::FunctionType* createFunctionType(FunStmt* stmt);
    llvm::Function* createFunction(FunStmt* stmt, llvm::FunctionType* type);

public:
    CodeGen(llvm::TargetMachine* TM, std::string fileName, Diag& diag)
        : TM(TM), builder(ctx), fileName(fileName), diag(diag) {
        module = std::make_unique<llvm::Module>(fileName, ctx);
        module->setTargetTriple(TM->getTargetTriple().getTriple());
        module->setDataLayout(TM->createDataLayout());
    }

    llvm::Module* getModule() { return module.get(); }

    // Runner.
    void run(ModuleStmt* moduleDecl) {
        evaluate(moduleDecl);
    }
};

}

#endif // CODEGEN_H
