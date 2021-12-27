#ifndef CODEGEN_H
#define CODEGEN_H

#include "llvm/IR/Instructions.h"
#include "llvm/Target/TargetMachine.h"
#include <memory>

#include "Diag.h"
#include "ScopeMgr.h"

namespace mxrlang {

class CodeGen : public ExprVisitor,
                public StmtVisitor {
    friend class ScopeMgr<CodeGen, llvm::AllocaInst*>;
    using ValueScopeMgr = ScopeMgr<CodeGen, llvm::AllocaInst*>;

    Environment<llvm::AllocaInst*> env = nullptr;

    llvm::LLVMContext ctx;
    llvm::TargetMachine* TM;
    std::unique_ptr<llvm::Module> module;
    std::string fileName;

    Diag& diag;

    // Intermediate result of the code gen.
    llvm::Value* interResult;

    // Expression visitor methods
    void visit(LiteralExpr* expr) override;

    // Statement visitor methods
    void visit(FunStmt* stmt) override;
    void visit(ModuleStmt* stmt) override;
    void visit(VarStmt* stmt) override;

    // Helper function for evaluating an expression or a statement.
    template <typename T>
    void evaluate(const T expr) {
        expr->accept(this);
    }

public:
    CodeGen(llvm::TargetMachine* TM, std::string fileName, Diag& diag)
        : TM(TM), fileName(fileName), diag(diag) {
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
