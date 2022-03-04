#ifndef CODEGEN_H
#define CODEGEN_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Target/TargetMachine.h"
#include <memory>

#include "Diag.h"
#include "ScopeMgr.h"
#include "Type.h"

namespace mxrlang {

class CodeGen : public Visitor {
    friend class ScopeMgr<CodeGen, llvm::Value>;
    using ValueScopeMgr = ScopeMgr<CodeGen, llvm::Value>;

    // Environment holding various Value pointers (allocas, functions, etc).
    Environment<llvm::Value>* env = nullptr;

    // Built-in print/scan functions declarations and format string.
    llvm::Function* printFun;
    llvm::Function* scanFun;
    llvm::Constant* printFormatStr;
    llvm::Constant* scanFormatStr;

    // LLVM internals.
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
    void visit(AssignExpr* expr) override;
    void visit(BinaryArithExpr* expr) override;
    void visit(BinaryLogicalExpr* expr) override;
    void visit(BoolLiteralExpr* expr) override;
    void visit(CallExpr* expr) override;
    void visit(GroupingExpr* expr) override;
    void visit(IntLiteralExpr* expr) override;
    void visit(UnaryExpr* expr) override;
    void visit(VarExpr* expr) override;

    // Statement visitor methods
    void visit(ExprStmt* stmt) override;
    void visit(IfStmt* stmt) override;
    void visit(PrintStmt* stmt) override;
    void visit(ReturnStmt* stmt) override;
    void visit(ScanStmt* stmt) override;
    void visit(UntilStmt* stmt) override;

    void visit(FunDecl* decl) override;
    void visit(ModuleDecl* decl) override;
    void visit(VarDecl* decl) override;

    // Helper function for evaluating an expression or a statement.
    template <typename T>
    void evaluate(const T expr) {
        expr->accept(this);
    }

    // Set the current BB and builder.
    void setCurrBB(llvm::BasicBlock* BB) {
        currBB = BB;
        builder.SetInsertPoint(BB);
    }

    // Convert mxrlang type to LLVM type.
    llvm::Type* convertTypeToLLVMType(Type* type);

    llvm::FunctionType* createFunctionType(FunDecl* decl);
    llvm::Function* createFunction(FunDecl* decl, llvm::FunctionType* type);

    // Declare the built-in print function.
    void createPrintScanFunctions();

public:
    CodeGen(llvm::TargetMachine* TM, std::string fileName, Diag& diag)
        : TM(TM), builder(ctx), fileName(fileName), diag(diag) {
        module = std::make_unique<llvm::Module>(fileName, ctx);
        module->setTargetTriple(TM->getTargetTriple().getTriple());
        module->setDataLayout(TM->createDataLayout());
    }

    llvm::Module* getModule() { return module.get(); }

    // Runner.
    void run(ModuleDecl* moduleDecl) {
        evaluate(moduleDecl);
    }
};

}

#endif // CODEGEN_H
