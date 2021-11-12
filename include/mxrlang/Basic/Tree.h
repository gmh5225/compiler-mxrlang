#ifndef TREE_H
#define TREE_H

#include "llvm/ADT/APSInt.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include <vector>

// This file contains definitions of abstract syntax tree expression
// and statement classes.

namespace mxrlang {

class Stmt;

using Stmts = std::vector<Stmt*>;

// The following classes describe expression nodes of the AST.

class LiteralExpr;

class ExprVisitor {
public:
    virtual void visit(LiteralExpr* expr) = 0;
};

class Expr {
public:
    enum class ExprKind {
        Literal
    };

private:
    ExprKind kind;

public:
    Expr(ExprKind kind) : kind(kind) {}
    virtual ~Expr() = default;

    // Pure virtual accept method of the visitor pattern.
    virtual void accept(ExprVisitor* visitor) = 0;

    // Check whether this is a valid target of an assignment.
    // Can be overridden by valid targets (eg. Variable_expr) to return an
    // Assign_expr.
    virtual Expr* makeAssignExpr(Expr* left, Expr* right) { return nullptr; }

    ExprKind getKind() const { return kind; }
};

class LiteralExpr : public Expr {
    llvm::APSInt value;

public:
    LiteralExpr(llvm::StringRef valueString)
        : Expr(ExprKind::Literal) {
        value = llvm::APInt(/* numBits= */ 64, valueString, /* radix= */ 10);
        value.setIsSigned(true);
    }

    llvm::APSInt& getValue() { return value; }

    virtual void accept(ExprVisitor* visitor) override {
        visitor->visit(this);
    }

    static bool classof(const Expr *E) {
      return E->getKind() == ExprKind::Literal;
    }
};

// The following classes describe statement nodes of the AST.

class FunStmt;
class ModuleStmt;
class VarStmt;

class StmtVisitor {
public:
    virtual void visit(FunStmt* stmt) = 0;
    virtual void visit(ModuleStmt* stmt) = 0;
    virtual void visit(VarStmt* stmt) = 0;
};

class Stmt {
public:
    enum class StmtKind {
        Fun,
        Module,
        Var
    };

private:
    StmtKind kind;

public:
    Stmt(StmtKind kind) : kind(kind) {}
    virtual ~Stmt() = default;

    // Pure virtual accept method of the visitor pattern.
    virtual void accept(StmtVisitor* visitor) = 0;

    StmtKind getKind() const { return kind; }
};

// Statement node describing a function definition.
// Currently it only holds the top level "main" function.
class FunStmt : public Stmt {
    llvm::StringRef name;
    Stmts body;

public:
    FunStmt(llvm::StringRef name, Stmts&& body)
        : Stmt(StmtKind::Fun), name(name), body(std::move(body)) {}

    llvm::StringRef getName() { return name; }
    Stmts& getBody() { return body; }

    virtual void accept(StmtVisitor* visitor) override {
        visitor->visit(this);
    }

    static bool classof(const Stmt *S) {
      return S->getKind() == StmtKind::Fun;
    }
};

// Statement node describing a module.
// Currently only one module supported per program.
class ModuleStmt : public Stmt {
    llvm::StringRef name;
    Stmts body;

public:
    ModuleStmt(llvm::StringRef name, Stmts&& body)
        : Stmt(StmtKind::Module), name(name), body(std::move(body)) {}

    llvm::StringRef getName() { return name; }
    Stmts& getBody() { return body; }

    virtual void accept(StmtVisitor* visitor) override {
        visitor->visit(this);
    }

    static bool classof(const Stmt *S) {
      return S->getKind() == StmtKind::Module;
    }
};

// Statement node decribing a variable declaration/definition.
class VarStmt : public Stmt {
    llvm::StringRef name;
    Expr* initializer;

public:
    VarStmt(llvm::StringRef name, Expr* initializer)
        : Stmt(StmtKind::Var), name(name), initializer(initializer) {}

    llvm::StringRef getName() { return name; }
    Expr* getInitializer() { return initializer; }

    virtual void accept(StmtVisitor* visitor) override {
        visitor->visit(this);
    }

    static bool classof(const Stmt *S) {
      return S->getKind() == StmtKind::Var;
    }
};

} // namespace mxrlang

#endif // TREE_H
