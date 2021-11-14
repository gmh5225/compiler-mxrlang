#ifndef TREE_H
#define TREE_H

#include "llvm/ADT/APSInt.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SMLoc.h"

#include <vector>

// This file contains definitions of abstract syntax tree expression
// and statement classes.

namespace mxrlang {

class Stmt;

using Stmts = std::vector<Stmt*>;

// The following class describes a declaration.

class Decl {
    llvm::StringRef name;

public:
    Decl(llvm::StringRef name) : name(name) {}

    llvm::StringRef getName() const { return name; }
};

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
    llvm::SMLoc loc;

public:
    Expr(ExprKind kind, llvm::SMLoc loc) : kind(kind), loc(loc) {}
    virtual ~Expr() = default;

    // Pure virtual accept method of the visitor pattern.
    virtual void accept(ExprVisitor* visitor) = 0;

    // Check whether this is a valid target of an assignment.
    // Can be overridden by valid targets (eg. Variable_expr) to return an
    // Assign_expr.
    virtual Expr* makeAssignExpr(Expr* left, Expr* right) { return nullptr; }

    ExprKind getKind() const { return kind; }
    llvm::SMLoc getLoc() const { return loc; }
};

class LiteralExpr : public Expr {
    llvm::APSInt value;

public:
    LiteralExpr(llvm::StringRef valueString, llvm::SMLoc loc)
        : Expr(ExprKind::Literal, loc) {
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
    llvm::SMLoc loc;

public:
    Stmt(StmtKind kind, llvm::SMLoc loc) : kind(kind), loc(loc) {}
    virtual ~Stmt() = default;

    // Pure virtual accept method of the visitor pattern.
    virtual void accept(StmtVisitor* visitor) = 0;

    StmtKind getKind() const { return kind; }
    llvm::SMLoc getLoc() const { return loc; }
};

// Statement node describing a function definition.
// Currently it only holds the top level "main" function.
class FunStmt : public Stmt,
                public Decl {
    llvm::StringRef name;
    Stmts body;

public:
    FunStmt(llvm::StringRef name, Stmts&& body, llvm::SMLoc loc)
        : Stmt(StmtKind::Fun, loc), Decl(name),
          name(name), body(std::move(body)) {}

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
class ModuleStmt : public Stmt,
                   public Decl {
    llvm::StringRef name;
    Stmts body;

public:
    ModuleStmt(llvm::StringRef name, Stmts&& body, llvm::SMLoc loc)
        : Stmt(StmtKind::Module, loc), Decl(name),
          name(name), body(std::move(body)) {}

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
class VarStmt : public Stmt,
                public Decl {
    llvm::StringRef name;
    Expr* initializer;

public:
    VarStmt(llvm::StringRef name, Expr* initializer, llvm::SMLoc loc)
        : Stmt(StmtKind::Var, loc), Decl(name),
          name(name), initializer(initializer) {}

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
