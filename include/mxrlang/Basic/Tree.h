#ifndef TREE_H
#define TREE_H

#include "Type.h"

#include "llvm/ADT/APSInt.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SMLoc.h"

#include <vector>

// This file contains definitions of abstract syntax tree expression
// and statement classes.

// Generates boilerplate code for each class.
#define ACCEPT()                                                \
    virtual void accept(Visitor* visitor) override {            \
        visitor->visit(this);                                   \
    }                                                           \

#define CLASSOF(PARENT, KIND)                                   \
    static bool classof(const PARENT* node) {                   \
        return node->getKind() == PARENT##Kind::KIND;           \
    }                                                           \

namespace mxrlang {

// Forward declare all classes.
class Expr;
class AssignExpr;
class BinaryArithExpr;
class BinaryLogicalExpr;
class BoolLiteralExpr;
class CallExpr;
class GroupingExpr;
class IntLiteralExpr;
class UnaryExpr;
class VarExpr;

class Stmt;
class ExprStmt;
class FunStmt;
class IfStmt;
class ModuleStmt;
class PrintStmt;
class ReturnStmt;
class VarStmt;

// Inherit from Visitor class in order to create an AST traversal class.
class Visitor {
public:
    virtual void visit(AssignExpr* expr) = 0;
    virtual void visit(BinaryArithExpr* expr) = 0;
    virtual void visit(BinaryLogicalExpr* expr) = 0;
    virtual void visit(BoolLiteralExpr* expr) = 0;
    virtual void visit(CallExpr* expr) = 0;
    virtual void visit(GroupingExpr* expr) = 0;
    virtual void visit(IntLiteralExpr* expr) = 0;
    virtual void visit(UnaryExpr* expr) = 0;
    virtual void visit(VarExpr* expr) = 0;

    virtual void visit(ExprStmt* stmt) = 0;
    virtual void visit(FunStmt* stmt) = 0;
    virtual void visit(IfStmt* stmt) = 0;
    virtual void visit(ModuleStmt* stmt) = 0;
    virtual void visit(PrintStmt* stmt) = 0;
    virtual void visit(ReturnStmt* stmt) = 0;
    virtual void visit(VarStmt* stmt) = 0;
};

using Stmts = std::vector<Stmt*>;
using FunCallArgs = std::vector<Expr*>;
using FunDeclArgs = std::vector<VarStmt*>;

// The following class describes a declaration.

class Decl {
public:
    enum class DeclKind {
        Fun,
        Module,
        Var
    };

private:
    llvm::StringRef name;
    DeclKind kind;
    Type* type;

public:
    Decl(llvm::StringRef name, DeclKind kind,
         Type* type = Type::getIntType())
        : name(name), kind(kind), type(type) {}

    llvm::StringRef getName() const { return name; }
    DeclKind getKind() const { return kind; }
    Type* getType() { return type; }
};

// The following classes describe expression nodes of the AST.

class Expr {
public:
    enum class ExprKind {
        Assign,
        BinaryArith,
        BinaryLogical,
        BoolLiteral,
        Call,
        Grouping,
        IntLiteral,
        Unary,
        Var
    };

private:
    ExprKind kind;
    llvm::SMLoc loc;
    // Every expression should have a type.
    Type* type;

public:
    Expr(ExprKind kind, llvm::SMLoc loc, Type* type = Type::getNoneType())
        : kind(kind), loc(loc), type(type) {}
    virtual ~Expr() = default;

    // Pure virtual accept method of the visitor pattern.
    virtual void accept(Visitor* visitor) = 0;

    // Check whether this is a valid target of an assignment.
    // Can be overridden by valid targets (eg. Variable_expr) to return an
    // Assign_expr.
    virtual Expr* makeAssignExpr(Expr* source) { return nullptr; }

    ExprKind getKind() const { return kind; }
    llvm::SMLoc getLoc() const { return loc; }
    Type* getType() const { return type; }

    void setType(Type* type) { this->type = type; }
};

class AssignExpr : public Expr {
    Expr* dest;
    Expr* source;

public:
    AssignExpr(Expr* dest, Expr* source, llvm::SMLoc loc)
        : Expr(ExprKind::Assign, loc), dest(dest), source(source) {}

    Expr* getDest() { return dest; }
    Expr* getSource() { return source; }

    ACCEPT()
    CLASSOF(Expr, Assign)
};

class BinaryArithExpr : public Expr {
public:
    enum class BinaryArithExprKind {
        Add,
        Div,
        Mul,
        Sub
    };

private:
    BinaryArithExprKind binKind;
    Expr* left;
    Expr* right;
    std::string opString;

public:
    BinaryArithExpr(BinaryArithExprKind binKind, Expr* left, Expr* right,
                    std::string opString, llvm::SMLoc loc)
        : Expr(ExprKind::BinaryArith, loc), binKind(binKind), left(left),
          right(right), opString(opString) {}

    BinaryArithExprKind getBinaryKind() { return binKind; }
    Expr* getLeft() { return left; }
    Expr* getRight() { return right; }
    std::string& getOpString() { return opString; }

    ACCEPT()
    CLASSOF(Expr, BinaryArith)
};

class BinaryLogicalExpr : public Expr {
public:
    enum class BinaryLogicalExprKind {
        And,
        Eq,
        Greater,
        GreaterEq,
        Less,
        LessEq,
        NotEq,
        Or
    };

private:
    BinaryLogicalExprKind binKind;
    Expr* left;
    Expr* right;
    std::string opString;

public:
    BinaryLogicalExpr(BinaryLogicalExprKind binKind, Expr* left,
                      Expr* right, std::string opString, llvm::SMLoc loc)
        : Expr(ExprKind::BinaryLogical, loc), binKind(binKind), left(left),
          right(right), opString(opString) {}

    BinaryLogicalExprKind getBinaryKind() { return binKind; }
    Expr* getLeft() { return left; }
    Expr* getRight() { return right; }
    std::string& getOpString() { return opString; }

    ACCEPT()
    CLASSOF(Expr, BinaryLogical)
};

class BoolLiteralExpr : public Expr {
    bool value;

public:
    BoolLiteralExpr(bool value, llvm::SMLoc loc)
        : Expr(ExprKind::BoolLiteral, loc, Type::getBoolType()),
          value(value) {}

    bool getValue() const { return value; }

    ACCEPT()
    CLASSOF(Expr, BoolLiteral)
};

class CallExpr : public Expr {
    llvm::StringRef funName;
    FunCallArgs args;

public:
    CallExpr(llvm::StringRef funName, FunCallArgs&& args, llvm::SMLoc loc)
        : Expr(ExprKind::Call, loc), funName(funName),
          args(std::move(args)) {}

    llvm::StringRef& getName() { return funName; }
    FunCallArgs& getArgs() { return args; }

    ACCEPT()
    CLASSOF(Expr, Call)
};

class GroupingExpr : public Expr {
    Expr* expr;

public:
    GroupingExpr(Expr* expr, llvm::SMLoc loc)
        : Expr(ExprKind::Grouping, loc), expr(expr) {}

    Expr* getExpr() { return expr; }

    ACCEPT()
    CLASSOF(Expr, Grouping)
};

class IntLiteralExpr : public Expr {
    llvm::APSInt value;

public:
    IntLiteralExpr(llvm::StringRef valueString, llvm::SMLoc loc)
        : Expr(ExprKind::IntLiteral, loc, Type::getIntType()) {
        value = llvm::APInt(/* numBits= */ 64, valueString, /* radix= */ 10);
        value.setIsSigned(true);
    }

    llvm::APSInt& getValue() { return value; }

    ACCEPT()
    CLASSOF(Expr, IntLiteral)
};

class UnaryExpr : public Expr {
public:
    enum class UnaryExprKind {
        NegArith,
        NegLogic
    };

private:
    UnaryExprKind unaryKind;
    Expr* expr;
    std::string opString;

public:
    UnaryExpr(UnaryExprKind unaryKind, Expr* expr, std::string opString,
              llvm::SMLoc loc)
        : Expr(ExprKind::Unary, loc), unaryKind(unaryKind), expr(expr),
          opString(opString) {}

    UnaryExprKind getUnaryKind() { return unaryKind; }
    Expr* getExpr() { return expr; }
    std::string& getOpString() { return opString; }

    ACCEPT()
    CLASSOF(Expr, Unary)
};

class VarExpr : public Expr {
    llvm::StringRef name;

public:
    VarExpr(llvm::StringRef name, llvm::SMLoc loc)
        : Expr(ExprKind::Var, loc), name(name) {}

    llvm::StringRef getName() { return name; }

    ACCEPT()
    CLASSOF(Expr, Var)

    // VarExpr is a valid assignment destination.
    Expr* makeAssignExpr(Expr* source) override {
        return new AssignExpr(this, source, this->getLoc());
    }
};

// The following classes describe statement nodes of the AST.

class Stmt {
public:
    enum class StmtKind {
        Expr,
        Fun,
        If,
        Module,
        Print,
        Return,
        Var
    };

private:
    StmtKind kind;
    llvm::SMLoc loc;

public:
    Stmt(StmtKind kind, llvm::SMLoc loc) : kind(kind), loc(loc) {}
    virtual ~Stmt() = default;

    // Pure virtual accept method of the visitor pattern.
    virtual void accept(Visitor* visitor) = 0;

    StmtKind getKind() const { return kind; }
    llvm::SMLoc getLoc() const { return loc; }
};

// Statement node describing an expression statement.
class ExprStmt : public Stmt {
    Expr* expr;

public:
    ExprStmt(Expr* expr, llvm::SMLoc loc)
        : Stmt(StmtKind::Expr, loc), expr(expr) {}

    Expr* getExpr() { return expr; }

    ACCEPT()
    CLASSOF(Stmt, Expr)
};

// Statement node describing a function definition.
// Currently it only holds the top level "main" function.
class FunStmt : public Stmt,
                public Decl {
    llvm::StringRef name;
    FunDeclArgs args;
    Stmts body;

public:
    FunStmt(llvm::StringRef name, Type* retType, FunDeclArgs&& args,
            Stmts&& body, llvm::SMLoc loc)
        : Stmt(StmtKind::Fun, loc), Decl(name, DeclKind::Fun, retType),
          name(name), args(std::move(args)), body(std::move(body)) {}

    llvm::StringRef getName() { return name; }
    FunDeclArgs& getArgs() { return args; }
    Stmts& getBody() { return body; }

    ACCEPT()
    CLASSOF(Stmt, Fun)
    CLASSOF(Decl, Fun)
};


// Statement node describing an IF statement.
class IfStmt : public Stmt {
    Expr* cond;
    Stmts thenStmts;
    Stmts elseStmts;

public:
    IfStmt(Expr* cond, Stmts&& thenStmts,
           Stmts&& elseStmts, llvm::SMLoc loc)
        : Stmt(StmtKind::If, loc), cond(cond),
          thenStmts(std::move(thenStmts)),
          elseStmts(std::move(elseStmts)) {}

    Expr* getCond() { return cond; }
    Stmts& getThenStmts() { return thenStmts; }
    Stmts& getElseStmts() { return elseStmts; }

    ACCEPT()
    CLASSOF(Stmt, If)
};


// Statement node describing a module.
// Currently only one module supported per program.
class ModuleStmt : public Stmt,
                   public Decl {
    llvm::StringRef name;
    Stmts body;

public:
    ModuleStmt(llvm::StringRef name, Stmts&& body, llvm::SMLoc loc)
        : Stmt(StmtKind::Module, loc), Decl(name, DeclKind::Module),
          name(name), body(std::move(body)) {}

    llvm::StringRef getName() { return name; }
    Stmts& getBody() { return body; }

    ACCEPT()
    CLASSOF(Stmt, Module)
    CLASSOF(Decl, Module)
};

// Statement node descibing a built-in PRINT function call.
class PrintStmt : public Stmt {
    Expr* printExpr;

public:
    PrintStmt(Expr* printExpr, llvm::SMLoc loc)
        : Stmt(StmtKind::Print, loc) , printExpr(printExpr) {}

    Expr* getPrintExpr() { return printExpr; }

    ACCEPT()
    CLASSOF(Stmt, Print)
};

// Statement node describing a return statement.
class ReturnStmt : public Stmt {
    Expr* retExpr;

public:
    ReturnStmt(Expr* retExpr, llvm::SMLoc loc)
        : Stmt(StmtKind::Return, loc), retExpr(retExpr) {}

    Expr* getRetExpr() { return retExpr; }

    ACCEPT()
    CLASSOF(Stmt, Return)
};

// Statement node decribing a variable declaration/definition.
class VarStmt : public Stmt,
                public Decl {
    llvm::StringRef name;
    Expr* initializer;

public:
    VarStmt(llvm::StringRef name, Expr* initializer, Type* type,
            llvm::SMLoc loc)
        : Stmt(StmtKind::Var, loc), Decl(name, DeclKind::Var, type),
          name(name), initializer(initializer) {}

    llvm::StringRef getName() { return name; }
    Expr* getInitializer() { return initializer; }

    void setInitializer(Expr* init) { this->initializer = init; }

    ACCEPT()
    CLASSOF(Stmt, Var)
    CLASSOF(Decl, Var)
};

} // namespace mxrlang

#undef ACCEPT
#undef CLASSOF

#endif // TREE_H
