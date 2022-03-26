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
class Node;

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
class IfStmt;
class PrintStmt;
class ReturnStmt;
class ScanStmt;
class UntilStmt;

class Decl;
class FunDecl;
class ModuleDecl;
class VarDecl;

using Nodes = std::vector<Node*>;
using Decls = std::vector<Decl*>;
using FunCallArgs = std::vector<Expr*>;
using FunDeclArgs = std::vector<VarDecl*>;

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
    virtual void visit(IfStmt* stmt) = 0;
    virtual void visit(PrintStmt* stmt) = 0;
    virtual void visit(ReturnStmt* stmt) = 0;
    virtual void visit(ScanStmt* stmt) = 0;
    virtual void visit(UntilStmt* stmt) = 0;

    virtual void visit(FunDecl* decl) = 0;
    virtual void visit(ModuleDecl* decl) = 0;
    virtual void visit(VarDecl* decl) = 0;
};

// Node class describes a single AST node.
class Node {
public:
    enum class NodeKind {
        Decl,
        Expr,
        Stmt
    };

private:
    NodeKind kind;
    // Ties the node to the location in the source code. Useful for error
    // reporting.
    llvm::SMLoc loc;

    // Parent node of this node.
    Node* parent;

public:
    Node(NodeKind kind, llvm::SMLoc loc, Node* parent = nullptr)
        : kind(kind), loc(loc), parent(parent) {}
    virtual ~Node() = default;

    // Pure virtual accept method of the visitor pattern.
    virtual void accept(Visitor* visitor) = 0;

    NodeKind getKind() const { return kind; }
    llvm::SMLoc getLoc() const { return loc; }
    Node* getParent() { return parent; }

    void setParent(Node* parent) { this->parent = parent; }
};

// Expr class describes expression nodes of the AST.
class Expr : public Node {
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
    // Every expression should have a type.
    Type* type;

public:
    Expr(ExprKind kind, llvm::SMLoc loc, Node* parent = nullptr,
         Type* type = Type::getNoneType())
        : Node(NodeKind::Expr, loc, parent), kind(kind), type(type) {}

    // Check whether this is a valid target of an assignment.
    // Can be overridden by valid targets (eg. Variable_expr) to return an
    // Assign_expr.
    virtual Expr* makeAssignExpr(Expr* source) { return nullptr; }

    ExprKind getKind() const { return kind; }
    Type* getType() const { return type; }

    void setType(Type* type) { this->type = type; }

    CLASSOF(Node, Expr)
};

// Stmt class describes statement nodes of the AST.
class Stmt : public Node {
public:
    enum class StmtKind {
        Expr,
        Fun,
        If,
        Module,
        Print,
        Return,
        Scan,
        Until
    };

private:
    StmtKind kind;

public:
    Stmt(StmtKind kind, llvm::SMLoc loc, Node* parent = nullptr)
        : Node(NodeKind::Stmt, loc, parent), kind(kind) {}

    StmtKind getKind() const { return kind; }

    CLASSOF(Node, Stmt)
};

// Decl class describes a declaration node of the AST.
class Decl : public Node {
public:
    enum class DeclKind {
        Fun,
        Module,
        Var
    };

private:
    DeclKind kind;
    // Every declaration should have a name
    llvm::StringRef name;

public:
    Decl(DeclKind kind, llvm::StringRef name, llvm::SMLoc loc,
         Node* parent = nullptr)
        : Node(NodeKind::Decl, loc, parent), kind(kind), name(name) {}

    DeclKind getKind() const { return kind; }
    llvm::StringRef getName() const { return name; }

    CLASSOF(Node, Decl)
};

// Describes an assignment (e.g. x := 5).
class AssignExpr : public Expr {
    Expr* dest;
    Expr* source;

public:
    AssignExpr(Expr* dest, Expr* source, llvm::SMLoc loc, Node* parent = nullptr)
        : Expr(ExprKind::Assign, loc, parent), dest(dest), source(source) {
        dest->setParent(this);
        source->setParent(this);
    }

    Expr* getDest() const { return dest; }
    Expr* getSource() const { return source; }

    void setDest(Expr* dest) { this->dest = dest; }
    void setSource(Expr* source) { this->source = source; }

    ACCEPT()
    CLASSOF(Expr, Assign)
};

// Describes a binary arithmetic expression (e.g. 5 * x).
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
    // Useful for printing out the AST.
    llvm::StringRef opString;

public:
    BinaryArithExpr(BinaryArithExprKind binKind, Expr* left, Expr* right,
                    llvm::StringRef opString, llvm::SMLoc loc,
                    Node* parent = nullptr)
        : Expr(ExprKind::BinaryArith, loc, parent), binKind(binKind), left(left),
          right(right), opString(opString) {
        left->setParent(this);
        right->setParent(this);
    }

    BinaryArithExprKind getBinaryKind() const { return binKind; }
    Expr* getLeft() const { return left; }
    Expr* getRight() const { return right; }
    const llvm::StringRef& getOpString() const { return opString; }

    void setLeft(Expr* left) { this->left = left; }
    void setRight(Expr* right) { this->right = right; }

    ACCEPT()
    CLASSOF(Expr, BinaryArith)
};

// Describes a binary logical expression (e.g. x < 5).
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
    // Useful for printing out the AST.
    llvm::StringRef opString;

public:
    BinaryLogicalExpr(BinaryLogicalExprKind binKind, Expr* left, Expr* right,
                      llvm::StringRef opString, llvm::SMLoc loc,
                      Node* parent = nullptr)
        : Expr(ExprKind::BinaryLogical, loc, parent), binKind(binKind), left(left),
          right(right), opString(opString) {
        left->setParent(this);
        right->setParent(this);
    }

    BinaryLogicalExprKind getBinaryKind() const { return binKind; }
    Expr* getLeft() const { return left; }
    Expr* getRight() const { return right; }
    const llvm::StringRef& getOpString() const { return opString; }

    void setLeft(Expr* left) { this->left = left; }
    void setRight(Expr* right) { this->right = right; }

    ACCEPT()
    CLASSOF(Expr, BinaryLogical)
};

// Descibes a BOOL type literal (e.g TRUE).
class BoolLiteralExpr : public Expr {
    bool value;

public:
    BoolLiteralExpr(bool value, llvm::SMLoc loc, Node* parent = nullptr)
        : Expr(ExprKind::BoolLiteral, loc, parent, Type::getBoolType()),
          value(value) {}

    bool getValue() const { return value; }

    ACCEPT()
    CLASSOF(Expr, BoolLiteral)
};

// Describes a function call (e.g. fun(5, 6)).
class CallExpr : public Expr {
    llvm::StringRef funName;
    FunCallArgs args;

public:
    CallExpr(llvm::StringRef funName, FunCallArgs&& args, llvm::SMLoc loc,
             Node* parent = nullptr)
        : Expr(ExprKind::Call, loc, parent), funName(funName),
          args(std::move(args)) {
        for (auto* arg : args)
            arg->setParent(this);
    }

    const llvm::StringRef& getName() const { return funName; }
    FunCallArgs& getArgs() { return args; }

    ACCEPT()
    CLASSOF(Expr, Call)
};

// Describes an expression inside a parentheses (e.g. (5 + 6 * x)).
class GroupingExpr : public Expr {
    Expr* expr;

public:
    GroupingExpr(Expr* expr, llvm::SMLoc loc, Node* parent = nullptr)
        : Expr(ExprKind::Grouping, loc, parent), expr(expr) {
        expr->setParent(this);
    }

    Expr* getExpr() const { return expr; }

    void setExpr(Expr* expr) { this->expr = expr; }

    ACCEPT()
    CLASSOF(Expr, Grouping)
};

// Describes an INT type literal (e.g. 1264).
class IntLiteralExpr : public Expr {
    llvm::APSInt value;

public:
    IntLiteralExpr(llvm::StringRef valueString, llvm::SMLoc loc,
                   Node* parent = nullptr)
        : Expr(ExprKind::IntLiteral, loc, parent, Type::getIntType()) {
        value = llvm::APInt(/* numBits= */ 64, valueString, /* radix= */ 10);
        value.setIsSigned(true);
    }

    IntLiteralExpr(llvm::APSInt value, llvm::SMLoc loc, Node* parent = nullptr)
        : Expr(ExprKind::IntLiteral, loc, parent, Type::getIntType()),
          value(value) {}

    const llvm::APSInt& getValue() const { return value; }

    ACCEPT()
    CLASSOF(Expr, IntLiteral)
};

// Describes an unary expression (e.g. !x).
class UnaryExpr : public Expr {
public:
    enum class UnaryExprKind {
        NegArith,
        NegLogic
    };

private:
    UnaryExprKind unaryKind;
    Expr* expr;
    // Useful for printing out the AST.
    llvm::StringRef opString;

public:
    UnaryExpr(UnaryExprKind unaryKind, Expr* expr, llvm::StringRef opString,
              llvm::SMLoc loc, Node* parent = nullptr)
        : Expr(ExprKind::Unary, loc, parent), unaryKind(unaryKind), expr(expr),
          opString(opString) {
        expr->setParent(this);
    }

    UnaryExprKind getUnaryKind() const { return unaryKind; }
    Expr* getExpr() const { return expr; }
    const llvm::StringRef& getOpString() const { return opString; }

    void setExpr(Expr* expr) { this->expr = expr; }

    ACCEPT()
    CLASSOF(Expr, Unary)
};

// Describes a variable acces (either to read or to write).
class VarExpr : public Expr {
    llvm::StringRef name;

public:
    VarExpr(llvm::StringRef name, llvm::SMLoc loc, Node* parent = nullptr)
        : Expr(ExprKind::Var, loc, parent), name(name) {}

    const llvm::StringRef& getName() const { return name; }

    ACCEPT()
    CLASSOF(Expr, Var)

    // VarExpr is a valid assignment destination.
    Expr* makeAssignExpr(Expr* source) override {
        return new AssignExpr(this, source, this->getLoc());
    }
};

// The following classes describe statement nodes of the AST.

// Statement node describing an expression statement.
class ExprStmt : public Stmt {
    Expr* expr;

public:
    ExprStmt(Expr* expr, llvm::SMLoc loc, Node* parent = nullptr)
        : Stmt(StmtKind::Expr, loc, parent), expr(expr) {
        expr->setParent(this);
    }

    Expr* getExpr() const { return expr; }

    void setExpr(Expr* expr) { this->expr = expr; }

    ACCEPT()
    CLASSOF(Stmt, Expr)
};

// Statement node describing an IF statement.
class IfStmt : public Stmt {
    Expr* cond;
    Nodes thenBody;
    Nodes elseBody;

public:
    IfStmt(Expr* cond, Nodes&& thenBody, Nodes&& elseBody, llvm::SMLoc loc,
           Node* parent = nullptr)
        : Stmt(StmtKind::If, loc, parent), cond(cond),
          thenBody(std::move(thenBody)), elseBody(std::move(elseBody)) {
        cond->setParent(this);
        for (auto* stmt : thenBody)
            stmt->setParent(this);
        for (auto* stmt : elseBody)
            stmt->setParent(this);
    }

    Expr* getCond() const { return cond; }
    Nodes& getThenBody() { return thenBody; }
    Nodes& getElseBody() { return elseBody; }

    void setCond(Expr* cond) { this->cond = cond; }

    ACCEPT()
    CLASSOF(Stmt, If)
};

// Statement node descibing a built-in PRINT function call.
class PrintStmt : public Stmt {
    Expr* printExpr;

public:
    PrintStmt(Expr* printExpr, llvm::SMLoc loc, Node* parent = nullptr)
        : Stmt(StmtKind::Print, loc, parent) , printExpr(printExpr) {
        printExpr->setParent(this);
    }

    Expr* getPrintExpr() const { return printExpr; }

    void setPrintExpr(Expr* printExpr) { this->printExpr = printExpr; }

    ACCEPT()
    CLASSOF(Stmt, Print)
};

// Statement node describing a return statement.
class ReturnStmt : public Stmt {
    Expr* retExpr;

public:
    ReturnStmt(Expr* retExpr, llvm::SMLoc loc, Node* parent = nullptr)
        : Stmt(StmtKind::Return, loc, parent), retExpr(retExpr) {
        retExpr->setParent(this);
    }

    Expr* getRetExpr() const { return retExpr; }

    void setRetExpr(Expr* retExpr) { this->retExpr = retExpr; }

    ACCEPT()
    CLASSOF(Stmt, Return)
};

// Statement node describing a built-in SCAN function call.
class ScanStmt : public Stmt {
    VarExpr* scanVar;

public:
    ScanStmt(VarExpr* scanVar, llvm::SMLoc loc, Node* parent = nullptr)
        : Stmt(StmtKind::Scan, loc, parent), scanVar(scanVar) {
        scanVar->setParent(this);
    }

    VarExpr* getScanVar() { return scanVar; }

    ACCEPT()
    CLASSOF(Stmt, Scan)
};

// Statement node describing an until/do statement.
class UntilStmt : public Stmt {
    Expr* cond;
    Nodes body;

public:
    UntilStmt(Expr* cond, Nodes&& body, llvm::SMLoc loc, Node* parent = nullptr)
        : Stmt(StmtKind::Until, loc, parent), cond(cond), body(body) {
        cond->setParent(this);
        for (auto* node : body)
            node->setParent(this);
    }

    Expr* getCond() const { return cond; }
    Nodes& getBody() { return body; }

    void setCond(Expr* cond) { this->cond = cond; }

    ACCEPT()
    CLASSOF(Stmt, Until)
};

// The following classes describe statement nodes of the AST.

// Declaration node describing a module.
// Currently only one module supported per program.
class ModuleDecl : public Decl {
    Decls body;

public:
    ModuleDecl(llvm::StringRef name, Decls&& body, llvm::SMLoc loc)
        : Decl(DeclKind::Module, name, loc, nullptr), body(std::move(body)) {
        for (auto* decl : body)
            decl->setParent(this);
    }

    Decls& getBody() { return body; }

    ACCEPT()
    CLASSOF(Decl, Module)
};

// Declaration node decribing a variable declaration/definition.
class VarDecl : public Decl {
    Type* type;
    Expr* initializer;
    // Whether this is a global variable declaration.
    bool global;

public:
    VarDecl(llvm::StringRef name, Expr* initializer, Type* type, bool global,
            llvm::SMLoc loc, Node* parent = nullptr)
        : Decl(DeclKind::Var, name, loc, parent), type(type),
          initializer(initializer), global(global) {
        if (initializer)
            initializer->setParent(this);
    }

    Type* getType() const { return type; }
    Expr* getInitializer() { return initializer; }
    bool isGlobal() const { return global; }

    void setInitializer(Expr* init) { this->initializer = init; }
    void setGlobal(bool global) { this->global = global; }

    ACCEPT()
    CLASSOF(Decl, Var)
};

// Declaration node describing a function definition.
class FunDecl : public Decl {
    Type* retType;
    FunDeclArgs args;
    Nodes body;

public:
    FunDecl(llvm::StringRef name, Type* retType, FunDeclArgs&& args,
            Nodes&& body, llvm::SMLoc loc, Node* parent = nullptr)
        : Decl(DeclKind::Fun, name, loc, parent), retType(retType),
          args(std::move(args)), body(std::move(body)) {
        for (auto* arg : args)
            arg->setParent(this);
        for (auto* node : body)
            node->setParent(this);
    }

    Type* getRetType() const { return retType; }
    FunDeclArgs& getArgs() { return args; }
    Nodes& getBody() { return body; }

    ACCEPT()
    CLASSOF(Decl, Fun)
};

} // namespace mxrlang

#undef ACCEPT
#undef CLASSOF

#endif // TREE_H
