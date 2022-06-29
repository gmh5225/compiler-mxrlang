#ifndef TREE_H
#define TREE_H

#include "llvm/ADT/APSInt.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>

#include "Type.h"

// This file contains definitions of abstract syntax tree expression
// and statement classes.

// Generates boilerplate code for each class.
#define ACCEPT()                                                               \
  virtual void accept(Visitor *visitor) override { visitor->visit(this); }

#define CLASSOF(PARENT, KIND)                                                  \
  static bool classof(const PARENT *node) {                                    \
    return node->getKind() == PARENT##Kind::KIND;                              \
  }

namespace mxrlang {

// Forward declare all classes.
class Node;

class Expr;
class ArrayAccessExpr;
class ArrayInitExpr;
class AssignExpr;
class BinaryArithExpr;
class BinaryLogicalExpr;
class BoolLiteralExpr;
class CallExpr;
class IntLiteralExpr;
class LoadExpr;
class PointerOpExpr;
class UnaryExpr;
class VarExpr;

class Stmt;
class ExprStmt;
class IfStmt;
class PrintStmt;
class ReturnStmt;
class ScanStmt;
class WhileStmt;

class Decl;
class FunDecl;
class ModuleDecl;
class VarDecl;

using Nodes = std::vector<Node *>;
using Decls = std::vector<Decl *>;
using Exprs = std::vector<Expr *>;
using FunCallArgs = std::vector<Expr *>;
using FunDeclArgs = std::vector<VarDecl *>;

// Inherit from Visitor class in order to create an AST traversal class.
class Visitor {
public:
  virtual void visit(ArrayAccessExpr *expr) {}
  virtual void visit(ArrayInitExpr *expr) {}
  virtual void visit(AssignExpr *expr) {}
  virtual void visit(BinaryArithExpr *expr) {}
  virtual void visit(BinaryLogicalExpr *expr) {}
  virtual void visit(BoolLiteralExpr *expr) {}
  virtual void visit(CallExpr *expr) {}
  virtual void visit(IntLiteralExpr *expr) {}
  virtual void visit(LoadExpr *expr) {}
  virtual void visit(PointerOpExpr *expr) {}
  virtual void visit(UnaryExpr *expr) {}
  virtual void visit(VarExpr *expr) {}

  virtual void visit(ExprStmt *stmt) {}
  virtual void visit(IfStmt *stmt) {}
  virtual void visit(PrintStmt *stmt) {}
  virtual void visit(ReturnStmt *stmt) {}
  virtual void visit(ScanStmt *stmt) {}
  virtual void visit(WhileStmt *stmt) {}

  virtual void visit(FunDecl *decl) {}
  virtual void visit(ModuleDecl *decl) {}
  virtual void visit(VarDecl *decl) {}
};

// Node class describes a single AST node.
class Node {
public:
  enum class NodeKind { Decl, Expr, Stmt };

private:
  NodeKind kind;
  // Ties the node to the location in the source code. Useful for error
  // reporting.
  llvm::SMLoc loc;

public:
  Node(NodeKind kind, llvm::SMLoc loc) : kind(kind), loc(loc) {}
  virtual ~Node() = default;

  // Pure virtual accept method of the visitor pattern.
  virtual void accept(Visitor *visitor) = 0;

  NodeKind getKind() const { return kind; }
  llvm::SMLoc getLoc() const { return loc; }
};

// Expr class describes expression nodes of the AST.
class Expr : public Node {
public:
  enum class ExprKind {
    ArrayAccess,
    ArrayInit,
    Assign,
    BinaryArith,
    BinaryLogical,
    BoolLiteral,
    Call,
    IntLiteral,
    Load,
    PointerOp,
    Unary,
    Var
  };

private:
  ExprKind kind;
  // Every expression should have a type.
  Type *type;

public:
  Expr(ExprKind kind, llvm::SMLoc loc, Type *type = Type::getNoneType())
      : Node(NodeKind::Expr, loc), kind(kind), type(type) {}

  // Check whether we can take the address of an expression.
  virtual bool canTakeAddressOf() { return false; }

  ExprKind getKind() const { return kind; }
  Type *getType() const { return type; }

  void setType(Type *type) { this->type = type; }

  CLASSOF(Node, Expr)
};

// Stmt class describes statement nodes of the AST.
class Stmt : public Node {
public:
  enum class StmtKind { Expr, Fun, If, Module, Print, Return, Scan, While };

private:
  StmtKind kind;

public:
  Stmt(StmtKind kind, llvm::SMLoc loc)
      : Node(NodeKind::Stmt, loc), kind(kind) {}

  StmtKind getKind() const { return kind; }

  CLASSOF(Node, Stmt)
};

// Decl class describes a declaration node of the AST.
class Decl : public Node {
public:
  enum class DeclKind { Fun, Module, Var };

private:
  DeclKind kind;
  // Every declaration should have a name
  llvm::StringRef name;

public:
  Decl(DeclKind kind, llvm::StringRef name, llvm::SMLoc loc)
      : Node(NodeKind::Decl, loc), kind(kind), name(name) {}

  DeclKind getKind() const { return kind; }
  const llvm::StringRef &getName() const { return name; }

  CLASSOF(Node, Decl)
};

// Describes an array access (e.g. arr[5]).
class ArrayAccessExpr : public Expr {
  // Array.
  Expr *array;
  // Accessed element.
  Expr *element;

public:
  ArrayAccessExpr(Expr *array, Expr *element, llvm::SMLoc loc)
      : Expr(ExprKind::ArrayAccess, loc), array(array), element(element) {}

  Expr *getArray() const { return array; }
  Expr *getElement() const { return element; }

  void setArray(Expr *array) { this->array = array; }
  void setElement(Expr *element) { this->element = element; }

  ACCEPT()
  CLASSOF(Expr, ArrayAccess)

  bool canTakeAddressOf() override { return array->canTakeAddressOf(); }
};

// Describes an array initializer list (e.g. VAR arr : INT[3] := {1,2,3};)
class ArrayInitExpr : public Expr {
  Exprs vals;

public:
  ArrayInitExpr(Exprs vals, llvm::SMLoc loc)
      : Expr(ExprKind::ArrayInit, loc), vals(std::move(vals)) {}

  Exprs &getVals() { return vals; }

  ACCEPT()
  CLASSOF(Expr, ArrayInit)
};

// Describes an assignment (e.g. x := 5).
class AssignExpr : public Expr {
  Expr *dest;
  Expr *source;

public:
  AssignExpr(Expr *dest, Expr *source, llvm::SMLoc loc)
      : Expr(ExprKind::Assign, loc), dest(dest), source(source) {}

  Expr *getDest() const { return dest; }
  Expr *getSource() const { return source; }

  void setDest(Expr *dest) { this->dest = dest; }
  void setSource(Expr *source) { this->source = source; }

  ACCEPT()
  CLASSOF(Expr, Assign)
};

// Describes a binary arithmetic expression (e.g. 5 * x).
class BinaryArithExpr : public Expr {
public:
  enum class BinaryArithExprKind { Add, Div, Mul, Sub };

private:
  BinaryArithExprKind binKind;
  Expr *left;
  Expr *right;
  // Useful for printing out the AST.
  llvm::StringRef opString;

public:
  BinaryArithExpr(BinaryArithExprKind binKind, Expr *left, Expr *right,
                  llvm::StringRef opString, llvm::SMLoc loc)
      : Expr(ExprKind::BinaryArith, loc), binKind(binKind), left(left),
        right(right), opString(opString) {}

  BinaryArithExprKind getBinaryKind() const { return binKind; }
  Expr *getLeft() const { return left; }
  Expr *getRight() const { return right; }
  const llvm::StringRef &getOpString() const { return opString; }

  void setLeft(Expr *left) { this->left = left; }
  void setRight(Expr *right) { this->right = right; }

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
  Expr *left;
  Expr *right;
  // Useful for printing out the AST.
  llvm::StringRef opString;

public:
  BinaryLogicalExpr(BinaryLogicalExprKind binKind, Expr *left, Expr *right,
                    llvm::StringRef opString, llvm::SMLoc loc)
      : Expr(ExprKind::BinaryLogical, loc), binKind(binKind), left(left),
        right(right), opString(opString) {}

  BinaryLogicalExprKind getBinaryKind() const { return binKind; }
  Expr *getLeft() const { return left; }
  Expr *getRight() const { return right; }
  const llvm::StringRef &getOpString() const { return opString; }

  void setLeft(Expr *left) { this->left = left; }
  void setRight(Expr *right) { this->right = right; }

  ACCEPT()
  CLASSOF(Expr, BinaryLogical)
};

// Descibes a BOOL type literal (e.g TRUE).
class BoolLiteralExpr : public Expr {
  bool value;

public:
  BoolLiteralExpr(bool value, llvm::SMLoc loc)
      : Expr(ExprKind::BoolLiteral, loc, Type::getBoolType()), value(value) {}

  bool getValue() const { return value; }

  ACCEPT()
  CLASSOF(Expr, BoolLiteral)
};

// Describes a function call (e.g. fun(5, 6)).
class CallExpr : public Expr {
  llvm::StringRef funName;
  FunCallArgs args;

public:
  CallExpr(llvm::StringRef funName, FunCallArgs &&args, llvm::SMLoc loc)
      : Expr(ExprKind::Call, loc), funName(funName), args(std::move(args)) {}

  const llvm::StringRef &getName() const { return funName; }
  FunCallArgs &getArgs() { return args; }

  ACCEPT()
  CLASSOF(Expr, Call)
};

// Describes an INT type literal (e.g. 1264).
class IntLiteralExpr : public Expr {
  llvm::APSInt value;

public:
  IntLiteralExpr(llvm::StringRef valueString, llvm::SMLoc loc)
      : Expr(ExprKind::IntLiteral, loc, Type::getIntType()) {
    value = llvm::APInt(/* numBits= */ 64, valueString, /* radix= */ 10);
    value.setIsSigned(true);
  }

  IntLiteralExpr(llvm::APSInt value, llvm::SMLoc loc)
      : Expr(ExprKind::IntLiteral, loc, Type::getIntType()), value(value) {}

  const llvm::APSInt &getValue() const { return value; }
  uint64_t getRawValue() const { return *value.getRawData(); }

  ACCEPT()
  CLASSOF(Expr, IntLiteral)
};

// LoadExpr is not syntactically tied to the language. It's used more as a
// notation node which tells us that during code generation, the value which
// is evaluated from LoadExpr's subtree, must be loaded from memory, to be used
// further in the program.
class LoadExpr : public Expr {
  Expr *expr;

public:
  LoadExpr(Expr *expr, llvm::SMLoc loc)
      : Expr(ExprKind::Load, loc), expr(expr) {}

  Expr *getExpr() const { return expr; }

  void setExpr(Expr *expr) { this->expr = expr; }

  ACCEPT()
  CLASSOF(Expr, Load)

  bool canTakeAddressOf() override { return true; }
};

// Describes an operation on a pointer (address-of/dereference).
class PointerOpExpr : public Expr {
public:
  enum class PointerOpKind { AddressOf, Dereference };

private:
  PointerOpKind pointerOpKind;
  // Pointer.
  Expr *expr;
  // Useful for printing out the AST.
  llvm::StringRef opString;

public:
  PointerOpExpr(PointerOpKind pointerOpKind, Expr *expr,
                llvm::StringRef opString, llvm::SMLoc loc)
      : Expr(ExprKind::PointerOp, loc), pointerOpKind(pointerOpKind),
        expr(expr), opString(opString) {}

  PointerOpKind getPointerOpKind() const { return pointerOpKind; }
  Expr *getExpr() const { return expr; }
  const llvm::StringRef &getOpString() const { return opString; }

  void setExpr(Expr *expr) { this->expr = expr; }

  ACCEPT()
  CLASSOF(Expr, PointerOp)
};

// Describes an unary expression (e.g. !x).
class UnaryExpr : public Expr {
public:
  enum class UnaryExprKind { NegArith, NegLogic };

private:
  UnaryExprKind unaryKind;
  Expr *expr;
  // Useful for printing out the AST.
  llvm::StringRef opString;

public:
  UnaryExpr(UnaryExprKind unaryKind, Expr *expr, llvm::StringRef opString,
            llvm::SMLoc loc)
      : Expr(ExprKind::Unary, loc), unaryKind(unaryKind), expr(expr),
        opString(opString) {}

  UnaryExprKind getUnaryKind() const { return unaryKind; }
  Expr *getExpr() const { return expr; }
  const llvm::StringRef &getOpString() const { return opString; }

  void setExpr(Expr *expr) { this->expr = expr; }

  ACCEPT()
  CLASSOF(Expr, Unary)
};

// Describes a variable acces (either to read or to write).
class VarExpr : public Expr {
  llvm::StringRef name;

public:
  VarExpr(llvm::StringRef name, llvm::SMLoc loc)
      : Expr(ExprKind::Var, loc), name(name) {}

  const llvm::StringRef &getName() const { return name; }

  ACCEPT()
  CLASSOF(Expr, Var)

  bool canTakeAddressOf() override { return true; }
};

// The following classes describe statement nodes of the AST.

// Statement node describing an expression statement.
class ExprStmt : public Stmt {
  Expr *expr;

public:
  ExprStmt(Expr *expr, llvm::SMLoc loc)
      : Stmt(StmtKind::Expr, loc), expr(expr) {}

  Expr *getExpr() const { return expr; }

  void setExpr(Expr *expr) { this->expr = expr; }

  ACCEPT()
  CLASSOF(Stmt, Expr)
};

// Statement node describing an IF statement.
class IfStmt : public Stmt {
  Expr *cond;
  Nodes thenBody;
  Nodes elseBody;

public:
  IfStmt(Expr *cond, Nodes &&thenBody, Nodes &&elseBody, llvm::SMLoc loc)
      : Stmt(StmtKind::If, loc), cond(cond), thenBody(std::move(thenBody)),
        elseBody(std::move(elseBody)) {}

  Expr *getCond() const { return cond; }
  Nodes &getThenBody() { return thenBody; }
  Nodes &getElseBody() { return elseBody; }

  void setCond(Expr *cond) { this->cond = cond; }

  ACCEPT()
  CLASSOF(Stmt, If)
};

// Statement node descibing a built-in PRINT function call.
class PrintStmt : public Stmt {
  Expr *printExpr;

public:
  PrintStmt(Expr *printExpr, llvm::SMLoc loc)
      : Stmt(StmtKind::Print, loc), printExpr(printExpr) {}

  Expr *getPrintExpr() const { return printExpr; }

  void setPrintExpr(Expr *printExpr) { this->printExpr = printExpr; }

  ACCEPT()
  CLASSOF(Stmt, Print)
};

// Statement node describing a return statement.
class ReturnStmt : public Stmt {
  Expr *retExpr;

public:
  ReturnStmt(Expr *retExpr, llvm::SMLoc loc)
      : Stmt(StmtKind::Return, loc), retExpr(retExpr) {}

  Expr *getRetExpr() const { return retExpr; }

  void setRetExpr(Expr *retExpr) { this->retExpr = retExpr; }

  ACCEPT()
  CLASSOF(Stmt, Return)
};

// Statement node describing a built-in SCAN function call.
class ScanStmt : public Stmt {
  Expr *scanVar;

public:
  ScanStmt(Expr *scanVar, llvm::SMLoc loc)
      : Stmt(StmtKind::Scan, loc), scanVar(scanVar) {}

  Expr *getScanVar() const { return scanVar; }

  void setScanVar(Expr *scanVar) { this->scanVar = scanVar; }

  ACCEPT()
  CLASSOF(Stmt, Scan)
};

// Statement node describing an while/do statement.
class WhileStmt : public Stmt {
  Expr *cond;
  Nodes body;

public:
  WhileStmt(Expr *cond, Nodes &&body, llvm::SMLoc loc)
      : Stmt(StmtKind::While, loc), cond(cond), body(body) {}

  Expr *getCond() const { return cond; }
  Nodes &getBody() { return body; }

  void setCond(Expr *cond) { this->cond = cond; }

  ACCEPT()
  CLASSOF(Stmt, While)
};

// The following classes describe statement nodes of the AST.

// Declaration node describing a module.
// Currently only one module supported per program.
class ModuleDecl : public Decl {
  Decls body;

public:
  ModuleDecl(llvm::StringRef name, Decls &&body, llvm::SMLoc loc)
      : Decl(DeclKind::Module, name, loc), body(std::move(body)) {}

  Decls &getBody() { return body; }

  ACCEPT()
  CLASSOF(Decl, Module)
};

// Declaration node decribing a variable declaration/definition.
class VarDecl : public Decl {
  Type *type;
  Expr *initializer;
  // If this is a local variable of array type, and it has an initializer,
  // lower the initialization into a list of expressions, each representing
  // an assignment of an initialization expression to an array member.
  Exprs loweredArrayInit;
  // Whether this is a global variable declaration.
  bool global;

public:
  VarDecl(llvm::StringRef name, Expr *initializer, Type *type, bool global,
          llvm::SMLoc loc)
      : Decl(DeclKind::Var, name, loc), type(type), initializer(initializer),
        global(global) {}

  Type *getType() const { return type; }
  Expr *getInitializer() const { return initializer; }
  Exprs &getLoweredArrayInit() { return loweredArrayInit; }
  bool isGlobal() const { return global; }

  void setInitializer(Expr *init) { this->initializer = init; }
  void setLoweredArrayInit(Exprs &&init) { loweredArrayInit = std::move(init); }
  void setGlobal(bool global) { this->global = global; }

  ACCEPT()
  CLASSOF(Decl, Var)
};

// Declaration node describing a function definition.
class FunDecl : public Decl {
  Type *retType;
  FunDeclArgs args;
  Nodes body;

public:
  FunDecl(llvm::StringRef name, Type *retType, FunDeclArgs &&args, Nodes &&body,
          llvm::SMLoc loc)
      : Decl(DeclKind::Fun, name, loc), retType(retType), args(std::move(args)),
        body(std::move(body)) {}

  Type *getRetType() const { return retType; }
  FunDeclArgs &getArgs() { return args; }
  Nodes &getBody() { return body; }

  ACCEPT()
  CLASSOF(Decl, Fun)
};

} // namespace mxrlang

#undef ACCEPT
#undef CLASSOF

#endif // TREE_H
