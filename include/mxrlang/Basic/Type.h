#ifndef TYPE_H
#define TYPE_H

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include <cstdint>
#include <unordered_map>

#include "Token.h"

using namespace std::string_literals;

namespace mxrlang {

// Holds the expression type.
class Type {
public:
  enum class TypeKind { Basic, Pointer, Array };

protected:
  TypeKind type;

  // A register of all program types (built-in and user defined).
  static std::unordered_map<std::string, Type *> typeTable;

public:
  Type(TypeKind type) : type(type) {}

  // Convert the type token.
  static Type *getTypeFromToken(const Token &token) {
    if (token.getKind() == TokenKind::kw_BOOL)
      return getBoolType();
    else if (token.getKind() == TokenKind::kw_INT)
      return getIntType();

    return nullptr;
  }

  // Get the built-in bool type.
  static Type *getBoolType() { return typeTable["BOOL"]; }

  // Get the built-in integer type.
  static Type *getIntType() { return typeTable["INT"]; }

  // Get the NONE type, which suggests that the type of expression
  // hasn't been inferred yet.
  static Type *getNoneType() { return typeTable["NONE"]; }

  // Check if the two provided types match.
  static bool checkTypesMatching(const Type *left, const Type *right,
                                 bool arrayDecay = true);

  TypeKind getTypeKind() const { return type; }

  // Return the subtype (only for array and pointer types).
  virtual Type *getSubtype() const { return getNoneType(); }

  // Convert the type to string. Useful when printing out the type.
  virtual std::string toString() const { return ""; }

  // Convert the Mxrlang type to LLVM type.
  virtual llvm::Type *toLLVMType(llvm::LLVMContext &ctx) const = 0;
};

// Holds the mxrlang built-in types.
class BasicType : public Type {
public:
  enum class BasicTypeKind { Bool, Int, None };

private:
  BasicTypeKind basicType;

public:
  BasicType(BasicTypeKind type) : Type(TypeKind::Basic), basicType(type) {}

  // Mxrlang built-in types.
  static BasicType boolType;
  static BasicType intType;
  static BasicType noneType;

  BasicTypeKind getBasicTypeKind() const { return basicType; }

  // Return width of the type in bits.
  int8_t getWidth() const {
    switch (basicType) {
    case BasicTypeKind::Bool:
      return 1;
    case BasicTypeKind::Int:
      return 64;
    default:
      return 0;
    }
  }

  // Convert the type to string. Useful when printing out the type.
  std::string toString() const override {
    if (basicType == BasicTypeKind::Bool)
      return "bool";
    else if (basicType == BasicTypeKind::Int)
      return "int";
    else if (basicType == BasicTypeKind::None)
      return "none";

    llvm_unreachable("Defective type.");
  }

  // Convert the Mxrlang type to LLVM type.
  llvm::Type *toLLVMType(llvm::LLVMContext &ctx) const override {
    if (basicType == BasicTypeKind::Int)
      return llvm::Type::getInt64Ty(ctx);
    else if (basicType == BasicTypeKind::Bool)
      return llvm::Type::getInt1Ty(ctx);
    llvm_unreachable("Unknown type.");
  }

  static bool classof(const Type *node) {
    return node->getTypeKind() == Type::TypeKind::Basic;
  }
};

// Holds the pointer types.
class PointerType : public Type {
private:
  Type *pointeeType;

public:
  PointerType(Type *pointeeType)
      : Type(TypeKind::Pointer), pointeeType(pointeeType) {}

  // Return the subtype (only for array and pointer types).
  Type *getSubtype() const override { return pointeeType; }

  // Convert the type to string. Useful when printing out the type.
  std::string toString() const override {
    return pointeeType->toString() + "*";
  }

  // Convert the Mxrlang type to LLVM type.
  llvm::Type *toLLVMType(llvm::LLVMContext &ctx) const override {
    return llvm::PointerType::get(pointeeType->toLLVMType(ctx), 0);
  }

  static bool classof(const Type *node) {
    return node->getTypeKind() == Type::TypeKind::Pointer;
  }
};

// Holds the array type.
class ArrayType : public Type {
private:
  Type *arrayType;
  // Size of the array (number of elements).
  uint64_t elNum;

public:
  ArrayType(Type *arrayType, uint64_t elNum)
      : Type(TypeKind::Array), arrayType(arrayType), elNum(elNum) {}

  // Return the subtype (only for array and pointer types)
  Type *getSubtype() const override { return arrayType; }
  uint64_t getElNum() const { return elNum; }

  // Convert the type to string. Useful when printing out the type.
  std::string toString() const override {
    auto typeStr = arrayType->toString();
    typeStr += "[" + std::to_string(elNum) + "]";

    return typeStr;
  }

  // Decays the array type to pointer type.
  Type *decay() const { return new PointerType(arrayType); }

  // Convert the Mxrlang type to LLVM type.
  llvm::Type *toLLVMType(llvm::LLVMContext &ctx) const override {
    return llvm::ArrayType::get(arrayType->toLLVMType(ctx), elNum);
  }

  static bool classof(const Type *node) {
    return node->getTypeKind() == Type::TypeKind::Array;
  }
};

} // namespace mxrlang

#endif // TYPE_H
