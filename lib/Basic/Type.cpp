#include "Type.h"

using namespace mxrlang;

BasicType BasicType::boolType = BasicType(BasicType::BasicTypeKind::Bool);
BasicType BasicType::intType = BasicType(BasicType::BasicTypeKind::Int);
BasicType BasicType::noneType = BasicType(BasicType::BasicTypeKind::None);

// A register of all program types (built-in and user defined).
std::unordered_map<std::string, Type *> Type::typeTable = {
    {"NONE", &BasicType::noneType},
    {"BOOL", &BasicType::boolType},
    {"INT", &BasicType::intType}};

// Check if the two provided types match.
bool Type::checkTypesMatching(const Type *left, const Type *right) {
  if (left->getTypeKind() == TypeKind::Basic) {
    if (right->getTypeKind() != TypeKind::Basic)
      return false;

    // Basic types only live as static members of the BasicType class.
    return left == right;
  } else if (left->getTypeKind() == TypeKind::Pointer) {
    if (left->getTypeKind() != TypeKind::Pointer)
      return false;

    auto *leftPointer = llvm::dyn_cast<PointerType>(left);
    auto *rightPointer = llvm::dyn_cast<PointerType>(right);

    // Recursively check pointee types.
    return checkTypesMatching(leftPointer->getPointeeType(),
                              rightPointer->getPointeeType());
  }

  return false;
}
