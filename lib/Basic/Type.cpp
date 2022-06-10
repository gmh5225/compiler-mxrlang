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
bool Type::checkTypesMatching(const Type *left, const Type *right,
                              bool arrayDecay) {
  if (left->getTypeKind() == TypeKind::Basic) {
    if (right->getTypeKind() != TypeKind::Basic)
      return false;

    // Basic types only live as static members of the BasicType class.
    return left == right;
  } else if (arrayDecay) {
    // Consider array and pointer types equal.
    return checkTypesMatching(left->getSubtype(), right->getSubtype());
  } else if (left->getTypeKind() == TypeKind::Pointer) {
    if (right->getTypeKind() == TypeKind::Pointer)
      return checkTypesMatching(left->getSubtype(), right->getSubtype(), false);

    return false;
  } else if (left->getTypeKind() == TypeKind::Array) {
    auto *leftArray = llvm::dyn_cast<ArrayType>(left);
    if (auto *rightArray = llvm::dyn_cast<ArrayType>(right)) {
      if (leftArray->getElNum() != rightArray->getElNum())
        return false;

      return checkTypesMatching(left->getSubtype(), right->getSubtype(), false);
    }

    return false;
  } else
    llvm_unreachable("Unrecognized type.");
}
