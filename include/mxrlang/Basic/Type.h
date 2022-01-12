#ifndef TYPE_H
#define TYPE_H

#include "Token.h"

#include "llvm/IR/Type.h"

using namespace std::string_literals;

namespace mxrlang {

// Holds the expression type.
class Type {
public:
    enum class TypeKind {
        Bool,
        Int,
        None
    };

private:
    TypeKind type;

    // Mxrlang built-in types.
    static Type boolType;
    static Type intType;
    static Type noneType;
public:
    Type(TypeKind type) : type(type) {}

    // Convert the type token.
    static Type* getTypeFromToken(const Token& token) {
        if (token.getKind() == TokenKind::kw_BOOL)
            return &boolType;
        else if (token.getKind() == TokenKind::kw_INT)
            return &intType;

        return nullptr;
    }

    // Get the built-in bool type.
    static Type* getBoolType() { return &boolType; }

    // Get the built-in integer type.
    static Type* getIntType() { return &intType; }

    // Get the NONE type, which suggests that the type of expression
    // hasn't been inferred yet.
    static Type* getNoneType() { return &noneType; }

    // Convert the type to string. Useful when printing out the type.
    std::string toString() const {
        if (type == TypeKind::Bool)
            return "bool";
        else if (type == TypeKind::Int)
            return "int";
        else if (type == TypeKind::None)
            return "none";

        llvm_unreachable("Defective type.");
    }
};

}

#endif // TYPE_H
