#ifndef TOKEN_H
#define TOKEN_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <list>

#include "TokenKinds.h"

namespace mxrlang {

class Lexer;

class Token {
  friend class Lexer;

  // Textual contents of the token.
  const char *lexeme;

  uint32_t length;
  TokenKind kind;

public:
  TokenKind getKind() const { return kind; }
  void setKind(TokenKind k) { kind = k; }

  // Helper methods to determine whether a token is (or isn't) of some type.
  bool is(TokenKind k) const { return kind == k; }
  bool isNot(TokenKind k) const { return kind != k; }
  bool isOneOf(TokenKind k1, TokenKind k2) const { return is(k1) || is(k2); }
  template <typename... Ts>
  bool isOneOf(TokenKind k1, TokenKind k2, Ts... ks) const {
    return is(k1) || isOneOf(k2, ks...);
  }

  const char *getName() const { return getTokenName(kind); }

  // Get the location of the token in the source code.
  llvm::SMLoc getLocation() const {
    return llvm::SMLoc::getFromPointer(lexeme);
  }

  uint32_t getLength() { return length; }

  llvm::StringRef getData() const { return llvm::StringRef(lexeme, length); }
};

using Tokens = std::list<Token>;

} // namespace mxrlang

#endif // TOKEN_H
