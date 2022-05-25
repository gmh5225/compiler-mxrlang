#ifndef LEXER_H
#define LEXER_H

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

#include "Diag.h"
#include "Token.h"

namespace mxrlang {

// Mapping of keywords to their textual representations.
class KeywordFilter {
  llvm::StringMap<TokenKind> kwTable;

  void addKeyword(llvm::StringRef kw, TokenKind kind);

public:
  void addKeywords();

  TokenKind getKeyword(llvm::StringRef name,
                       TokenKind defaultTokKind = TokenKind::unknown) {
    auto result = kwTable.find(name);
    if (result != kwTable.end())
      return result->second;
    return defaultTokKind;
  }
};

class Lexer {
  llvm::SourceMgr &srcMgr;
  Diag &diag;

  // Buffer containing the source code.
  llvm::StringRef currBuff;
  // Pointer in the buffer to the character that we're currently processing.
  const char *currPtr;

  // This is the current buffer index we're lexing from
  // as managed by the SourceMgr object.
  uint32_t currBuffer = 0;

  // Filter the keywords from the identifiers.
  KeywordFilter keywords;

  // List of all tokens.
  Tokens tokens;

public:
  Lexer(llvm::SourceMgr &srcMgr, Diag &diag) : srcMgr(srcMgr), diag(diag) {
    currBuffer = srcMgr.getMainFileID();
    currBuff = srcMgr.getMemoryBuffer(currBuffer)->getBuffer();
    currPtr = currBuff.begin();
    keywords.addKeywords();
  }

  Diag &getDiag() { return diag; }

  llvm::StringRef getBuffer() const { return currBuff; }

  // Perform lexing.
  Tokens &&lex();

private:
  // Lex an identifier.
  void identifier(Token &result);
  // Lex a number.
  void number(Token &result);

  llvm::SMLoc getLoc() { return llvm::SMLoc::getFromPointer(currPtr); }

  // Create a token, given a pointer to its end in the buffer, and its kind.
  void formToken(Token &result, const char *tokEnd, TokenKind kind);
  // Get the next token.
  void next(Token &result);
};

} // namespace mxrlang

#endif // LEXER_H
