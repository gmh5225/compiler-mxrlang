#ifndef TOKENKINDS_H
#define TOKENKINDS_H

namespace mxrlang {

enum class TokenKind {
#define TOK(ID) ID,
#include "TokenKinds.def"
  NUM_TOKENS
};

const char *getTokenName(TokenKind kind);
const char *getPunctuatorSpelling(TokenKind kind);
const char *getKeywordSpelling(TokenKind kind);

} // namespace mxrlang

#endif // TOKENKINDS_H
