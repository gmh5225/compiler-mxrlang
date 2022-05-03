#include "llvm/Support/ErrorHandling.h"
#include <cstdint>

#include "TokenKinds.h"

using namespace mxrlang;

static const char* const TokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "TokenKinds.def"
    nullptr
};

const char* mxrlang::getTokenName(TokenKind kind) {
    if (kind < TokenKind::NUM_TOKENS)
        return TokNames[static_cast<uint32_t>(kind)];
    llvm_unreachable("Unknown TokenKind.");
    return nullptr;
}

const char* mxrlang::getPunctuatorSpelling(TokenKind kind) {
    switch (kind) {
#define PUNCTUATOR(ID, SP) case TokenKind::ID: return SP;
#include "TokenKinds.def"
    default:
        llvm_unreachable("TokenKind not a punctuator.");
    }
    return nullptr;
}

const char* mxrlang::getKeywordSpelling(TokenKind kind) {
    switch (kind) {
#define KEYWORD(ID, FLAG) case TokenKind::kw_##ID: return #ID;
#include "TokenKinds.def"
    default:
        llvm_unreachable("TokenKind not a keyword.");
    }
    return nullptr;
}
