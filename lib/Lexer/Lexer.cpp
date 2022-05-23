#include "Lexer.h"

using namespace mxrlang;

void KeywordFilter::addKeyword(llvm::StringRef kw, TokenKind kind) {
  kwTable.insert(std::make_pair(kw, kind));
}

void KeywordFilter::addKeywords() {
#define KEYWORD(NAME, FLAGS)                                                   \
  addKeyword(llvm::StringRef(#NAME), TokenKind::kw_##NAME);
#include "TokenKinds.def"
}

namespace charinfo {

inline bool isVerticalWhitespace(char ch) { return ch == '\r' || ch == '\n'; }

inline bool isHorizontalWhitespace(char ch) {
  return ch == ' ' || ch == '\t' || ch == '\f' || ch == '\v';
}

inline bool isWhitespace(char ch) {
  return isVerticalWhitespace(ch) || isHorizontalWhitespace(ch);
}

inline bool isDigit(char ch) { return ch >= '0' && ch <= '9'; }

inline bool isIdentHead(char ch) {
  return ch == '_' || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
}

inline bool isIdentBody(char ch) { return isIdentHead(ch) || isDigit(ch); }

} // namespace charinfo

void Lexer::identifier(Token &result) {
  const char *start = currPtr;
  const char *end = currPtr + 1;
  while (charinfo::isIdentBody(*end))
    end++;
  llvm::StringRef name(start, end - start);
  formToken(result, end, keywords.getKeyword(name, TokenKind::identifier));
  (void)result.getName();
}

void Lexer::number(Token &result) {
  const char *end = currPtr + 1;
  while (charinfo::isDigit(*end))
    end++;
  formToken(result, end, TokenKind::integer_literal);
}

void Lexer::next(Token &result) {
  while (*currPtr && charinfo::isWhitespace(*currPtr))
    currPtr++;
  if (!*currPtr) {
    result.setKind(TokenKind::eof);
    return;
  }
  if (charinfo::isIdentHead(*currPtr)) {
    identifier(result);
    return;
  } else if (charinfo::isDigit(*currPtr)) {
    number(result);
    return;
  } else {
    switch (*currPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(result, currPtr + 1, tok);                                       \
    break;

      CASE(')', TokenKind::closedpar)
      CASE(',', TokenKind::comma)
      CASE('=', TokenKind::equal)
      CASE('-', TokenKind::minus)
      CASE('(', TokenKind::openpar)
      CASE('+', TokenKind::plus)
      CASE(';', TokenKind::semicolon)
      CASE('/', TokenKind::slash)
      CASE('*', TokenKind::star)
#undef CASE
    case ':':
      if (*(currPtr + 1) == '=')
        formToken(result, currPtr + 2, TokenKind::colonequal);
      else
        formToken(result, currPtr + 1, TokenKind::colon);
      break;
    case '<':
      if (*(currPtr + 1) == '=')
        formToken(result, currPtr + 2, TokenKind::lesseq);
      else
        formToken(result, currPtr + 1, TokenKind::less);
      break;
    case '>':
      if (*(currPtr + 1) == '=')
        formToken(result, currPtr + 2, TokenKind::greatereq);
      else
        formToken(result, currPtr + 1, TokenKind::greater);
      break;
    case '!':
      if (*(currPtr + 1) == '=')
        formToken(result, currPtr + 2, TokenKind::noteq);
      else
        formToken(result, currPtr + 1, TokenKind::bang);
      break;
    case '&':
      if (*(currPtr + 1) == '&')
        formToken(result, currPtr + 2, TokenKind::logicand);
      else
        formToken(result, currPtr + 1, TokenKind::ampersand);
      break;
    case '|':
      if (*(currPtr + 1) == '|')
        formToken(result, currPtr + 2, TokenKind::logicor);
      break;
    default:
      diag.report(getLoc(), DiagID::err_unknown_token);
      result.setKind(TokenKind::unknown);
    }
  }
  return;
}

void Lexer::formToken(Token &result, const char *tokEnd, TokenKind kind) {
  uint32_t tokLen = tokEnd - currPtr;
  result.lexeme = currPtr;
  result.length = tokLen;
  result.kind = kind;
  currPtr = tokEnd;
}

Tokens &&Lexer::lex() {
  Token tok;
  do {
    next(tok);
    tokens.push_back(tok);
  } while (!tok.is(TokenKind::eof) && !tok.is(TokenKind::unknown));

  return std::move(tokens);
}
