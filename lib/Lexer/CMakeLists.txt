set(LLVM_LINK_COMPONENTS Support)

add_mxrlang_library(mxrlangLexer
  Lexer.cpp

  LINK_LIBS
  mxrlangBasic
  )
