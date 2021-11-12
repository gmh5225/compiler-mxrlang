set(LLVM_LINK_COMPONENTS Support)

add_mxrlang_library(mxrlangParser
  Parser.cpp

  LINK_LIBS
  mxrlangBasic
  mxrlangLexer
  )
