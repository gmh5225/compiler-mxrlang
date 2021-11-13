set(LLVM_LINK_COMPONENTS Support)

add_mxrlang_library(mxrlangASTPasses
  ASTPrinter.cpp

  LINK_LIBS
  mxrlangBasic
  )
