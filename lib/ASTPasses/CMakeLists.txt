set(LLVM_LINK_COMPONENTS Support)

add_mxrlang_library(mxrlangASTPasses
  ASTPrinter.cpp
  CodeGen.cpp
  SemaCheck.cpp

  LINK_LIBS
  mxrlangBasic
  )
