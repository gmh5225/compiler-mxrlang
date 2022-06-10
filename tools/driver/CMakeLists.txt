set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Analysis
  CodeGen
  Core
  IPO
  AggressiveInstCombine
  InstCombine
  Instrumentation
  MC
  MCParser
  ObjCARCOpts
  Option
  ScalarOpts
  Support
  TransformUtils
  Vectorize
  AggressiveInstCombine Analysis AsmParser
  BitWriter CodeGen Core Coroutines IPO IRReader
  InstCombine Instrumentation MC ObjCARCOpts Remarks
  ScalarOpts Support Target TransformUtils Vectorize
  Passes)

add_mxrlang_tool(mxrlang
  Driver.cpp
  )

target_link_libraries(mxrlang
  PRIVATE
  mxrlangBasic
  mxrlangLexer
  mxrlangParser
  mxrlangASTPasses
  )
