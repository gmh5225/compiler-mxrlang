set(LLVM_LINK_COMPONENTS
  Support
  )

add_mxrlang_tool(mxrlang
  Driver.cpp
  )

target_link_libraries(mxrlang
  PRIVATE
  mxrlangBasic
  mxrlangLexer
  mxrlangParser
  )
