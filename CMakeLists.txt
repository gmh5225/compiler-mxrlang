cmake_minimum_required(VERSION 3.4.3)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    project(Mxrlang)

    find_package(LLVM REQUIRED HINTS "${LLVM_CMAKE_PATH}")
    list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})
    include(ChooseMSVCCRT)

    include(AddLLVM)
    include(HandleLLVMOptions)

    include_directories("${LLVM_BINARY_DIR}/include" "${LLVM_INCLUDE_DIR}")
    link_directories("${LLVM_LIBRARY_DIR}")

    set(MXRLANG_BUILT_STANDALONE 1)
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE)
  message(FATAL_ERROR "In-source builds are not allowed. ")
endif()

set(MXRLANG_VERSION_STRING "0.1")
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/include/mxrlang/Basic/Version.inc.in
  ${CMAKE_CURRENT_BINARY_DIR}/include/mxrlang/Basic/Version.inc)

include(AddMxrlang)

include_directories(BEFORE
  ${CMAKE_CURRENT_BINARY_DIR}/include/mxrlang/Basic
  ${CMAKE_CURRENT_BINARY_DIR}/include/mxrlang/Lexer
  ${CMAKE_CURRENT_BINARY_DIR}/include/mxrlang/Parser
  ${CMAKE_CURRENT_BINARY_DIR}/include/mxrlang/ASTPasses
  ${CMAKE_CURRENT_SOURCE_DIR}/include/mxrlang/Basic
  ${CMAKE_CURRENT_SOURCE_DIR}/include/mxrlang/Lexer
  ${CMAKE_CURRENT_SOURCE_DIR}/include/mxrlang/Parser
  ${CMAKE_CURRENT_SOURCE_DIR}/include/mxrlang/ASTPasses
  )

add_subdirectory(lib)
add_subdirectory(tools)
