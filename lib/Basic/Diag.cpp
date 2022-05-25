#include "Diag.h"

namespace mxrlang {
const char *diagText[] = {
#define DIAG(ID, Level, Msg) Msg,
#include "Diag.def"
};
llvm::SourceMgr::DiagKind diagKind[] = {
#define DIAG(ID, Level, Msg) llvm::SourceMgr::DK_##Level,
#include "Diag.def"
};
} // namespace mxrlang

using namespace mxrlang;

// Returns the error text based on the error ID.
const char *Diag::getDiagText(DiagID diagID) {
  return diagText[static_cast<uint32_t>(diagID)];
}

// Returns the error kind based on the error ID.
llvm::SourceMgr::DiagKind Diag::getDiagKind(DiagID diagID) {
  return diagKind[static_cast<uint32_t>(diagID)];
}
