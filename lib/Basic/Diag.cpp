#include "Diag.h"

namespace mxrlang {
const char* diagText[] = {
#define DIAG(ID, Level, Msg) Msg,
#include "Diag.def"
};
llvm::SourceMgr::DiagKind diagKind[] = {
#define DIAG(ID, Level, Msg) llvm::SourceMgr::DK_##Level,
#include "Diag.def"
};
} // namespace mxrlang

using namespace mxrlang;

const char* Diag::getDiagText(DiagID diagID) {
    return diagText[static_cast<uint32_t>(diagID)];
}

llvm::SourceMgr::DiagKind Diag::getDiagKind(DiagID diagID) {
    return diagKind[static_cast<uint32_t>(diagID)];
}
