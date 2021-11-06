#include "Diag.h"

namespace mxrlang {
const char* diagText[] = {
#define DIAG(ID, Level, Msg) Msg,
#include "Diag.def"
};
llvm::SourceMgr::DiagKind diagKind[] = {
#define DIAG(ID, Level, Msg) SourceMgr::DK_##Level,
#include "Diag.def"
};
} // namespace mxrlang

using namespace mxrlang;

const char* Diag::getDiagText(uint32_t diagID) {
    return diagText[diagID];
}

llvm::SourceMgr::DiagKind Diag::getDiagKind(uint32_t diagID) {
    return diagKind[diagID];
}
