#ifndef DIAG_H
#define DIAG_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdint>

namespace mxrlang {

enum class DiagID {
#include "Diag.def"
};

class Diag {
    static const char* getDiagText(uint32_t diagType);
    static llvm::SourceMgr::DiagKind getDiagKind(uint32_t diagID);

    llvm::SourceMgr& srcMgr;
    uint32_t numErrs;

public:
    Diag(llvm::SourceMgr& srcMgr) : srcMgr(srcMgr), numErrs(0) {}

    uint32_t getNumErrs() { return numErrs; }

    template <typename... Args>
    void report(llvm::SMLoc loc, uint32_t diagID, Args&&... args) {
        auto msg = llvm::formatv(getDiagText(diagID),
                                 std::forward<Args>(args)...)
                .str();
        auto kind = getDiagKind(diagID);
        srcMgr.PrintMessage(loc, kind, msg);
        numErrs += (kind == llvm::SourceMgr::DK_Error);
    }
};

} // namespace mxrlang

#endif // DIAG_H
