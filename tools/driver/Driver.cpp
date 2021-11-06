#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"

#include "Diag.h"

using namespace mxrlang;

int main(int argc_, const char **argv_) {
    llvm::InitLLVM x(argc_, argv_);
    llvm::SmallVector<const char*, 256> argv(argv_ + 1, argv_ + argc_);

    for (auto* fileName : argv) {
        llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> file =
                llvm::MemoryBuffer::getFile(fileName);
        if (auto buffErr = file.getError()) {
            llvm::errs() << "Error reading " << fileName << ": "
                         << buffErr.message() << "\n";
            continue;
        }

        llvm::SourceMgr srcMgr;
        Diag diag(srcMgr);
    }

    return 0;
}
