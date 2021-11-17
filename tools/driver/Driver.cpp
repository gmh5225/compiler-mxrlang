#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Target/TargetMachine.h"

#include "ASTPrinter.h"
#include "Diag.h"
#include "Lexer.h"
#include "Parser.h"
#include "SemaCheck.h"
#include "Version.h"

using namespace mxrlang;

static llvm::codegen::RegisterCodeGenFlags CGF;

static llvm::cl::list<std::string>
    inputFiles(llvm::cl::Positional,
               llvm::cl::desc("<input-files>"));

static llvm::cl::opt<bool>
    emitLLVM("emit-llvm",
             llvm::cl::desc("Emit IR code instead of assembler"),
             llvm::cl::init(false));

static const char* Head = "mxrlang - Mxrlang compiler";

void printVersion(llvm::raw_ostream &OS) {
  OS << Head << " " << getMxrlangVersion() << "\n";
  OS << "  Default target: "
     << llvm::sys::getDefaultTargetTriple() << "\n";
  std::string CPU(llvm::sys::getHostCPUName());
  OS << "  Host CPU: " << CPU << "\n";
  OS << "\n";
  OS.flush();
  llvm::TargetRegistry::printRegisteredTargetsForVersion(
      OS);
  exit(EXIT_SUCCESS);
}

llvm::TargetMachine* createTargetMachine(const char* argv0) {
  llvm::Triple triple = llvm::Triple(llvm::sys::getDefaultTargetTriple());

  llvm::TargetOptions targetOptions =
      llvm::codegen::InitTargetOptionsFromCodeGenFlags(triple);
  std::string cpuStr = llvm::codegen::getCPUStr();
  std::string featureStr = llvm::codegen::getFeaturesStr();

  std::string error;
  const llvm::Target* target =
      llvm::TargetRegistry::lookupTarget(llvm::codegen::getMArch(), triple,
                                         error);

  if (!target) {
    llvm::WithColor::error(llvm::errs(), argv0) << error;
    return nullptr;
  }

  llvm::TargetMachine *TM = target->createTargetMachine(
      triple.getTriple(), cpuStr, featureStr, targetOptions,
      llvm::Optional<llvm::Reloc::Model>(llvm::codegen::getRelocModel()));
  return TM;
}

bool emit(llvm::StringRef argv0, llvm::Module* M,
          llvm::TargetMachine* TM,
          llvm::StringRef inputFilename) {
  llvm::CodeGenFileType fileType = llvm::codegen::getFileType();
  std::string outputFilename;
  if (inputFilename == "-") {
    outputFilename = "-";
  } else {
    if (inputFilename.endswith(".mxr") ||
        inputFilename.endswith(".mxr"))
      outputFilename = inputFilename.drop_back(4).str();
    else
      outputFilename = inputFilename.str();
    switch (fileType) {
    case llvm::CGFT_AssemblyFile:
      outputFilename.append(emitLLVM ? ".ll" : ".s");
      break;
    case llvm::CGFT_ObjectFile:
      outputFilename.append(".o");
      break;
    case llvm::CGFT_Null:
      outputFilename.append(".null");
      break;
    }
  }

  // Open the file.
  std::error_code EC;
  llvm::sys::fs::OpenFlags openFlags = llvm::sys::fs::OF_None;
  if (fileType == llvm::CGFT_AssemblyFile)
    openFlags |= llvm::sys::fs::OF_Text;
  auto out = std::make_unique<llvm::ToolOutputFile>(
      outputFilename, EC, openFlags);
  if (EC) {
    llvm::WithColor::error(llvm::errs(), argv0) << EC.message() << '\n';
    return false;
  }

  llvm::legacy::PassManager PM;
  if (fileType == llvm::CGFT_AssemblyFile && emitLLVM) {
    PM.add(llvm::createPrintModulePass(out->os()));
  } else {
    if (TM->addPassesToEmitFile(PM, out->os(), nullptr,
                                fileType)) {
      llvm::WithColor::error() << "No support for file type\n";
      return false;
    }
  }
  PM.run(*M);
  out->keep();
  return true;
}

int main(int argc_, const char **argv_) {
    llvm::InitLLVM x(argc_, argv_);

    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmPrinters();
    llvm::InitializeAllAsmParsers();

    llvm::cl::SetVersionPrinter(&printVersion);
    llvm::cl::ParseCommandLineOptions(argc_, argv_, Head);

    llvm::TargetMachine *TM = createTargetMachine(argv_[0]);
    if (!TM)
      exit(EXIT_FAILURE);

    for (const auto& fileName : inputFiles) {
        llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> file =
                llvm::MemoryBuffer::getFile(fileName);
        if (auto buffErr = file.getError()) {
            llvm::errs() << "Error reading " << fileName << ": "
                         << buffErr.message() << "\n";
            continue;
        }

        llvm::SourceMgr srcMgr;
        Diag diag(srcMgr);

        // Tell SrcMgr about this buffer, which is what the
        // parser will pick up.
        srcMgr.AddNewSourceBuffer(std::move(*file), llvm::SMLoc());

        Lexer lexer(srcMgr, diag);
        auto tokens = std::move(lexer.lex());

        if (diag.getNumErrs() > 0)
            return 0;

        Parser parser(tokens, diag);
        auto moduleDecl = parser.parse();

        if (diag.getNumErrs() > 0)
            return 0;

        ASTPrinter astPrinter;
        astPrinter.run(moduleDecl);

        SemaCheck semaCheck(diag);
        semaCheck.run(moduleDecl);
    }

    return 0;
}
