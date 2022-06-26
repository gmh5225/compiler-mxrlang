#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Utils.h"

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

#include "ASTPrinter.h"
#include "CodeGen.h"
#include "Diag.h"
#include "Lexer.h"
#include "Parser.h"
#include "SemaCheck.h"
#include "Version.h"

using namespace mxrlang;

static llvm::codegen::RegisterCodeGenFlags CGF;

static llvm::cl::list<std::string> inputFiles(llvm::cl::Positional,
                                              llvm::cl::desc("<input-files>"));

static llvm::cl::opt<bool>
    emitLLVM("emit-llvm", llvm::cl::desc("Emit IR code instead of assembler"),
             llvm::cl::init(false));

static llvm::cl::opt<bool>
    printAST("print-ast", llvm::cl::desc("Print the AST of the modules"),
             llvm::cl::init(false));

static llvm::cl::opt<signed char> OptLevel(
    llvm::cl::desc("Setting the optimization level:"), llvm::cl::ZeroOrMore,
    llvm::cl::values(clEnumValN(3, "O", "Equivalent to -O3"),
                     clEnumValN(0, "O0", "Optimization level 0"),
                     clEnumValN(1, "O1", "Optimization level 1"),
                     clEnumValN(2, "O2", "Optimization level 2"),
                     clEnumValN(3, "O3", "Optimization level 3"),
                     clEnumValN(-1, "Os",
                                "Like -O2 with extra optimizations "
                                "for size"),
                     clEnumValN(-2, "Oz",
                                "Like -Os but reduces code size further")),
    llvm::cl::init(0));

static llvm::cl::opt<std::string>
    PassPipeline("passes",
                 llvm::cl::desc("A description of the pass pipeline"));

static const char *Head = "mxrlang - Mxrlang compiler";

void printVersion(llvm::raw_ostream &OS) {
  OS << Head << " " << getMxrlangVersion() << "\n";
  OS << "  Default target: " << llvm::sys::getDefaultTargetTriple() << "\n";
  std::string CPU(llvm::sys::getHostCPUName());
  OS << "  Host CPU: " << CPU << "\n";
  OS << "\n";
  OS.flush();
  llvm::TargetRegistry::printRegisteredTargetsForVersion(OS);
  exit(EXIT_SUCCESS);
}

llvm::TargetMachine *createTargetMachine(const char *argv0) {
  llvm::Triple triple = llvm::Triple(llvm::sys::getDefaultTargetTriple());

  llvm::TargetOptions targetOptions =
      llvm::codegen::InitTargetOptionsFromCodeGenFlags(triple);
  std::string cpuStr = llvm::codegen::getCPUStr();
  std::string featureStr = llvm::codegen::getFeaturesStr();

  std::string error;
  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(
      llvm::codegen::getMArch(), triple, error);

  if (!target) {
    llvm::WithColor::error(llvm::errs(), argv0) << error;
    return nullptr;
  }

  llvm::TargetMachine *TM = target->createTargetMachine(
      triple.getTriple(), cpuStr, featureStr, targetOptions,
      llvm::Optional<llvm::Reloc::Model>(llvm::Reloc::Model::PIC_));
  return TM;
}

bool emit(llvm::StringRef argv0, llvm::Module *M, llvm::TargetMachine *TM,
          llvm::StringRef inputFilename) {
  // Create the optimization pipeline.
  llvm::PassBuilder PB(TM);

  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  // Register the AA manager first so that our version
  // is the one used.
  FAM.registerPass([&] { return PB.buildDefaultAAPipeline(); });

  // Register all the basic analyses with the managers.
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::ModulePassManager MPM;

  if (!PassPipeline.empty()) {
    if (auto err = PB.parsePassPipeline(MPM, PassPipeline)) {
      llvm::WithColor::error(llvm::errs(), argv0)
          << llvm::toString(std::move(err)) << "\n";
      return false;
    }
  } else {
    llvm::StringRef defaultPass = "default<O0>";
    if (auto err = PB.parsePassPipeline(MPM, defaultPass)) {
      llvm::WithColor::error(llvm::errs(), argv0)
          << llvm::toString(std::move(err)) << "\n";
      return false;
    }
  }

  llvm::CodeGenFileType fileType = llvm::codegen::getFileType();
  std::string outputFilename;
  // REPL
  if (inputFilename == "-") {
    outputFilename = "-";
  } else {
    // Input file sould have an .mxr extension.
    // Output file will have the same name as the input file (with
    // different extension).
    if (inputFilename.endswith(".mxr"))
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
  auto out =
      std::make_unique<llvm::ToolOutputFile>(outputFilename, EC, openFlags);
  if (EC) {
    llvm::WithColor::error(llvm::errs(), argv0) << EC.message() << '\n';
    return false;
  }

  // Create the legacy pass manager for code gen.
  llvm::legacy::PassManager codeGenPM;
  if (fileType == llvm::CGFT_AssemblyFile && emitLLVM) {
    codeGenPM.add(llvm::createPrintModulePass(out->os()));
  } else {
    if (TM->addPassesToEmitFile(codeGenPM, out->os(), nullptr, fileType)) {
      llvm::WithColor::error() << "No support for file type\n";
      return false;
    }
  }

  MPM.run(*M, MAM);
  codeGenPM.run(*M);
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

  for (const auto &fileName : inputFiles) {
    // Get file buffer.
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> file =
        llvm::MemoryBuffer::getFile(fileName);
    if (auto buffErr = file.getError()) {
      llvm::errs() << "Error reading " << fileName << ": " << buffErr.message()
                   << "\n";
      continue;
    }

    llvm::SourceMgr srcMgr;
    // Diagnostics manager, used for error reports.
    Diag diag(srcMgr);

    // Tell SrcMgr about this buffer, which is what the
    // parser will pick up.
    srcMgr.AddNewSourceBuffer(std::move(*file), llvm::SMLoc());

    // Create and run the lexer.
    Lexer lexer(srcMgr, diag);
    auto tokens = std::move(lexer.lex());

    if (diag.getNumErrs() > 0)
      continue;

    // Create and run the parser.
    Parser parser(tokens, diag);
    auto moduleDecl = parser.parse();

    if (diag.getNumErrs() > 0)
      continue;

    // Create and run the semantic checker.
    SemaCheck semaCheck(diag);
    semaCheck.run(moduleDecl);

    if (diag.getNumErrs() > 0)
      continue;

    // Helper pass which prints the AST.
    if (printAST) {
      ASTPrinter astPrinter;
      astPrinter.run(moduleDecl);
    }

    if (diag.getNumErrs() > 0)
      continue;

    // Generate code for this module.
    if (moduleDecl) {
      CodeGen codeGen(TM, fileName, diag);
      codeGen.run(moduleDecl);
      if (!emit(argv_[0], codeGen.getModule(), TM, fileName))
        llvm::WithColor::error(llvm::errs(), argv_[0]) << "Error"
                                                          " writing output\n";
    } else
      continue;
  }

  return 0;
}
