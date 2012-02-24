//===- llvm-prof.cpp - Read in and process llvmprof.out data files --------===//
//
//                      The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This tools is meant for use with the various LLVM profiling instrumentation
// passes.  It reads in the data file produced by executing an instrumented
// program, and outputs a nice report.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "llvm-prof"
#include "llvm/Instructions.h"
#include "llvm/InstrTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Assembly/AssemblyAnnotationWriter.h"
#include "llvm/Analysis/ProfileInfo.h"
#include "llvm/Analysis/ProfileInfoLoader.h"
#include "llvm/Analysis/IndirectFunctionCallProfileInfo.h"
#include "llvm/Analysis/TraceProfileInfo.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/system_error.h"
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <map>
#include <set>

using namespace llvm;

//----------------------------------------------------------------------------//
//===--------------------- Command Line Arguments -------------------------===//
//----------------------------------------------------------------------------//
namespace {
  cl::opt<std::string>
  BitcodeFile(cl::Positional, cl::desc("<program bitcode file>"),
              cl::Required);

  cl::opt<std::string>
  ProfileDataFile(cl::Positional, cl::desc("<llvmprof.out file>"),
                  cl::Optional, cl::init("llvmprof.out"));

  cl::opt<bool>
  PrintAnnotatedLLVM("annotated-llvm",
                     cl::desc("Print LLVM code with frequency annotations"));
  cl::alias PrintAnnotated2("A", cl::desc("Alias for --annotated-llvm"),
                            cl::aliasopt(PrintAnnotatedLLVM));
  cl::opt<bool>
  PrintAllCode("print-all-code",
               cl::desc("Print annotated code for the entire program"));

  // Type of data stored in the profile input file
  enum ProfileTypes {
    EdgeProfileType
  , IFCProfileType
  , TraceProfileType
  };
  cl::opt<ProfileTypes>
  ProfileType("profile-type", cl::desc("Profiling data file type:"),
    cl::values(clEnumValN(EdgeProfileType,"edge","Edge/basic Block profile"),
               clEnumValN(IFCProfileType,"ifc","Indirect function call profile"),
               clEnumValN(TraceProfileType,"trace","Trace profile"),
               clEnumValEnd));
}

// PairSecondSort - A sorting predicate to sort by the second element of a pair.
template<class T>
struct PairSecondSortReverse
  : public std::binary_function<std::pair<T, double>,
                                std::pair<T, double>, bool> {
  bool operator()(const std::pair<T, double> &LHS,
                  const std::pair<T, double> &RHS) const {
    return LHS.second > RHS.second;
  }
};

static double ignoreMissing(double w) {
  if (w == ProfileInfo::MissingValue) return 0;
  return w;
}

//----------------------------------------------------------------------------//
//===------------------ Edge/Basic Block Profiles -------------------------===//
//----------------------------------------------------------------------------//
namespace {
  class ProfileAnnotator : public AssemblyAnnotationWriter {
    ProfileInfo &PI;
  public:
    ProfileAnnotator(ProfileInfo &pi) : PI(pi) {}

    virtual void emitFunctionAnnot(const Function *F,
                                   formatted_raw_ostream &OS) {
      double w = PI.getExecutionCount(F);
      if (w != ProfileInfo::MissingValue) {
        OS << ";;; %" << F->getName() << " called "<<(unsigned)w
           <<" times.\n;;;\n";
      }
    }
    virtual void emitBasicBlockStartAnnot(const BasicBlock *BB,
                                          formatted_raw_ostream &OS) {
      double w = PI.getExecutionCount(BB);
      if (w != ProfileInfo::MissingValue) {
        if (w != 0) {
          OS << "\t;;; Basic block executed " << (unsigned)w << " times.\n";
        } else {
          OS << "\t;;; Never executed!\n";
        }
      }
    }

    virtual void emitBasicBlockEndAnnot(const BasicBlock *BB,
                                        formatted_raw_ostream &OS) {
      // Figure out how many times each successor executed.
      std::vector<std::pair<ProfileInfo::Edge, double> > SuccCounts;

      const TerminatorInst *TI = BB->getTerminator();
      for (unsigned s = 0, e = TI->getNumSuccessors(); s != e; ++s) {
        BasicBlock* Succ = TI->getSuccessor(s);
        double w = ignoreMissing(PI.getEdgeWeight(std::make_pair(BB, Succ)));
        if (w != 0)
          SuccCounts.push_back(std::make_pair(std::make_pair(BB, Succ), w));
      }
      if (!SuccCounts.empty()) {
        OS << "\t;;; Out-edge counts:";
        for (unsigned i = 0, e = SuccCounts.size(); i != e; ++i)
          OS << " [" << (SuccCounts[i]).second << " -> "
             << (SuccCounts[i]).first.second->getName() << "]";
        OS << "\n";
      }
    }
  };
}

namespace {
  /// ProfileInfoPrinterPass - Helper pass to dump the profile information for
  /// a module.
  //
  // FIXME: This should move elsewhere.
  class ProfileInfoPrinterPass : public ModulePass {
    ProfileInfoLoader &PIL;
  public:
    static char ID; // Class identification, replacement for typeinfo.
    explicit ProfileInfoPrinterPass(ProfileInfoLoader &_PIL) 
      : ModulePass(ID), PIL(_PIL) {}

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<ProfileInfo>();
    }

    bool runOnModule(Module &M);
  };
}

char ProfileInfoPrinterPass::ID = 0;

bool ProfileInfoPrinterPass::runOnModule(Module &M) {
  ProfileInfo &PI = getAnalysis<ProfileInfo>();
  std::map<const Function  *, unsigned> FuncFreqs;
  std::map<const BasicBlock*, unsigned> BlockFreqs;
  std::map<ProfileInfo::Edge, unsigned> EdgeFreqs;

  // Output a report. Eventually, there will be multiple reports selectable on
  // the command line, for now, just keep things simple.

  // Emit the most frequent function table...
  std::vector<std::pair<Function*, double> > FunctionCounts;
  std::vector<std::pair<BasicBlock*, double> > Counts;
  for (Module::iterator FI = M.begin(), FE = M.end(); FI != FE; ++FI) {
    if (FI->isDeclaration()) continue;
    double w = ignoreMissing(PI.getExecutionCount(FI));
    FunctionCounts.push_back(std::make_pair(FI, w));
    for (Function::iterator BB = FI->begin(), BBE = FI->end(); 
         BB != BBE; ++BB) {
      double w = ignoreMissing(PI.getExecutionCount(BB));
      Counts.push_back(std::make_pair(BB, w));
    }
  }

  // Sort by the frequency, backwards.
  sort(FunctionCounts.begin(), FunctionCounts.end(),
            PairSecondSortReverse<Function*>());

  double TotalExecutions = 0;
  for (unsigned i = 0, e = FunctionCounts.size(); i != e; ++i)
    TotalExecutions += FunctionCounts[i].second;

  outs() << "===" << std::string(73, '-') << "===\n"
         << "LLVM profiling output for execution";
  if (PIL.getNumExecutions() != 1) outs() << "s";
  outs() << ":\n";

  for (unsigned i = 0, e = PIL.getNumExecutions(); i != e; ++i) {
    outs() << "  ";
    if (e != 1) outs() << i+1 << ". ";
    outs() << PIL.getExecution(i) << "\n";
  }

  outs() << "\n===" << std::string(73, '-') << "===\n";
  outs() << "Function execution frequencies:\n\n";

  // Print out the function frequencies...
  outs() << " ##   Frequency\n";
  for (unsigned i = 0, e = FunctionCounts.size(); i != e; ++i) {
    if (FunctionCounts[i].second == 0) {
      outs() << "\n  NOTE: " << e-i << " function" 
        << (e-i-1 ? "s were" : " was") << " never executed!\n";
      break;
    }

    outs() << format("%3d", i+1) << ". "
      << format("%5.2g", FunctionCounts[i].second) << "/"
      << format("%g", TotalExecutions) << " "
      << FunctionCounts[i].first->getNameStr() << "\n";
  }

  std::set<Function*> FunctionsToPrint;

  TotalExecutions = 0;
  for (unsigned i = 0, e = Counts.size(); i != e; ++i)
    TotalExecutions += Counts[i].second;
  
  // Sort by the frequency, backwards.
  sort(Counts.begin(), Counts.end(),
       PairSecondSortReverse<BasicBlock*>());
  
  outs() << "\n===" << std::string(73, '-') << "===\n";
  outs() << "Top 20 most frequently executed basic blocks:\n\n";
  
  // Print out the function frequencies...
  outs() <<" ##      %% \tFrequency\n";
  unsigned BlocksToPrint = Counts.size();
  if (BlocksToPrint > 20) BlocksToPrint = 20;
  for (unsigned i = 0; i != BlocksToPrint; ++i) {
    if (Counts[i].second == 0) break;
    Function *F = Counts[i].first->getParent();
    outs() << format("%3d", i+1) << ". " 
      << format("%5g", Counts[i].second/(double)TotalExecutions*100) << "% "
      << format("%5.0f", Counts[i].second) << "/"
      << format("%g", TotalExecutions) << "\t"
      << F->getNameStr() << "() - "
       << Counts[i].first->getNameStr() << "\n";
    FunctionsToPrint.insert(F);
  }

  if (PrintAnnotatedLLVM || PrintAllCode) {
    outs() << "\n===" << std::string(73, '-') << "===\n";
    outs() << "Annotated LLVM code for the module:\n\n";
  
    ProfileAnnotator PA(PI);

    if (FunctionsToPrint.empty() || PrintAllCode)
      M.print(outs(), &PA);
    else
      // Print just a subset of the functions.
      for (std::set<Function*>::iterator I = FunctionsToPrint.begin(),
             E = FunctionsToPrint.end(); I != E; ++I)
        (*I)->print(outs(), &PA);
  }

  return false;
}

//----------------------------------------------------------------------------//
//===--------------- Indirect Function Call Profiles ----------------------===//
//----------------------------------------------------------------------------//
namespace {
  class IFCProfileAnnotator : public AssemblyAnnotationWriter {
    IndirectFunctionCallProfileInfo &PI;
  public:
    IFCProfileAnnotator(IndirectFunctionCallProfileInfo &pi) : PI(pi) {}
    void emitInstructionAnnot(const Instruction *, formatted_raw_ostream &);
  };
}

void IFCProfileAnnotator::
emitInstructionAnnot(const Instruction *I, formatted_raw_ostream& OS) {
  const CallInst *Call = dyn_cast<CallInst>(I);
  if(Call && PI.hasProfileInfo(Call)) {
    OS << ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
    OS << ";;; Indirect Targets:\n";
    const CallSiteProfile &Targets = PI.getCallSiteProfile(Call);
    for(CallSiteProfile::const_iterator T = Targets.begin(), TE = Targets.end();
        T!=TE; ++T) {
      const std::string &TargetName = T->Target->getName().str();
      OS << ";;;\t"
         << "@"  << TargetName
         << " "  << format("%3.2f", 100.0 * T->Percent) << "%"
         << " (" << T->Count << " times)\n";
    }
    OS << ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
  }
}

namespace {
  /// IFCProfileInfoPrinterPass - Helper pass to dump the ifc profile
  /// information for a module.
  //
  class IFCProfileInfoPrinterPass : public ModulePass {
  public:
    static char ID; // Class identification, replacement for typeinfo.
    IFCProfileInfoPrinterPass() : ModulePass(ID) {}

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<IndirectFunctionCallProfileInfo>();
    }

    // the full name of the printer pass
    virtual const char* getPassName() const {
      return "IFC Profiling Information Printer";
    }

    bool runOnModule(Module &M);
  };
}

char IFCProfileInfoPrinterPass::ID = 0;
bool IFCProfileInfoPrinterPass::runOnModule(Module& M) {
  IndirectFunctionCallProfileInfo &PI =
    getAnalysis<IndirectFunctionCallProfileInfo>();
  DEBUG(dbgs() << "IFC Profiling info has entries for "
        << PI.getProfileMap().size() <<" call sites\n");

  typedef std::vector<CallInst *> CallVec;
  typedef std::map<Function*, CallVec> FunctionProfileMap;
  FunctionProfileMap Map;

  // Run through the module and collect all the callsites for which we have
  // profiling info
  int NumCallSites = 0;
  for (Module::iterator F = M.begin(), FE = M.end(); F != FE; ++F) {
    for (Function::iterator BB = F->begin(), BBE = F->end(); BB != BBE; ++BB) {
      for(BasicBlock::iterator I = BB->begin(), IE = BB->end(); I!=IE; ++I) {
        CallInst *C = dyn_cast<CallInst>(&*I);
        if(!C) continue;

        if(PI.hasProfileInfo(C)) {
          NumCallSites++;
          Map[F].push_back(C);
        }
      }
    }
  }

  DEBUG(dbgs() << "Processing " << NumCallSites << " call sites in "
        << Map.size() << " functions\n");

  // Print out all the call sites we found with profiling info
  for(FunctionProfileMap::iterator I = Map.begin(), E = Map.end(); I!=E; ++I) {
    Function *F = I->first;
    CallVec &Calls = I->second;
    outs() << F->getName() << ":\n";
    for(CallVec::iterator C = Calls.begin(), CE = Calls.end(); C!=CE; ++C) {
      outs() << " @BB_" << (*C)->getParent()->getName() << "\n";
      const CallSiteProfile &Targets = PI.getCallSiteProfile(*C);
      for(CallSiteProfile::const_iterator T = Targets.begin(), TE = Targets.end();
          T!=TE; ++T){
        const std::string &CalledValue= (*C)->getCalledValue()->getName().str();
        const std::string &TargetName = T->Target->getName().str();
        std::cout << "    "
                  << std::setw(20) << std::right << CalledValue  << "() => "
                  << std::setw(30) << std::left  << TargetName   << " "
                  << std::setw(6)  << std::right << std::setprecision(2)
                  << std::fixed    << (100 * T->Percent) << "%"
                  << " ("<<T->Count<< ")" << "\n";
      }
    }
    outs() << "\n";
  }

  // Print the annotated llvm if requested
  if (PrintAnnotatedLLVM) {
    outs() << "\n===" << std::string(73, '-') << "===\n";
    outs() << "Annotated LLVM code for the module:\n\n";

    IFCProfileAnnotator PA(PI);
    if(PrintAllCode) M.print(outs(), &PA);
    else {
      for(FunctionProfileMap::iterator I = Map.begin(), E = Map.end(); I!=E; ++I) {
        Function *F = I->first;
        F->print(outs(), &PA);
      }
    }
  }
  return false;
}

//----------------------------------------------------------------------------//
//===---------------         Trace Profiles          ----------------------===//
//----------------------------------------------------------------------------//
namespace {
  /// IFCProfileInfoPrinterPass - Helper pass to dump the ifc profile
  /// information for a module.
  //
  class TraceProfileInfoPrinterPass : public ModulePass {
  public:
    static char ID; // Class identification, replacement for typeinfo.
    TraceProfileInfoPrinterPass() : ModulePass(ID) {}

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<TraceProfileInfo>();
    }

    // the full name of the printer pass
    virtual const char* getPassName() const {
      return "Trace Profiling Information Printer";
    }

    bool runOnModule(Module &M);
  };
}

char TraceProfileInfoPrinterPass::ID = 0;
bool TraceProfileInfoPrinterPass::runOnModule(Module& M) {
    TraceProfileInfo &PI = getAnalysis<TraceProfileInfo>();
    outs() << PI.getTraces().size() << " Traces found\n";
    outs() << PI.brokenTraceCount() << " Broken traces found\n";
    int TraceId = 0;

    TraceProfileList& Traces = PI.getTraces();
    for(TraceProfileList::iterator I = Traces.begin(), E = Traces.end();
        I!=E; ++I) {
      TraceProfile& Trace = *I;
      TraceProfile::FunctionList& Functions = Trace.getFunctions();
      TraceId++;

      outs() << std::string(80, '-') << "\n";
      outs() << "Trace #" << TraceId
             << " @" << Functions.front()->getName().str()
             << " (" << Trace.Blocks.size() << " Blocks"
             << " in "    << Functions.size()    << " Functions) "
             << format("%6.2f", (Trace.ExecutionPercent * 100)) << "%"
             << "\n";
      outs() << std::string(80, '-') << "\n";

      Function *Fprev = NULL;
      int i = 0;
      for(TraceProfile::iterator B = Trace.Blocks.begin(), BE = Trace.Blocks.end();
          B != BE; ++B) {
        BasicBlock  *BB = *B;
        Function    *F  = BB->getParent();
        double exitPercent = Trace.ExitPercents[i++];

        if(F != Fprev) {
          Fprev = F;
          outs() << "  @" << F->getName() << "\n";
        }
        outs() << "    " << BB->getName() << " "
               << format("%15.2f", (exitPercent * 100)) << "%\n";
      }
    }

    return false;
}


//----------------------------------------------------------------------------//
//===----------------------- Main Entry Point -----------------------------===//
//----------------------------------------------------------------------------//
int main(int argc, char **argv) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);

  LLVMContext &Context = getGlobalContext();
  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.
  
  cl::ParseCommandLineOptions(argc, argv, "llvm profile dump decoder\n");

  // Read in the bitcode file...
  std::string ErrorMessage;
  OwningPtr<MemoryBuffer> Buffer;
  error_code ec;
  Module *M = 0;
  if (!(ec = MemoryBuffer::getFileOrSTDIN(BitcodeFile, Buffer))) {
    M = ParseBitcodeFile(Buffer.get(), Context, &ErrorMessage);
  } else
    ErrorMessage = ec.message();
  if (M == 0) {
    errs() << argv[0] << ": " << BitcodeFile << ": "
      << ErrorMessage << "\n";
    return 1;
  }

  // Initialize passes
  PassRegistry &Registry = *PassRegistry::getPassRegistry();
  initializeAnalysis(Registry);

  // Run the printer pass.
  PassManager PassMgr;

  switch(ProfileType) {
  case EdgeProfileType:{
    // Read the profiling information. This is redundant since we load it again
    // using the standard profile info provider pass, but for now this gives us
    // access to additional information not exposed via the ProfileInfo
    // interface.
    ProfileInfoLoader *PIL = new ProfileInfoLoader(argv[0], ProfileDataFile, *M);
    PassMgr.add(createProfileLoaderPass(ProfileDataFile));
    PassMgr.add(new ProfileInfoPrinterPass(*PIL));
    break;
  }
  case IFCProfileType:
    PassMgr.add(createIndirectFunctionCallProfileLoaderPass());
    PassMgr.add(new IFCProfileInfoPrinterPass());
    break;
  case TraceProfileType:
    PassMgr.add(createTraceProfileLoaderPass());
    PassMgr.add(new TraceProfileInfoPrinterPass());
    break;
  default:
    errs() << "Unknown profile type: " << ProfileType;
  }

  PassMgr.run(*M);

  return 0;
}
