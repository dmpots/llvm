//===- TraceBuilder.cpp - Code to perform simple function inlining --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements a trace building pass. The pass reads trace profile data
// and uses it to construct likely traces in the program.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "build-traces"

#include "llvm/Attributes.h"
#include "llvm/BasicBlock.h"
#include "llvm/GlobalValue.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Profile/ProfilingSupport.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/TraceProfileInfo.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/Cloning.h"

using namespace llvm;

STATISTIC(sNumTracesBuilt, "The # of traces built");
STATISTIC(sNumIndirectChecks, "The # of inserted checks for indirect trace targets");

namespace {
  class TraceBuilder : public ModulePass {
  public:
    static char ID; // Pass identification, replacement for typeid
    TraceBuilder() : ModulePass(ID) {}

    bool runOnModule(Module &M);
    void getAnalysisUsage(AnalysisUsage &AU) const;
  };

  struct CallFixupRecord {
    CallInst *Call;
    Function *OrigTarget;
    Function *TraceTarget;
    bool     IsLoop;
    
    CallFixupRecord(CallInst *C, Function *OT, Function* TT, bool Loop) :
      Call(C), OrigTarget(OT), TraceTarget(TT), IsLoop(Loop)
    {}
  };

  typedef llvm::DenseMap<BasicBlock*, BasicBlock*> BlockMapping;
  typedef llvm::SmallVector<TraceProfile::iterator, 16> BlockCloneList;
  typedef llvm::SmallVector<CallFixupRecord, 64> CallFixupList;
  typedef llvm::SmallVector<Function *, 64> FunctionList;
}

//==============================================================================
// Pass Initialization
//
char TraceBuilder::ID = 0;
INITIALIZE_PASS_BEGIN(TraceBuilder, "build-traces",
                "Build program traces from profiling data", false, false)
INITIALIZE_AG_DEPENDENCY(TraceProfileInfo)
INITIALIZE_PASS_END(TraceBuilder, "build-traces",
                "Build program traces from profiling data", false, false)

ModulePass *llvm::createTraceBuilderPass() { return new TraceBuilder(); }

static cl::opt<bool>
InsertTraceProfiling("profile-traces", cl::init(false),
  cl::desc("Add instrumentation to traces built with the build-traces pass"),
  cl::Hidden);

static cl::opt<double>
TraceCompletionCutoff("trace-completion-cutoff", cl::init(0.0),
  cl::desc("Don't build traces with completion rates less than this"),
  cl::Hidden);


//==============================================================================
// Trace Instrumentation
//
static Constant *getPrintfFun(Module *M) {
  static Constant* Printf = NULL;
  if(Printf == NULL) {
    LLVMContext &Context = M->getContext();
    PointerType *Pchar = PointerType::get(IntegerType::get(Context, 8), 0);
    Type *PrintfArgTys = {Pchar};
    FunctionType *PrintfTy =
      FunctionType::get(IntegerType::get(Context, 32), PrintfArgTys, true);
    Printf = M->getOrInsertFunction("printf", PrintfTy);
  }
  return Printf;
}

static void insertPrintfCall(Module *M, BasicBlock *BB, const Twine& msg) {
  IRBuilder<> Builder(BB->getFirstInsertionPt());
  Constant *Fprintf = getPrintfFun(M);
  Value *Str  = Builder.CreateGlobalStringPtr(msg.str());
  Builder.CreateCall(Fprintf, Str);
}

static void addTraceInstrumentation(Module *M,
                                    TraceProfile& TraceProfile,
                                    FunctionList& FunctionClones) {
  BasicBlock &BBEnter  = FunctionClones.front()->getEntryBlock();
  BasicBlock &BBFinish = FunctionClones.back()->getEntryBlock();

  insertPrintfCall(M, &BBEnter,
                   Twine("Entered  Trace ") + Twine(TraceProfile.TraceNum) + "\n");
  insertPrintfCall(M, &BBFinish,
                   Twine("Finished Trace ") + Twine(TraceProfile.TraceNum) + "\n");
}

//==============================================================================
// Pass Implementation
//

static void mapBlocks(Function *F, Function *F_trace,
                      BlockMapping *Map, bool Clear=true) {
  if(Clear) Map->clear();

  Function::iterator
    B1 = F->begin(),
    B2 = F_trace->begin(),
    E1 = F->end(),
    E2 = F_trace->end(); 
  for(; B1 != E1 && B2 != E2; ++B1, ++B2) {
    (*Map)[B1] = B2;
  }
  
  assert((B1 == E1) && (B2 == E2) && "Cloned functions differ in size");
}

static void findClonePoints(TraceProfile& Trace, BlockCloneList *Worklist) {
  TraceProfile::iterator Prev_B = Trace.begin();
  Function   *Prev_F = (*Prev_B)->getParent();

  for(TraceProfile::iterator I = Trace.begin(), E = Trace.end(); I != E; ++I) {
    BasicBlock *BB = *I;
    if(BB->getParent() != Prev_F) {
      Worklist->push_back(Prev_B);
      Prev_F = BB->getParent();
    }
    Prev_B = I;
  }
}

static Function *cloneTraceFunction(Module *M, Function *F_clone,
                                    int TraceNum, int FunNum,
                                    bool Inline = true) {
  Function *F_trace = CloneFunction(F_clone);
  F_trace->setName(Twine(F_clone->getName()) + "_trace_" +
                   Twine(TraceNum) + "_" + Twine(FunNum));
  if(Inline) F_trace->addFnAttr(Attribute::AlwaysInline);
  F_trace->setLinkage(GlobalValue::InternalLinkage);
  M->getFunctionList().insert(M->getFunctionList().end(), F_trace);
  
  DEBUG(dbgs() << "Cloning function " << F_clone->getName()
        << " as " << F_trace->getName() << "\n");
  return F_trace;
}

static CallInst* findCallToClone(BasicBlock* BB) {
  // Find the call instruction in this block which we will change to call the
  // cloned trace function. The call must exist and for now we require that
  // there is only one call in the block or that the other calls are intrinsic
  // calls.
  CallInst *Call = NULL;
  for(BasicBlock::iterator I = BB->begin(), E = BB->end(); I!=E; ++I) {
    if(CallInst *C = dyn_cast<CallInst>(&(*I))) {
      assert((Call == NULL || prof::isIntrinsicCall(*Call))
             && "Can't clone a trace block with multiple calls");
      Call = C;
    }
  }

  assert(Call && "Should have a call in a trace clone point");
  return Call;
}

static void cloneTraceFunctions(Module *M, TraceProfile& Trace, 
                                /*out*/CallFixupList *CallFixups,
                                /*out*/FunctionList  *FunctionClones) {
  // Find list of blocks containing calls that need to be cloned
  BlockCloneList ClonePoints;
  findClonePoints(Trace, &ClonePoints);
  DEBUG(dbgs() << "Trace has " << ClonePoints.size() << " clone points\n");
  
  // Clone the root of the trace
  int i = 0;
  Function *Root = Trace.Blocks[0]->getParent();
  Function *MappedFunction = Root;
  Function *TraceRoot = cloneTraceFunction(M, Root, Trace.TraceNum, i++,
                                        // vvvvv don't mark it as always inline
                                           false);
  FunctionClones->push_back(TraceRoot);
  
  // Compute initial block mapping
  BlockMapping BlockMap;
  mapBlocks(Root, TraceRoot, &BlockMap);

  // Work the list On this pass we clone all the necessary functions and record
  // all of the call instructions that need to be changed to keep us on the
  // trace.
  for(BlockCloneList::iterator I = ClonePoints.begin(), E = ClonePoints.end();
      I != E; ++I) {
    BasicBlock *BBProfile = **I;
    BasicBlock *BBProfileTarget = *((*I) + 1);
    Function   *FProfileTarget = BBProfileTarget->getParent();
    BasicBlock *BBTrace = BlockMap[BBProfile];
    const bool EndOfTrace = (I+1) == E;
    const bool IsLoop     = FProfileTarget == Root;

    DEBUG(dbgs() << "Clone point " << i << " " << BBProfile->getName() << " "
          << " in function " << BBProfile->getParent()->getName()
          << " to function " << FProfileTarget->getName() << "\n");
    assert((MappedFunction == BBProfile->getParent()) 
           && "BlockMap is out of date");
    assert((BBProfile->getParent() != BBProfileTarget->getParent())
           && "Not a clone point");

    // Determine the target of the call. If the trace is a loop then have the
    // call go back to the trace head function. Otherwise go clone a new version
    // of the targted function and use that as the trace target.
    Function *FTraceTarget = NULL;
    if(IsLoop) {
      assert(EndOfTrace && "Unexpected trace loop before end of trace");
      FTraceTarget = TraceRoot;
    }
    else {
      FTraceTarget =
        cloneTraceFunction(M, FProfileTarget, Trace.TraceNum, i++);
      FunctionClones->push_back(FTraceTarget);
    }

    // Find the callinst we need to modify to stay on the trace. The callinst
    // should be the one on the trace we are building not the original one from
    // the profile so we use the blockmap to find the basic block on the trace
    // that contains the call.
    CallInst *CallInst = findCallToClone(BBTrace);

    // Add the callinst to the worklist
    CallFixups->push_back(CallFixupRecord(CallInst, FProfileTarget,
                                          FTraceTarget, IsLoop));
    
    // Map the newly cloned function so that the next clone point will find the
    // correct block to search for the callinst
    if(!EndOfTrace) {
      mapBlocks(FProfileTarget, FTraceTarget, &BlockMap);
      MappedFunction = FProfileTarget;
    }
  }
}

static void promoteIndirectCall(CallInst *call,
                                Function *checkTarget,
                                Function *directTarget) {
  sNumIndirectChecks++;

  BasicBlock *BB = call->getParent();
  Function *F = BB->getParent();
  LLVMContext &C = F->getContext();
  Value    *indirectTarget = call->getCalledValue();

  // Create new block for direct call
  BasicBlock *bbDirect = BasicBlock::Create(C, "trace.continue", F);
  CallInst *directCall = cast<CallInst>(call->clone());
  directCall->setCalledFunction(directTarget);
  bbDirect->getInstList().push_back(directCall);
  bbDirect->getInstList().push_back(BB->getTerminator()->clone());
  
  // Create default block for falling back to indirect call
  BasicBlock *bbFallback = BasicBlock::Create(C, "trace.fail", F);
  bbFallback->getInstList().push_back(call->clone());
  bbFallback->getInstList().push_back(BB->getTerminator()->clone());

  // Create cmp instruction for potential targets
  ICmpInst *cmp = new ICmpInst(call, CmpInst::ICMP_EQ, 
                               indirectTarget, checkTarget, "trace.hit");
  
  // Create br instruction to jump to correct landing pad
  BranchInst::Create(bbDirect, bbFallback, cmp, call);

  // Erase old call and terminator instructions
  call->eraseFromParent();
  BB->getTerminator()->eraseFromParent();
}

static void fixupInternalTraceCalls(CallFixupList& CallFixups) {
  for(CallFixupList::iterator I = CallFixups.begin(), E = CallFixups.end(); I!=E; ++I) {
    CallInst *Call = I->Call;
    DEBUG(dbgs()
          << "Fixing up call in " << Call->getParent()->getName()
          << "@" << Call->getParent()->getParent()->getName()
          << " to function " << I->TraceTarget->getName() << "\n");

    if(prof::isIndirectCall(*Call)) {
      // For indirect calls we need to insert a guard to ensure that the
      // function we are calling is the same as when we built the
      // trace. Normally this check is to see that the indirect call is for the
      // original profiled function target and if so we insert a call to the
      // trace target. For looping calls we want to insert a call back to the
      // trace function because we are going to be replacing uses of the
      // original profiled function with the trace function.
      Function *CheckTarget = I->IsLoop ? I->TraceTarget : I->OrigTarget;
      promoteIndirectCall(Call, CheckTarget, I->TraceTarget);
    }
    else {
      Call->setCalledFunction(I->TraceTarget);
    }
  }
}

static void replaceTraceRootUses(Function *OrigF, Function *TraceRoot) {
  DEBUG(dbgs() << "Replacing " << OrigF->getNumUses() << " uses of trace root"
        << OrigF->getName() << "\n");
  OrigF->replaceAllUsesWith(TraceRoot);
}

static void buildTrace(Module *M, TraceProfile& Trace) {
  sNumTracesBuilt++;
  CallFixupList CallFixups;
  FunctionList  FunctionClones;

  cloneTraceFunctions(M, Trace, &CallFixups, &FunctionClones);
  fixupInternalTraceCalls(CallFixups);
  replaceTraceRootUses(Trace.Blocks[0]->getParent(), FunctionClones[0]);

  if(InsertTraceProfiling)
    addTraceInstrumentation(M, Trace, FunctionClones);
}

static bool shouldInstantiateTrace(const TraceProfile& Trace) {
  bool ok = true;
  if(Trace.ExitPercents.back() < TraceCompletionCutoff) {
    ok = false;
  }

  return ok;
}

//==============================================================================
// External Interface
//
void TraceBuilder::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<TraceProfileInfo>();
}

bool TraceBuilder::runOnModule(Module &M) {
  TraceProfileInfo &TracePI = getAnalysis<TraceProfileInfo>();

  TraceProfileList &Traces = TracePI.getTraces();
  for(TraceProfileList::iterator T = Traces.begin(), E = Traces.end();T!=E;++T) {
    if(shouldInstantiateTrace(*T)) {
      buildTrace(&M, *T);
    }
  }

  return sNumTracesBuilt > 0;
}


