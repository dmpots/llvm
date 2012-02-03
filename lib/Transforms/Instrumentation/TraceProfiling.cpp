//===- TraceProfiling.cpp - Insert instrumentation for trace profiling -------//
//
//                      The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass instruments the program with callbacks to find and record hot
// traces in the target program. A trace is simple a sequence of basic
// blocks. There are two compilications. First, a trace may jump to a function
// for which we do not have a defintion (i.e we only have the declaration). When
// we encounter a call to these functions we must notify the trace runtime that
// we will encounter a break in the trace. The second issue is indirect function
// calls. These calls may be to known or unknown functions, but we won't know
// which until runtime. We reuse the mechansim for IFC profiling so that we can
// map addressess back to actual functions at runtime.
//
// We insert the following callbacks:
//
//  llvm_tracer_trace_head(BB) - at each trace head
//  llvm_tracer_trace_path(BB) - at each basic block
//  llvm_tracer_check_trace_break(Addr) - at each indirect function call
//  llvm_tracer_break_trace(FN) - at each direct call to external function
//
// We also insert function number annotations so that we can use the
// IFCJitListener to map function addresses to function numbers at runtime.
//
// The overall algorithm looks like this:
//
//    for each Function F
//        add function number annotation
//    for each BasicBlock B
//        if IS_TRACE_HEAD(B)
//            insert call to llvm_tracer_trace_head(BasicBlockNumber(B))
//        else
//            insert call to llvm_tracer_trace_path(BasicBlockNumber(B))
//    for each CallSite C
//       if C.isIndirect?
//           add call to llvm_tracer_check_trace_break(Address(C.target))
//       elsif C.target.isOnlyDeclaration?
//           add call to llvm_tracer_break_trace(FunctionNumber(C.target))
//
//
// [Note: Picking trace heads for Haskell] Currently we select trace heads based
// on some knowledge of what the generated Haskell code looks like. We only mark
// trace heads as those functions that have the _entry suffix in their
// name. This choice may be poor, but lets see how far we can get with it.
//
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "insert-trace-profiling"

#include "ProfilingUtils.h"
#include "llvm/Constants.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Profile/ProfilingSupport.h"
#include "llvm/ADT/Statistic.h"

#include <map>

using namespace llvm;

STATISTIC(NumTraceHeaders,       "The # of trace headers");

namespace {
  class TraceProfiler : public ModulePass {
    bool runOnModule(Module &M);
  public:
    static char ID; // Pass identification, replacement for typeid
    TraceProfiler() : ModulePass(ID) {};

    virtual const char *getPassName() const {
      return "Trace Profile Instrumenter";
    }

  private:
    typedef std::map<Function*, prof::FunctionNumber> FunctionMap;

    void instrumentBlock(BasicBlock *BB, prof::BasicBlockNumber BN);
    void addTraceHeaderInstrumentation(BasicBlock *BB, prof::BasicBlockNumber BN);
    void addTracePathInstrumentation(BasicBlock *BB, prof::BasicBlockNumber BN);
    void instrumentCall(CallInst *C, const FunctionMap &FMap);
    void addDirectCallInstrumentation(CallInst *C,const FunctionMap &FMap);
    void addIndirectCallInstrumentation(CallInst *C);

    Constant *Fun_llvm_tracer_trace_head;
    Constant *Fun_llvm_tracer_trace_path;
    Constant *Fun_llvm_tracer_check_trace_break;
    Constant *Fun_llvm_tracer_trace_break;
  };
}

//==============================================================================
// Pass Initialization
//
char TraceProfiler::ID = 0;
INITIALIZE_PASS(TraceProfiler, "insert-trace-profiling",
                "Insert instrumentation for trace profiling",
                false, false)

cl::opt<bool>
BreakTraceOnIntrinsic("break-trace-on-intrinsic", cl::init(true),
  cl::desc("A trace break is inserted for intrinsic calls"), cl::Hidden);


//==============================================================================
// Pass Implementation
//
static bool isEntryBlock(BasicBlock const& BB) {
  return (&(BB.getParent()->getEntryBlock())) == &BB;
}

static bool isTraceHeader(BasicBlock const& BB) {
  if(!isEntryBlock(BB)) return false;

  StringRef const& Name = BB.getParent()->getName();
  bool isValidHeader = (Name.endswith("_entry") &&
                        !Name.startswith("stg_") &&
                        !Name.startswith("base_"));

  return isValidHeader;
}

static Constant *getI32Callback(Module& M, const char* Name) {
  LLVMContext& Context = M.getContext();

  return
    M.getOrInsertFunction(Name,
                          Type::getVoidTy(Context),    // return type
                          Type::getInt32Ty(Context),   // first arg
                          NULL );
}

static Constant *getAddrCallback(Module& M, const char* Name) {
  LLVMContext& Context = M.getContext();

  return
    M.getOrInsertFunction(Name,
                          Type::getVoidTy(Context),    // return type
                          Type::getInt8PtrTy(Context), // function address
                          NULL );
}

static void insertI32Callback(Constant *Callback,
                              uint32_t    Val,
                              Instruction *Where) {

  // Prepare profiling call arguments
  LLVMContext& Context = Where->getParent()->getParent()->getContext();
  std::vector<Value*> args(1);
  args[0] = ConstantInt::get(Type::getInt32Ty(Context), Val);

  // Insert the profiling call right before the location given
  CallInst::Create(Callback, args, "", Where);
}

static void insertAddrCallback(Constant *Callback,
                               Value *Addr,
                               Instruction *Where) {
  assert(Addr && "Address should not be null");
  LLVMContext &Context = Where->getParent()->getParent()->getContext();

  // Prepare profiling call arguments
  std::vector<Value*> args(1);
  args[0] = CastInst::CreateTruncOrBitCast(Addr,
                                           Type::getInt8PtrTy(Context),
                                           "trace.check.target", Where);

  // Insert the profiling call right before the location given
  CallInst::Create(Callback, args, "", Where);
}

void TraceProfiler::addTraceHeaderInstrumentation(BasicBlock *BB,
                                                  prof::BasicBlockNumber BN) {
  insertI32Callback(Fun_llvm_tracer_trace_head,
                    BN, BB->getFirstInsertionPt());
  NumTraceHeaders++;
}

void TraceProfiler::addTracePathInstrumentation(BasicBlock *BB,
                                                prof::BasicBlockNumber BN) {
  insertI32Callback(Fun_llvm_tracer_trace_path,
                    BN, BB->getFirstInsertionPt());
}

void TraceProfiler::instrumentBlock(BasicBlock *BB, prof::BasicBlockNumber BN) {
  if(isTraceHeader(*BB)) {
    addTraceHeaderInstrumentation(BB, BN);
  }
  else {
    addTracePathInstrumentation(BB, BN);
  }
}

void TraceProfiler::addIndirectCallInstrumentation(CallInst *C) {
  insertAddrCallback(Fun_llvm_tracer_check_trace_break, C->getCalledValue(), C);
}

void TraceProfiler::addDirectCallInstrumentation(CallInst *C, const FunctionMap &FMap) {
  Function *F = C->getCalledFunction();
  assert(F && "Direct function calls should have a function attached");

  // If the function is only a declaration we will not be able to follow the
  // trace through the function call, so let the runtime know we are about to
  // have a break.
  if(F->isDeclaration()) {
    FunctionMap::const_iterator FMI = FMap.find(F);
    assert((FMI != FMap.end()) && "Declared function has no function number");
    prof::FunctionNumber FN = FMI->second;
    insertI32Callback(Fun_llvm_tracer_trace_break, FN, C);
  }
}

void TraceProfiler::instrumentCall(CallInst *C, const FunctionMap &FMap) {
  if(prof::isIntrinsicCall(*C) && !BreakTraceOnIntrinsic) return;

  if(prof::isIndirectCall(*C)) {
    addIndirectCallInstrumentation(C);
  }
  else {
    addDirectCallInstrumentation(C, FMap);
  }
}

//==============================================================================
// Pass Entry Point
//
bool TraceProfiler::runOnModule(Module &M) {
  // lookup the callback functions
  Fun_llvm_tracer_trace_head  = getI32Callback(M, "llvm_tracer_trace_head");
  Fun_llvm_tracer_trace_path  = getI32Callback(M, "llvm_tracer_trace_path");
  Fun_llvm_tracer_trace_break = getI32Callback(M, "llvm_tracer_trace_break");
  Fun_llvm_tracer_check_trace_break =
    getAddrCallback(M, "llvm_tracer_check_trace_break");

  // data file with the actual functions and callsites they represent.
  prof::FunctionNumbering   Functions;
  prof::BasicBlockNumbering Blocks;
  prof::CallSiteNumbering   Calls;
  prof::computeNumbering(M, &Functions, &Blocks, &Calls);

  // Add a function number annotation to each function
  FunctionMap FMap;
  for(prof::FunctionNumber FN = 1; FN < Functions.size(); ++FN) {
    Function *F = Functions[FN];
    prof::addFunctionNumberAnnotation(F, FN);
    FMap.insert(std::make_pair(F, FN));
  }

  // Add appropriate callbacks to each block
  for(prof::BasicBlockNumber BN = 1; BN < Blocks.size(); ++BN) {
    instrumentBlock(Blocks[BN], BN);
  }

  // Add appropriate callbacks to each call site
  for(prof::CallSiteNumber CN = 1; CN < Calls.size(); ++CN) {
    instrumentCall(Calls[CN], FMap);
  }

  // Insert initialization call if we have a main function
  Function *Main = M.getFunction("main");
  assert(Main && "Must have main function to initialize profiling runtime");
  if(Main) {
    InsertProfilingInitCall(Main, "llvm_start_trace_profiling_runtime",
                            NULL, /* No constant array needed */
                            NULL  /* No constant array type needed*/);
  }

  return true;
}
