//===- IndirectFunctionCallProfiling.cpp - Instrument indirect function targets//
//
//                      The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass instruments the program with callbacks to track the targets of
// indirect branches. To keep track of which functions are involved we add
// annotations to each function that records the function number of that
// function. The function number is simply a sequential labeling of the
// functions in the module.
//
// The annotations added to the module look like:
//
//   @ifc.profiling.annotation = "ifc.key"
//   ...
//   fun() {
//     call llvm.var.annotation(<functionNumber>, @ifc.profiling.annotation, ...)
//   }
//
// With a few additional casts that are needed to satisfy the type system when
// calling the annotation intrinsic function.
//
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "insert-ifc-profiling"

#include "ProfilingUtils.h"
#include "llvm/Constants.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/ADT/Statistic.h"
#include <set>

using namespace llvm;

STATISTIC(NumAnnotationsInserted, "The # of annotations inserted.");

namespace {
  class IndirectFunctionCallProfiler : public ModulePass {
    bool runOnModule(Module &M);
  public:
    static char ID; // Pass identification, replacement for typeid
    IndirectFunctionCallProfiler() : ModulePass(ID), fn(0), cn(0) {
      initializeIndirectFunctionCallProfilerPass(*PassRegistry::getPassRegistry());
    }

    virtual const char *getPassName() const {
      return "Indirect Function Call Profiler";
    }

  private:
    // TODO we should share these definitions so other passes can see them
    typedef uint32_t FunctionNumber;
    typedef uint32_t CallSiteNumber;
    FunctionNumber fn; // Use pre-increment to keep 0 as an invalid function number
    CallSiteNumber cn; // Use pre-increment to keep 0 as an invalid callsite number

    void addFunctionNumberAnnotation(Function *F);
    void addIndirectCallInstrumentation(Function *F);
  };
}

//==============================================================================
// Module Initialization
//
char IndirectFunctionCallProfiler::ID = 0;
INITIALIZE_PASS(IndirectFunctionCallProfiler, "insert-ifc-profiling",
                "Insert instrumentation for indirect function call profiling",
                false, false)

//==============================================================================
// Helper Functions
//
static const char annotationVarName[] = "ifc.profiling.annotation";
static const char annotationKey[]     = "ifc.key";
static const char annotationFunName[] = "llvm.var.annotation";
static GlobalVariable* getAnnotationString(Module* M) {
  GlobalVariable *A = 
    M->getGlobalVariable(annotationVarName, true /* allow local */);

  if(!A) {
    //ArrayType *T = ArrayType::get(IntegerType::get(M->getContext(), 8), 
    //                              annotationVarName.size());
    Constant *S = ConstantArray::get(M->getContext(), annotationKey);
    A = new GlobalVariable(/*Module=*/*M, 
                           /*Type=*/S->getType(),
                           /*isConstant=*/true,
                           /*Linkage=*/GlobalValue::PrivateLinkage,
                           /*Initializer=*/S,
                           /*Name=*/annotationVarName);
    
  }

  return A;
}

static Function* getAnnotationFunction(Module *M) {
  Function* F = M->getFunction(annotationFunName);
  if (!F) {
    PointerType* PointerTy_i8 = 
      PointerType::get(IntegerType::get(M->getContext(), 8), 0);
    std::vector<Type*>F_args;
    //F_args.push_back(IntegerType::get(M->getContext(), 32));
    F_args.push_back(PointerTy_i8);
    F_args.push_back(PointerTy_i8);
    F_args.push_back(PointerTy_i8);
    F_args.push_back(IntegerType::get(M->getContext(), 32));
    FunctionType* FTy = 
      FunctionType::get(/*Result=*/Type::getVoidTy(M->getContext()),
                        /*Params=*/F_args,
                        /*isVarArg=*/false);

    F = Function::Create(/*Type=*/FTy,
                         /*Linkage=*/GlobalValue::ExternalLinkage,
                         /*Name=*/annotationFunName, M); // (external, no body)
    F->setCallingConv(CallingConv::C);
 }
  return F;
}

//==============================================================================
// Pass Implementation
//
bool IndirectFunctionCallProfiler::runOnModule(Module &M) {
  // Loop over the module, looking for function definitions We add an annotation
  // to each function definition that records the function number, which is just
  // a linear numbering of the functions in the module. This number is used at
  // runtime as an identifer for functions that are targeted by indirect
  // branches.
  //
  // In addition to the function number annotation, we add a callback to the
  // profiling runtime at each indirect call. The callback takes an identifer
  // for the call site and the address of the function being called.
  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    
    // Look at all the functions in the Module
    Function *F = dyn_cast<Function>(I);
    if(!F) continue;
    if(F->isDeclaration()) continue;

    addFunctionNumberAnnotation(F);
    addIndirectCallInstrumentation(F);
  }

  return true;
}


////////////////////////////////////////////////////////////////////////////////
/// Add a call to llvm.annotation.i32(fn, ".ibprofile.annotation", NULL, 0)
/// that we can read later to get the function number for this function.
///
void IndirectFunctionCallProfiler::addFunctionNumberAnnotation(Function *F) {
  // Add annotation to initial block
  Module *M = F->getParent();
  const BasicBlock::iterator I = F->getEntryBlock().getFirstInsertionPt();
  
  // Get function number as an i8* to pass to the annotation
  ConstantInt *FunctionNumberConst = 
    ConstantInt::get(M->getContext(), APInt(32, ++fn));
  PointerType* PointerTy_i8 = 
    PointerType::get(IntegerType::get(M->getContext(), 8), 0);
  Constant *FunctionNumber = 
    ConstantExpr::getIntToPtr(FunctionNumberConst, PointerTy_i8);

  // Get annotation function and string identifier
  Function    *AnnF = getAnnotationFunction(M);
  GlobalValue *AnnS = getAnnotationString(M);

  // Create call paramaters
  ConstantInt* constInt_0 = ConstantInt::get(M->getContext(), APInt(32, 0));
  std::vector<Constant*> annStrIndices;
  annStrIndices.push_back(constInt_0);
  annStrIndices.push_back(constInt_0);
  Constant* annStrPtr = ConstantExpr::getGetElementPtr(AnnS, annStrIndices);
  
  // Create call param vector
  std::vector<Value*> annParams;
  annParams.push_back(FunctionNumber); // the annotation value
  annParams.push_back(annStrPtr);      // identifier for the annotation
  annParams.push_back(annStrPtr);      // not used so just reuse string
  annParams.push_back(constInt_0);     // not used so just set to 0

  // Create and insert annotation call
  CallInst* ann = CallInst::Create(AnnF, annParams, "", I);
  ann->setCallingConv(CallingConv::C);
  ann->setTailCall(false);
  NumAnnotationsInserted++;
}

void IndirectFunctionCallProfiler::addIndirectCallInstrumentation(Function *F) {
  // TODO: Add instrumentation at each indirect call
}
