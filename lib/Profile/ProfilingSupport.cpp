//===- ProfilingSupport.cpp -----------------------------------*- C++ -*---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "llvm/Profile/ProfilingSupport.h"
#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"

using namespace llvm;

////////////////////////////////////////////////////////////////////////////////
// Indirect call predicate
//
bool prof::isIndirectCall(const CallInst& call) {
  // An indirect call will return NULL for the Function
  if(call.getCalledFunction()) return false;


  // Seems that intrinsic fucntions also return null for getCalledFunction
  // try to filter them out by checking that we are not calling a global value.
  if(isa<GlobalValue>(call.getCalledValue())) return false;

  return true;
}

////////////////////////////////////////////////////////////////////////////////
// Compute the correct function and callsite numbers by walking the module
//
void prof::computeFunctionAndCallSiteNumbers(Module& M,
                                             FunctionNumbering *Functions,
                                             CallSiteNumbering *Calls) {
  // FunctionNumber and CallSiteNumber 0 is used to indicate an invalid value
  assert((Functions->size() == 0) && (Calls->size() == 0) &&
         "Functions and Calls should start out empty");
  Functions->push_back(0);
  Calls->push_back(0);

  // Examine every function and callsite in the program
  for(Module::iterator F = M.begin(), FE = M.end(); F!=FE; ++F) {
    // Map this function to the current index at the end of the vector
    Functions->push_back(F);
    for(Function::iterator B = F->begin(), BE = F->end(); B!=BE; ++B){
      for(BasicBlock::iterator I = B->begin(), IE = B->end(); I!=IE; ++I) {
        CallInst *C = dyn_cast<CallInst>(I);
        if(!C || !isIndirectCall(*C))
          continue;

        // Map this callsite to the current index at the end of the vector
        Calls->push_back(C);
      }
    }
  }
}

//==============================================================================
// Function Number Annotation
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

////////////////////////////////////////////////////////////////////////////////
/// Add a call to llvm.annotation.i32(fn, ".ibprofile.annotation", NULL, 0)
/// that we can read later to get the function number for this function.
///
void
prof::addFunctionNumberAnnotation(Function *F, FunctionNumber FN) {
  assert(F && "Function should not be null");
  if(F->isDeclaration()) return;

  // Add annotation to initial block
  Module *M = F->getParent();
  const BasicBlock::iterator I = F->getEntryBlock().getFirstInsertionPt();

  // Get function number as an i8* to pass to the annotation
  ConstantInt *FunctionNumberConst =
    ConstantInt::get(M->getContext(), APInt(32, FN));

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
}
