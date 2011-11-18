//===-- IBProfileJITEventListener.cpp - Help for indirect branch profiling ===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a JITEventListener object that helps profiling
// for indirect function call targets
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ibprofile-jit-event-listener"
#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

namespace {
  class IFCProfileJITEventListener : public JITEventListener {
  public:
    IFCProfileJITEventListener();
    ~IFCProfileJITEventListener();
    
    virtual void NotifyFunctionEmitted(const Function &F,
                                       void *Code, size_t Size,
                                       const EmittedFunctionDetails &Details);
    virtual void NotifyFreeingMachineCode(void *OldPtr);
  };

  typedef uint32_t FunctionNumber;
}

//==============================================================================
// Helper Functions
//
static const char annotationKey[]     = "ifc.key";
static const char annotationFunName[] = "llvm.var.annotation";

////////////////////////////////////////////////////////////////////////////////
// Read the function number from an llvm annotation. If the annotation is not
// found then return 0. We have to read the function number from the annotation
// because we are running an instrumented version of the program that may have
// added some functions to the IR and we want to make sure we match up with the
// original function numbering.
//
// The annotation call we want looks like 
//
//    llvm.var.annotation(ptrtoint <fn>, getelemtptr <global string> 0 0, ...)
//
// To verify that we have the correct annotation we search through the function
// header looking for an annotation call that looks like the above. When we find
// one we check to see that the global string matches the expected annotation
// key, which is the "ifc.key" string value.
//
// Once we have the right annotation call we can simple read off the first
// paramater to the annotation call which is the function number (cast to an i8*
// to satisify the type system)
static FunctionNumber GetFunctionNumber(const Function &F) {
  const BasicBlock &B = F.getEntryBlock();
  for(BasicBlock::const_iterator I = B.begin(), E = B.end(); I != E; ++I) {
    const CallInst *C = dyn_cast<CallInst>(&*I);    
    if(!C)
      continue;
    if(C->getCalledFunction()->getName() != annotationFunName)
      continue;

    // Ok, this is an annotation call so check to see if it is a function number
    // annotation. We do this by checking the string argument which
    // should be the ifc.profiling.annotation global variable
    // Check for GEP that points to the constant annotation string
    Value *annotationPointer = C->getArgOperand(1);
    ConstantExpr *gep = dyn_cast<ConstantExpr>(annotationPointer);
    if(!gep || gep->getOpcode() != Instruction::GetElementPtr) {
      DEBUG(dbgs() << "Expected GEP inst as pointer to annotation string\n");
      continue;
    }

    // Check first op of gep is a global string variable
    GlobalVariable *annotationStringVar = 
      dyn_cast<GlobalVariable>(gep->getOperand(0));
    if(!annotationStringVar) {
      DEBUG(dbgs() << "Expected pointer to global\n");
      continue;
    }
    ConstantArray *annotationString = 
      dyn_cast<ConstantArray>(annotationStringVar->getInitializer());
    if(!annotationString || !annotationString->isCString()) {
      DEBUG(dbgs() << "Expected constant string\n");
      continue;
    }
    
    // Check that string == annotationKey
    if(annotationString->getAsCString() != annotationKey) {
      DEBUG(dbgs() << "Annotation name does not match expected value\n"
            << "  Got: "      << annotationString->getAsString() << "\n"
            << "  Expected: " << annotationKey                   << "\n");
      continue;
    }

    // Ok, this is the correct annotation call so now get the function number
    DEBUG(dbgs() << "Annotation found, extracting function number\n");
    ConstantExpr *annotationCast = dyn_cast<ConstantExpr>(C->getArgOperand(0));
    if(!annotationCast) {
      DEBUG(dbgs() << "Odd ifcprofile annotation: expected cast\n" 
            << *C->getArgOperand(0) << "\n");
      return 0;
    }
    
    ConstantInt *fn = dyn_cast<ConstantInt>(annotationCast->getOperand(0));
    if(!fn) {
      DEBUG(dbgs() << "Odd ifcprofile annotation: expected constant int\n");
      return 0;
    }
    
    return fn->getZExtValue();
  }
  return 0;
}

//==============================================================================
// Listener Implementation
//
IFCProfileJITEventListener::IFCProfileJITEventListener() {}
IFCProfileJITEventListener::~IFCProfileJITEventListener() {}

void IFCProfileJITEventListener::NotifyFunctionEmitted(
                                  const Function &F,
                                  void *Code, size_t Size,
                                  const EmittedFunctionDetails &Details) {
  DEBUG(dbgs() << "== Emitted Callback: " << F.getName() << " ==\n");
  FunctionNumber fn = GetFunctionNumber(F);
  DEBUG(dbgs() << "Function Number: " << fn << "\n");
  // TODO: add callback to the profiling runtime registering this function
}

void IFCProfileJITEventListener::NotifyFreeingMachineCode(void *OldPtr) {
  DEBUG(dbgs() << "Freeing Callback" << "\n");
}

namespace llvm {
JITEventListener *createIFCProfileJITEventListener() {
  return new IFCProfileJITEventListener();
}
}
