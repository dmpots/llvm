//===-- IBProfileJITEventListener.cpp - Help for indirect branch profiling ===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a JITEventListener object that helps profiling for indirect
// function call targets.
//
// Its primary job is to record the association between machine code addresses
// and LLVM Function objects. The listener waits for the jit to resolve a
// function stub to an actual function. When that happens we get a notification
// and issue a callback to the profiling runtime that associates the function
// stub to a function number. The function number is read from the Function
// object by looking for the annotation that was inserted with the
// -insert-ifc-profiling pass. That pass also inserts callbacks before each
// indirect function call which will pass the indirect function (which is
// actually a function stub) to the profiling runtime which can then use the
// info from the listener to match the function stub address to an actual
// function.
//
// To avoid having to link with the profiling runtime just to support this one
// listener we dynamically lookup the profiling runtime callback function at
// execution time. The user will have to load the profiling runtime using a
// -load option to the JIT. This restriction is not too bad since they will have
// to do that to access the other profiling functions in the instrumented
// program anyway.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ifcprofile-jit-event-listener"
#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/IndirectFunctionCallProfiling.h"
#include "llvm/Module.h"
#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
  typedef void (*IFCCallbackFun)(prof::FunctionNumber, void*);

  class IFCProfileJITEventListener : public JITEventListener {
  public:
    IFCProfileJITEventListener();
    ~IFCProfileJITEventListener();
    
    virtual void NotifyResolvedLazyStub(void *Stub,
                                        const Function& F,
                                        void *FunctionCode);
    bool hasProfilingCallback();

  private:
    IFCCallbackFun llvm_ifc_add_target_address;
    bool attemptedLookup; // only lookup the callback function once
  };
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
static prof::FunctionNumber getFunctionNumber(const Function &F) {
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

////////////////////////////////////////////////////////////////////////////////
// Attempt to locate the profiling callback function dynamically
//
static IFCCallbackFun dynamicLookupCallbackFun() {
  void *callback =
    sys::DynamicLibrary::SearchForAddressOfSymbol("llvm_ifc_add_target_address");

  if(callback == NULL) return NULL;

  // We have to do this double cast because there is no other way in
  // C++ to cast a void* to a function pointer. Presumably a function
  // pointer may not be the same size as a data pointer, but we ignore
  // that difficulty.
  return reinterpret_cast<IFCCallbackFun>(reinterpret_cast<uintptr_t>(callback));
}

//==============================================================================
// Listener Implementation
//
IFCProfileJITEventListener::IFCProfileJITEventListener() :
  llvm_ifc_add_target_address(NULL), attemptedLookup(false){}

IFCProfileJITEventListener::~IFCProfileJITEventListener() {}

////////////////////////////////////////////////////////////////////////////////
// See if we can callback into the profiling runtime.
// The profiling function will be dynamically linked from the profiling runtime
// library.
//
bool IFCProfileJITEventListener::hasProfilingCallback() {
  if(!attemptedLookup) {
    attemptedLookup = true;
    llvm_ifc_add_target_address = dynamicLookupCallbackFun();
    if(llvm_ifc_add_target_address == NULL) {
      DEBUG(dbgs() << "Could not find llvm_ifc_add_target_address function\n"
            << "Did you run the JIT with the libprofile_rt loaded?\n");

    }
  }

  return (llvm_ifc_add_target_address != NULL);
}

////////////////////////////////////////////////////////////////////////////////
// Callback to the profiling runtime to record the mapping from
// function stub address to the function number.
//
void IFCProfileJITEventListener::NotifyResolvedLazyStub(void *Stub,
                                                        const Function& F,
                                                        void *FunctionCode) {
  if(!hasProfilingCallback()) return;

  prof::FunctionNumber Fn = getFunctionNumber(F);
  DEBUG(dbgs()
        << "NotifyResolvedLazyStub Callback: "
        << F.getName() << "@" << Stub << " = " << Fn << "\n");

  llvm_ifc_add_target_address(Fn, Stub);
}

namespace llvm {
JITEventListener *createIFCProfileJITEventListener() {
  return new IFCProfileJITEventListener();
}
}
