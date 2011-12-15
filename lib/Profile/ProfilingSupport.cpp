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
