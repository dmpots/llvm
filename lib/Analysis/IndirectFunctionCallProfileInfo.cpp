//===- IndirectFunctionCallProfileInfo.cpp --------------------*- C++ -*---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interface used by optimizers to load indirect function
// call profiles profiles, and provides a loader pass which reads an indirect
// function call profile file. It also includes the default implementation for
// the analysis group which reads no input files and provides no profiling
// information.
//
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "ifc-profile-info"

#include "llvm/Analysis/IndirectFunctionCallProfileInfo.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/PassRegistry.h"
#include "llvm/Analysis/ProfileInfoTypes.h"
using namespace llvm;

//===----------------------------------------------------------------------===//
//  Initialize analysis group
//

// register PathLoader
INITIALIZE_ANALYSIS_GROUP(IndirectFunctionCallProfileInfo,
                          "Indirect Function Call Profile Information",
                          NoIndirectFunctionCallProfileInfo)

//===----------------------------------------------------------------------===//
//  IndirectFunctionCallProfileInfo implementation
//
char IndirectFunctionCallProfileInfo::ID = 0;
IndirectFunctionCallProfileInfo::IndirectFunctionCallProfileInfo()  {}
IndirectFunctionCallProfileInfo::~IndirectFunctionCallProfileInfo() {}
const CallSiteProfile IndirectFunctionCallProfileInfo::NoProfileInfo;

const CallSiteProfile&
IndirectFunctionCallProfileInfo::getCallSiteProfile(CallInst* call) const {
  ProfileMap::const_iterator P = CallProfileMap.find(call);
  if(P == CallProfileMap.end()) return NoProfileInfo;

  return P->second;
}

/// Return true if there is profiling info for this callsite
bool IndirectFunctionCallProfileInfo::hasProfileInfo(CallInst *call) const {
  return getCallSiteProfile(call).size() > 0;
}

/// Returns the most frequently taken target for this call site
Function*
IndirectFunctionCallProfileInfo::getMostFrequentTarget(CallInst *call) const {
  if(!hasProfileInfo(call))
    return NULL;
  return getCallSiteProfile(call).front().Target;
}


//===----------------------------------------------------------------------===//
//  NoProfile PathProfileInfo implementation
//

namespace {
  class NoIndirectFunctionCallProfileInfo
    : public ImmutablePass,
      public IndirectFunctionCallProfileInfo {
  public:
    static char ID; // Class identification, replacement for typeinfo
    NoIndirectFunctionCallProfileInfo() : ImmutablePass(ID) {}

    /// getAdjustedAnalysisPointer - This method is used when a pass implements
    /// an analysis interface through multiple inheritance.  If needed, it
    /// should override this to adjust the this pointer as needed for the
    /// specified pass info.
    virtual void *getAdjustedAnalysisPointer(AnalysisID PI) {
      if (PI == &IndirectFunctionCallProfileInfo::ID)
        return (IndirectFunctionCallProfileInfo*)this;
      return this;
    }

    virtual const char *getPassName() const {
      return "NoIndirectFunctionCallProfileInfo";
    }
  };
}  // End of anonymous namespace

char NoIndirectFunctionCallProfileInfo::ID = 0;
// Register the NoIndirectFunctionCallProfileInfo pass...
INITIALIZE_AG_PASS(NoIndirectFunctionCallProfileInfo,
                   IndirectFunctionCallProfileInfo,
                   "no-ifc-profile",
                   "No Indirect Function Call Profile Information",
                   false, // Is CFG Only
                   true,  // Is Analysis
                   true)  // Is default Analysis Group Implementation

// Link NoIndirectFunctionCallProfileInfo as a pass, and make it available as an
// optimisation
ImmutablePass *llvm::createNoIndirectFunctionCallProfileInfoPass() {
  return new NoIndirectFunctionCallProfileInfo;
}
