//===- IndirectFunctionCallProfileInfo.h ----------------------*- C++ -*---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file outlines the interface used by optimizers to load indirect function
// call profiles.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_INDIRECT_FUNCTION_CALL_PROFILE_INFO_H
#define LLVM_INDIRECT_FUNCTION_CALL_PROFILE_INFO_H

#include <vector>
#include <map>

namespace llvm {

class CallInst;
class Function;

/// A profiling record that stores the target function and the
/// percent of the calls that went to the target.
struct IFCTarget {
  Function *Target;
  double   Percent;
};

/// CallSiteProfile should store targets in order from most to least called
typedef std::vector<IFCTarget> CallSiteProfile;

/// Holds profiling info obtained by indirect function call profiling
class IndirectFunctionCallProfileInfo {
 public:
  //========================== Members ===============================//
  static char ID; // Class identification, replacement for typeinfo
  static const CallSiteProfile NoProfileInfo;

  //======================== Constructors ============================//
  IndirectFunctionCallProfileInfo();
  virtual ~IndirectFunctionCallProfileInfo(); // We may be subclassed

  //========================== Methods ===============================//
  /// Returns the CallSiteInfo for the given call site.
  virtual const CallSiteProfile& getCallSiteProfile(CallInst *) const;

  /// Returns true if there is profiling info for this call site
  virtual bool hasProfileInfo(CallInst *call) const;

  /// Returns the most frequently taken target for this call site
  virtual Function* getMostFrequentTarget(CallInst *call) const;

 protected:
  typedef std::map<CallInst *, CallSiteProfile> ProfileMap;
  ProfileMap CallProfileMap;
};

}
#endif
