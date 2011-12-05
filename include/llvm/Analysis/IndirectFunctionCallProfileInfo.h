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
#include <functional>

namespace llvm {

class CallInst;
class Function;

/// A profiling record that stores the target function and the
/// percent of the calls that went to the target.
struct IFCTarget {
  IFCTarget(Function *F, double P = 0.0) : Target(F), Percent(P) {}
  Function *Target;
  double   Percent;
};

/// CallSiteProfile should store targets in order from most to least called
typedef std::vector<IFCTarget> CallSiteProfile;

/// Holds profiling info obtained by indirect function call profiling
class IndirectFunctionCallProfileInfo {
 public:
  //========================== Types =================================//
  typedef std::map<const CallInst *, CallSiteProfile> ProfileMap;

  //========================== Members ===============================//
  static char ID; // Class identification, replacement for typeinfo
  static const CallSiteProfile NoProfileInfo;

  //======================== Constructors ============================//
  IndirectFunctionCallProfileInfo();
  virtual ~IndirectFunctionCallProfileInfo(); // We may be subclassed

  //========================== Methods ===============================//
  /// Returns the CallSiteInfo for the given call site.
  virtual const CallSiteProfile& getCallSiteProfile(const CallInst *) const;

  /// Returns true if there is profiling info for this call site
  virtual bool hasProfileInfo(const CallInst *call) const;

  /// Returns the most frequently taken target for this call site
  virtual Function* getMostFrequentTarget(const CallInst *call) const;

  /// Returns a const reference to the underlying profile map. Useful
  /// if you want to perform some action (such as iterating over all
  /// call sites) not provided by the function-based convenience interface.
  virtual const ProfileMap& getProfileMap() const;

  // Predicate for testing equality of IFCTarget records by Function
  // useful for using with the find_if stl algorithm.
  class FunTarget_eq : public std::unary_function<Function*, bool> {
    Function* _fn;
  public:
    explicit FunTarget_eq(Function* fn);
    bool operator() (const IFCTarget& t);
  };

 protected:
  ProfileMap CallProfileMap;
};

}
#endif
