//===- TraceProfileInfo.h -------------------------------------*- C++ -*---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file outlines the interface used by optimizers to load trace profiles
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ANALYSIS_TRACE_PROFILE_INFO_H
#define LLVM_ANALYSIS_TRACE_PROFILE_INFO_H

#include <vector>

namespace llvm {
class BasicBlock;
class Function;

/// Profile data extracted from a trace profile data file
class TraceProfile {
 public:
  typedef std::vector<BasicBlock*> Trace;
  typedef Trace::iterator iterator;
  typedef Trace::const_iterator const_iterator;
  typedef std::vector<Function*> FunctionList;
  typedef std::vector<double> ExitPercentMap;

  double ExecutionPercent;
  Trace Blocks;
  FunctionList& getFunctions();
  int TraceNum;
  ExitPercentMap ExitPercents;

  // Iteration
  iterator begin() {return Blocks.begin();}
  iterator end()   {return Blocks.end();}

 private:
  FunctionList Functions;
};

/// List of profiles from a profile data file
typedef std::vector<TraceProfile> TraceProfileList;

/// Holds profiling info obtained by trace profiling
class TraceProfileInfo {
 public:
  //========================== Types =================================//

  //========================== Members ===============================//
  static char ID; // Class identification, replacement for typeinfo

  //========================== Methods ===============================//
  virtual TraceProfileList& getTraces();
  virtual int brokenTraceCount() const;

  //======================== Constructors ============================//
  TraceProfileInfo();
  virtual ~TraceProfileInfo(); // We may be subclassed

 protected:
  TraceProfileList TraceProfiles;
  int NumBrokenTraces;
};

}
#endif
