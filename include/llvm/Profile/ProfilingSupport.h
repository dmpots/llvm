//===- ProfilingSupport.h - Support for profiling -------------------------===//
//
//                      The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This header contains types and function prototypes used for the indirect
// function call profiling framework. A common set of types reduces the chance
// of a mismatch when generating and reading profile data. These definitions are
// placed in a separate namespace to avoid polluting the general llvm namespace.
// ===----------------------------------------------------------------------===//
#ifndef LLVM_PROFILE_PROFILING_SUPPORT_H
#define LLVM_PROFILE_PROFILING_SUPPORT_H

#include "llvm/Support/DataTypes.h"
#include <vector>

namespace llvm {
  class Instruction;
  class CallInst;
  class Function;
  class Module;

  namespace prof {
    // Types
    typedef uint32_t FunctionNumber;
    typedef uint32_t CallSiteNumber;
    typedef uint32_t BasicBlockNumber;
    typedef uint64_t BigCounter;
    typedef std::vector<Function *> FunctionNumbering;
    typedef std::vector<CallInst *> CallSiteNumbering;

    // Record structure that is written to the output file
    struct IFCProfileRecord {
      CallSiteNumber CallSite;
      FunctionNumber Target;
      BigCounter     Count;
    };

    // Functions
    bool isIndirectCall(const CallInst& call);
    void computeFunctionAndCallSiteNumbers(Module& M,
                                           FunctionNumbering *Functions,
                                           CallSiteNumbering *Calls);
    // Data
    static const FunctionNumber UnknownFunction = 0;
  }
}
#endif
