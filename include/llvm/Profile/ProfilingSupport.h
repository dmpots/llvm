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
  class BasicBlock;
  class Function;
  class Module;

  namespace prof {
    // -----------------------------     Types    -----------------------------//
    typedef uint32_t FunctionNumber;
    typedef uint32_t CallSiteNumber;
    typedef uint32_t BasicBlockNumber;
    typedef uint64_t BigCounter;
    typedef std::vector<Function *>  FunctionNumbering;
    typedef std::vector<CallInst *>  CallSiteNumbering;
    typedef std::vector<BasicBlock*> BasicBlockNumbering;

    // -----------------------------  Functions  -----------------------------//
    bool isIndirectCall(const CallInst& call);
    bool isIntrinsicCall(const CallInst& call);
    void computeFunctionNumbering(Module& M, FunctionNumbering *Functions);
    void computeCallSiteNumbering(Module& M, CallSiteNumbering *Calls);
    void computeBasicBlockNumbering(Module& M, BasicBlockNumbering *Blocks);

    /// Compute all numberings in one walk over the module. Any combination of
    /// the numberings can also be computed by passing NULL for the numberings that
    /// should be skipped.
    void computeNumbering(Module& M,
                          FunctionNumbering   *Functions,
                          BasicBlockNumbering *Blocks,
                          CallSiteNumbering   *Calls);

    /// Insert an llvm.annotation to record the function number of the function
    void addFunctionNumberAnnotation(Function *F, FunctionNumber FN);

    /// Convert a big conter to a double but check for overflow. If it overflows
    /// set it to be the max double value.
    double convertToDoubleWithOverflowCheck(prof::BigCounter Counter);

    // -----------------------------     Data    -----------------------------//
    static const FunctionNumber UnknownFunction = 0;


    // ------------------------- Output file formats -------------------------//
    // Indirect Function Call Record structure that is written to the output file
    struct IFCProfileRecord {
      CallSiteNumber CallSite;
      FunctionNumber Target;
      BigCounter     Count;
    };

    // Trace profiling external format (binary data file)
    //
    // =======================================
    // ProfilingInfoType (TraceInfo)
    // -- Header --
    // TraceSize (NumRecords = N)
    // NumHeaderHits (HotnessCounter[Header])
    // -- Header --
    // TraceProfileRecord1
    // TraceProfileRecord2
    // ...
    // TraceProfileRecordN
    // =======================================
    enum TraceProfileRecordType {
      TraceGapRecord,
      TraceBlockRecord,
      TraceHeaderBlockRecord
    };

    struct TraceProfileHeader {
      int TraceSize;
      BigCounter NumHits;
    };

    struct TraceProfileRecord {
      TraceProfileRecordType Tag;
      union {
        BasicBlockNumber BlockNumber;
        FunctionNumber   FunctionNumber;
      };
    };
  }
}
#endif
