//===-- TraceRuntime.cpp - Support library for collecting runtime traces -----//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the call back routines for the trace instrumentation
// pass. This should be used with the -insert-trace-profiling LLVM pass.
//
//===----------------------------------------------------------------------===//

#include "TraceFileManagement.h"
#include "llvm/Analysis/ProfileInfoTypes.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Profile/ProfilingSupport.h"
#if !defined(_MSC_VER) && !defined(__MINGW32__)
#include <unistd.h>
#else
#include <io.h>
#endif
#include <iostream>
#include <map>
#include <set>
#include <cassert>

#define dbgs()   std::cout
#define DEBUG(s)

using namespace llvm;
using namespace llvm::prof;

namespace {
  typedef void* Address;
  typedef std::map<Address, FunctionNumber> AddrMap;

  // Internal Trace Interface
  enum RuntimeTraceRecordType {
    RuntimeTraceAddressBreak,
    RuntimeTraceFunctionBreak,
    RuntimeTraceBlock,
    RuntimeTraceHeader
  };

  struct RuntimeTraceRecord {
    RuntimeTraceRecord(Address addr)
      : Tag(RuntimeTraceAddressBreak), AddressBreak(addr) {}

    RuntimeTraceRecord(RuntimeTraceRecordType tag, BasicBlockNumber n)
      : Tag(tag), BlockNumber(n) {}

    RuntimeTraceRecordType Tag;
    union {
      BasicBlockNumber BlockNumber;
      FunctionNumber   FunctionBreak;
      Address          AddressBreak;
    };
  };

  class Trace {
  public:
    //-------------------- Types ---------------------------//
    typedef std::vector<RuntimeTraceRecord> BlockVec;
    typedef BlockVec::size_type size_type;
    typedef BlockVec::value_type value_type;

    typedef std::vector<TraceProfileRecord> ProfileVec;

    //------------------ Vector Interface -------------------//
    void     push_back(const RuntimeTraceRecord& b) {Blocks.push_back(b);}
    size_type size() {return Blocks.size();}
    RuntimeTraceRecord& operator[] ( size_type n ) {return Blocks[n];}

    //------------------ Trace Interface --------------------//
    void convertToExternalFormat(const AddrMap& FunAddrMap);
    void addExternalRecord(TraceProfileRecordType tag, BasicBlockNumber n);
    const ProfileVec& getProfileRecords() const {return ProfileRecords;}

  private:
    BlockVec Blocks;
    ProfileVec ProfileRecords;
  };

  class Tracer {
  public:
    Tracer() :
      State(Profiling), CurrentTrace(NULL),
      HotnessThreshold(10), TraceLengthLimit(100), EndTraceOnBreak(false) {}

    //------------------ Public Interface -------------------//
    void writeDataToFile(int fd);
    void traceHead(BasicBlockNumber);
    void tracePath(BasicBlockNumber);
    void traceBreak(FunctionNumber);
    void checkTraceBreak(Address);
    void addFunctionMapping(FunctionNumber FN, Address Addr);

  private:
    //-------------------- Types ---------------------------//
    enum TraceState {
      Profiling, Tracing
    };
    typedef std::set<BasicBlockNumber> BBSet;
    typedef BigCounter HotnessCounter;
    typedef std::map<BasicBlockNumber, HotnessCounter> HeatMap;
    typedef std::vector<Trace*> TraceVec;
    //typedef std::vector<RuntimeTraceRecord> Trace;

    //------------------ Variables -------------------------//
    // Tracer state
    TraceState        State;
    BBSet TraceBlocks; // All header blocks that are included in any trace
    HeatMap HotnessCounters;
    Trace *CurrentTrace;
    TraceVec Traces;
    AddrMap FunctionAddressMap;

    // Trace building paramters
    HotnessCounter    HotnessThreshold;
    Trace::size_type  TraceLengthLimit;
    bool EndTraceOnBreak;

    //------------------- Methods --------------------------//
    void startNewTrace(BasicBlockNumber BBHead);
    void commitTrace();
    void extendTrace(RuntimeTraceRecordType Tag, BasicBlockNumber N);
    void extendTraceWithBreak(const RuntimeTraceRecord& TraceRecord);
    void breakTrace(FunctionNumber FN);
    void breakTraceOnAddress(Address addr);
  };
}


//==============================================================================
// Trace Implementation
//
void Trace::addExternalRecord(TraceProfileRecordType tag, BasicBlockNumber n) {
  TraceProfileRecord Rec;
  Rec.Tag = tag;
  Rec.BlockNumber = n;
  ProfileRecords.push_back(Rec);
}

void Trace::convertToExternalFormat(const AddrMap& FunAddrMap) {
  assert((ProfileRecords.size() == 0) && "Vector should be empty");
  for(BlockVec::iterator I = Blocks.begin(), E = Blocks.end(); I!=E; ++I) {
    switch(I->Tag) {
      // Check to see we can now resolve the address to a known function. If not
      // then keep the break in the trace.
    case RuntimeTraceAddressBreak:
      if(FunAddrMap.find(I->AddressBreak) == FunAddrMap.end()) {
        addExternalRecord(TraceGapRecord, UnknownFunction);
      }
      break;

    case RuntimeTraceFunctionBreak:
      addExternalRecord(TraceGapRecord, I->FunctionBreak);
      break;

    case RuntimeTraceBlock:
      addExternalRecord(TraceBlockRecord, I->BlockNumber);
      break;

    case RuntimeTraceHeader:
      addExternalRecord(TraceHeaderBlockRecord, I->BlockNumber);
      break;

    default:
      assert(false && "unexpected trace tag");
    }
  }

  // Get rid of extra space used by blocks
  Blocks.clear();
}

//==============================================================================
// Tracer Implementation
//

////////////////////////////////////////////////////////////////////////////////
// Writes profiling data to output file
//
// The trace profiling data consists of a header followed by a number of trace
// profiling record entries.
//
// The header contains an identifier for the type of profiling info, the number
// of records in the trace, and the number of times that the trace header was
// executed.  along with the number of records.
//
// Header:
// <-- 4 bytes --> <-- 4 bytes --> <--------- 8 bytes ----------->
// +--------------+---------------+------------------------------+
// | IFCInfo      |  # of Records | Number of Header Executions  |
// +--------------+---------------+------------------------------+
//
// A trace record consists of a tag indicating what kind of record we have: a
// break in the trace to some external function, a basic block that has not been
// marked as a potential trace header, or a basic block that is marked as a
// potential trace header. The payload is either a function number for breaks in
// the trace or a basic block number for the normal trace entries.
//
// Record:
//  <-- 4 bytes --> <-- 4 bytes  -->
// +---------------+----------------+
// |   Tag         |   Payload      |
// +---------------+----------------+
//
void Tracer::writeDataToFile(int fd) {
  DEBUG(dbgs() << "Writing " << Traces.size() << " traces to disk\n");
  ProfilingType TraceProfileInfo = TraceInfo;

  // Loop over all the traces
  for(TraceVec::iterator T = Traces.begin(), E = Traces.end(); T != E; ++T) {
    const Trace::ProfileVec& Records = (*T)->getProfileRecords();

    // Write profiling info tag to indicate a trace comes next
    if(write(fd, &TraceProfileInfo, sizeof(TraceProfileInfo)) < 1) {
      std::cerr << "error: Unable to write trace profile tag\n";
    }

    // Write out the trace header
    TraceProfileHeader Header;
    Header.TraceSize = Records.size();
    Header.NumHits   = HotnessCounters[Records[0].BlockNumber];
    if(write(fd, &Header, sizeof(TraceProfileHeader)) < 0) {
      std::cerr << "error: Unable to write trace profile header.\n";
    }

    // Write out the trace records
    for(Trace::ProfileVec::const_iterator R = Records.begin(), E = Records.end();
        R != E; ++R) {

      // Copy out to preserve const qualifier
      TraceProfileRecord Record;
      Record.Tag = R->Tag;
      Record.BlockNumber = R->BlockNumber; // also works for function number

      if(write(fd, &Record, sizeof(TraceProfileRecord)) < 0) {
        std::cerr << "error: Unable to write trace profile record.\n";
      }
    }
  }

}

void Tracer::startNewTrace(BasicBlockNumber BBHead) {
  assert((State == Profiling) && "Should be profiling to start new trace");
  assert((CurrentTrace == NULL) && "Should have committed previous trace");
  DEBUG(dbgs() << "!\n!TRACE #" << Traces.size() << " @" << BBHead << "\n!\n");
  State = Tracing;

  CurrentTrace = new Trace;
  extendTrace(RuntimeTraceHeader, BBHead);
}

void Tracer::commitTrace() {
  DEBUG(dbgs() << "Commiting trace of length " << CurrentTrace->size() << "\n");
  CurrentTrace->convertToExternalFormat(FunctionAddressMap);
  Traces.push_back(CurrentTrace);

  State = Profiling;
  CurrentTrace = NULL;
}

void Tracer::extendTrace(RuntimeTraceRecordType Tag, BasicBlockNumber N) {
  assert((State == Tracing) && "Should be tracing to extend trace");
  DEBUG(dbgs() << "Extending Trace with block " << N << "\n");

  RuntimeTraceRecord TraceRecord(Tag, N);
  CurrentTrace->push_back(TraceRecord);

  // If this block could be a header for another trace then then insert it into
  // the set of blocks to rembember that it is already part of a trace.
  if(Tag == RuntimeTraceHeader) {
    TraceBlocks.insert(N);
  }

  // See if we need to end the trace
  Trace::size_type TraceSize = CurrentTrace->size();
  bool IsLoop = (TraceSize > 1) && ((*CurrentTrace)[0].BlockNumber == N);
  bool IsLong = TraceSize > TraceLengthLimit;
  if(IsLoop || IsLong) {
    DEBUG(dbgs() << "Ending trace "
          << "Loop ?= " << IsLoop << ", Long ?= " << IsLong << "\n");
    commitTrace();
  }
}

void Tracer::extendTraceWithBreak(const RuntimeTraceRecord& TraceRecord) {
  CurrentTrace->push_back(TraceRecord);

  if(EndTraceOnBreak || CurrentTrace->size() > TraceLengthLimit) {
    DEBUG(dbgs() << "Ending trace on break\n");
    commitTrace();
  }
}

void Tracer::breakTrace(FunctionNumber FN) {
  DEBUG(dbgs() << "Breaking trace with function " << FN << "\n");
  RuntimeTraceRecord TraceRecord(RuntimeTraceFunctionBreak, FN);
  extendTraceWithBreak(TraceRecord);
}

void Tracer::breakTraceOnAddress(Address Addr) {
  DEBUG(dbgs() << "Checking breaking on address " << Addr << "...");


  // To check if we need to break the trace we look to see if we have a
  // definition for the function that is being targeted by the indirect jump. If
  // we have recieved a callback from the JIT letting us know that this address
  // is a known function then we don't add a break record. Otherwise we add a
  // break record now and then check later to see if it is resolved when we dump
  // the trace to disk.
  if(FunctionAddressMap.find(Addr) == FunctionAddressMap.end()) {
    DEBUG(dbgs() << "MAYBE BREAK\n");
    RuntimeTraceRecord TraceRecord(Addr);
    extendTraceWithBreak(TraceRecord);
  }
  else {DEBUG(dbgs() << "NO BREAK\n");}
}

////////////////////////////////////////////////////////////////////////////////
//                          TRACER PUBLIC INTERFACE
//                       (callbacks from the C interface)
////////////////////////////////////////////////////////////////////////////////
void Tracer::traceHead(BasicBlockNumber BBHead) {
  // If we are already tracing then simple add a new record for this block
  if(State == Tracing) {
    extendTrace(RuntimeTraceHeader, BBHead);
    return;
  }
  // Update the hotness counter for this trace head. We always do this so that
  // we can include some basic hotness statistics with the trace.
  HotnessCounter Hits = ++(HotnessCounters[BBHead]);

  // Otherwise we might start a new trace. Check to see that the block meets all
  // the conditions for starting a new trace and if so have at it.

  // Check to see if this header block is included in another trace. If so then
  // we won't start a new trace from this block.
  if(TraceBlocks.find(BBHead) != TraceBlocks.end()) {
    return;
  }

  // Check to see if the trace hot enough to start tracing.
  if(Hits > HotnessThreshold) {
    startNewTrace(BBHead);
  }
}

void Tracer::tracePath(BasicBlockNumber BBPath) {
  if(State == Tracing) {
    extendTrace(RuntimeTraceBlock, BBPath);
  }
}

void Tracer::checkTraceBreak(Address Addr) {
  if(State == Tracing) {
    breakTraceOnAddress(Addr);
  }
}

void Tracer::traceBreak(FunctionNumber FN) {
  if(State == Tracing) {
    breakTrace(FN);
  }
}

void Tracer::addFunctionMapping(FunctionNumber FN, Address Addr) {
  FunctionAddressMap[Addr] = FN;
}

//==============================================================================
// External C interface
//
Tracer *tracer;

static void TracerExitHandler(void) {
  if(!tracer) return;

  int fd = getOutFile();
  tracer->writeDataToFile(fd);

  delete tracer;
}

extern "C"
void llvm_tracer_trace_head(BasicBlockNumber BBn) {
  tracer->traceHead(BBn);
}

extern "C"
void llvm_tracer_trace_path(BasicBlockNumber BBn) {
  tracer->tracePath(BBn);
}

extern "C"
void llvm_tracer_check_trace_break(Address Addr) {
  tracer->checkTraceBreak(Addr);
}

extern "C"
void llvm_tracer_trace_break(FunctionNumber FN) {
  tracer->traceBreak(FN);
}

extern "C"
void llvm_ifc_add_target_address(FunctionNumber FN, Address Addr) {
  tracer->addFunctionMapping(FN, Addr);
}

extern "C"
int llvm_start_trace_profiling_runtime(int argc,
                                       const char **argv,
                                       unsigned *_unused,
                                       unsigned _unused2) {
  int Ret = save_arguments(argc, argv);
  tracer = new Tracer();
  atexit(TracerExitHandler);
  return Ret;
}
