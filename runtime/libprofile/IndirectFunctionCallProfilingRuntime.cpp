/*===-- IndirectFunctionCallProfiling.cpp - Support library for ifc profiling *\
|*
|*                     The LLVM Compiler Infrastructure
|*
|* This file is distributed under the University of Illinois Open Source
|* License. See LICENSE.TXT for details.
|*
|*===----------------------------------------------------------------------===*|
|*
|* This file implements the call back routines for the indirect function call
|* profiling instrumentation pass. This should be used with the
|* -insert-ifc-profiling LLVM pass and the -use-ifcprofile-listener lli option.
|*
\*===----------------------------------------------------------------------===*/


#include "Profiling.h"
#include "llvm/IndirectFunctionCallProfiling.h"
#include "llvm/Support/DataTypes.h"
#include <iostream>
#include <map>

typedef void*    Address;
using namespace llvm;

namespace {
  class IFCProfiler {
  public:
    typedef uint64_t Counter;
    typedef std::pair<prof::CallSiteNumber, prof::FunctionNumber> ProfileEntry;
    typedef std::map<ProfileEntry, Counter> ProfileMap;
    typedef std::map<Address, prof::FunctionNumber> TargetMap;

    void incrementTarget(prof::CallSiteNumber cs, Address target);
    void addTargetAddress(prof::FunctionNumber fn, Address addr);
    inline ProfileMap &getCounts(){return Counts;}

  private:
    ProfileMap Counts;
    TargetMap Targets;
  };
}

static IFCProfiler profiler;
static bool Init = false;

//==============================================================================
// Profiler Implementation
//
void IFCProfiler::incrementTarget(prof::CallSiteNumber cs, Address addr) {
  TargetMap::iterator I = Targets.find(addr);
  prof::FunctionNumber target = 0;
  if(I != Targets.end())
    target = I->second;

  ProfileEntry entry = std::make_pair(cs, target);
  Counts[entry]++;
}

void IFCProfiler::addTargetAddress(prof::FunctionNumber fn, Address addr) {
  Targets[addr] = fn;
}


static void IFCExitHandler(void) {
  std::cerr << "Done!\n";
  for(IFCProfiler::ProfileMap::iterator
        I = profiler.getCounts().begin(),
        E = profiler.getCounts().end(); I != E; ++I) {
    std::cout << I->first.first  << " "
              << I->first.second << " "
              << I->second       << "\n";
  }
}

//==============================================================================
// External C interface
//
extern "C"
void llvm_increment_indirect_target_count(prof::CallSiteNumber cs, Address addr) {
  profiler.incrementTarget(cs, addr);
  if(!Init){Init=true;atexit(IFCExitHandler);}
}

extern "C"
void llvm_ifc_add_target_address(prof::FunctionNumber fn, Address addr) {
  profiler.addTargetAddress(fn, addr);
}

extern "C"
int llvm_start_indirect_function_call_profiling(int argc,
                                                const char **argv,
                                                unsigned *_unused,
                                                unsigned _unused2) {
  int Ret = save_arguments(argc, argv);
  atexit(IFCExitHandler);
  return Ret;
}
