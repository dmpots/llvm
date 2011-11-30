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


using namespace llvm;

namespace {
  typedef void*    Address;

  class IFCProfiler {
  public:
    typedef uint64_t Counter;
    typedef std::pair<prof::CallSiteNumber, Address> ProfileEntry;
    typedef std::map<ProfileEntry, Counter> ProfileMap;
    typedef std::map<Address, prof::FunctionNumber> TargetMap;

    void incrementTarget(prof::CallSiteNumber cs, Address target);
    void addTargetAddress(prof::FunctionNumber fn, Address addr);
    inline ProfileMap &getCounts()   {return Counts;}
    inline TargetMap  &getTargets()  {return Targets;}

  private:
    ProfileMap Counts;
    TargetMap Targets;
  };
}

static IFCProfiler *profiler;

//==============================================================================
// Debugging Helpers
//
static void dumpProfileMap(IFCProfiler& profiler) {
  for(IFCProfiler::ProfileMap::const_iterator
        I = profiler.getCounts().begin(),
        E = profiler.getCounts().end(); I != E; ++I) {

    prof::FunctionNumber target = 0;
    IFCProfiler::TargetMap::const_iterator T =
      profiler.getTargets().find(I->first.second);
    if(T != profiler.getTargets().end()) {
      target = T->second;
    }

    std::cout << I->first.first  << " "
              << target          << " "
              << I->first.second << " "
              << I->second       << "\n";
  }
}

//==============================================================================
// Profiler Implementation
//
void IFCProfiler::incrementTarget(prof::CallSiteNumber cs, Address addr) {
  ProfileEntry entry = std::make_pair(cs, addr);
  Counts[entry]++;
}

void IFCProfiler::addTargetAddress(prof::FunctionNumber fn, Address addr) {
  //std::cout << "Added " << fn << " => " << addr << "\n";
  Targets[addr] = fn;
}

static void IFCExitHandler(void) {
  if(profiler) {
    dumpProfileMap(*profiler);
    delete profiler;
  }
}

//==============================================================================
// External C interface
//
extern "C"
void llvm_increment_indirect_target_count(prof::CallSiteNumber cs, Address addr) {
  if(profiler)
    profiler->incrementTarget(cs, addr);
}

extern "C"
void llvm_ifc_add_target_address(prof::FunctionNumber fn, Address addr) {
  if(profiler)
    profiler->addTargetAddress(fn, addr);
}

extern "C"
int llvm_start_indirect_function_call_profiling(int argc,
                                                const char **argv,
                                                unsigned *_unused,
                                                unsigned _unused2) {
  int Ret = save_arguments(argc, argv);
  profiler = new IFCProfiler();
  atexit(IFCExitHandler);
  return Ret;
}
