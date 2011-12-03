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
#if !defined(_MSC_VER) && !defined(__MINGW32__)
#include <unistd.h>
#else
#include <io.h>
#endif
#include <iostream>
#include <map>


using namespace llvm;

namespace {
  typedef void*    Address;

  class IFCProfiler {
  public:
    // Types
    typedef prof::BigCounter Counter;
    typedef std::pair<prof::CallSiteNumber, Address> ProfileEntry;
    typedef std::map<ProfileEntry, Counter> ProfileMap;
    typedef std::map<Address, prof::FunctionNumber> TargetMap;

    // Constructors
    IFCProfiler();

    // Methods
    void incrementTarget(prof::CallSiteNumber cs, Address target);
    void addTargetAddress(prof::FunctionNumber fn, Address addr);
    void writeDataToFile(int fd);
    void writeProfileRecord(int fd, const ProfileEntry &entry, Counter count);

    // Accessors
    inline ProfileMap &getCounts()   {return Counts;}
    inline TargetMap  &getTargets()  {return Targets;}

  private:
    ProfileMap Counts;
    TargetMap Targets;
    uint32_t NumRecordsWritten;
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
IFCProfiler::IFCProfiler() : NumRecordsWritten(0) {}

void IFCProfiler::incrementTarget(prof::CallSiteNumber cs, Address addr) {
  ProfileEntry entry = std::make_pair(cs, addr);
  Counts[entry]++;
}

void IFCProfiler::addTargetAddress(prof::FunctionNumber fn, Address addr) {
  //std::cout << "Added " << fn << " => " << addr << "\n";
  Targets[addr] = fn;
}

////////////////////////////////////////////////////////////////////////////////
// Writes profiling data to output file
//
// The profiling data consists of a header followed by a number of
// profiling record entries. The header contains an identifier of the
// profiling info along with the number of records.
//
// Header:
// <-- 4 bytes --> <-- 4 bytes -->
// +--------------+---------------+
// | IFCInfo      | # of Records  |
// +--------------+---------------+
//
// A record contains the callsite number, the target function number
// and the number of times the edge was taken.
//
// Record:
//  <-- 4 bytes --> <-- 4 bytes  --> <--   8 bytes  -->
// +---------------+----------------+------------------+
// |   CallSite#   |Target Function#|     Count        |
// +---------------+----------------+------------------+
//
#define HEADER_SIZE 2
void IFCProfiler::writeDataToFile(int fd) {
  uint32_t header[HEADER_SIZE] = {IFCInfo, 0};

  // Skip over the header for now
  off_t headerLocation = lseek(fd, 0, SEEK_CUR);
  lseek(fd, HEADER_SIZE*sizeof(uint32_t), SEEK_CUR);

  // Write profile records
  for(ProfileMap::const_iterator I = Counts.begin(), E = Counts.end(); I != E; ++I){
    writeProfileRecord(fd, I->first, I->second);
  }

  // Setup and write the ifc profile header
  header[1] = NumRecordsWritten;
  off_t currentLocation = lseek(fd, 0, SEEK_CUR);
  lseek(fd, headerLocation, SEEK_SET);
  if (write(fd, header, sizeof(header)) < 0) {
    std::cerr << "error: unable to write path profile header to output file.\n";
    return;
  }

  // Move back to the end of the file
  lseek(fd, currentLocation, SEEK_SET);
}

void IFCProfiler::writeProfileRecord(int fd,
                                     const ProfileEntry &entry,
                                     Counter count) {
  // See if we have a mapping for the target address
  prof::FunctionNumber target = 0;
  TargetMap::const_iterator T = Targets.find(entry.second);
  if(T != Targets.end()) {
    target = T->second;
  }

  // The call targets an unknown function, so don't write a record
  if(target == 0) return;

  // Write the record to the output file
  prof::IFCProfileRecord record;
  record.CallSite = entry.first;
  record.Target   = target;
  record.Count    = count;
  if(write(fd, &record, sizeof(prof::IFCProfileRecord)) < 0) {
    std::cerr << "error: unable to write ifc entry to output file.\n";
    return;
  }
  NumRecordsWritten++;
}

static void IFCExitHandler(void) {
  if(!profiler) return;

  if(0) dumpProfileMap(*profiler);
  int fd = getOutFile();
  profiler->writeDataToFile(fd);

  delete profiler;
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
