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
#include "llvm/Support/DataTypes.h"
#include "llvm/Profile/ProfilingSupport.h"
#if !defined(_MSC_VER) && !defined(__MINGW32__)
#include <unistd.h>
#else
#include <io.h>
#endif
#include <iostream>
#include <map>


using namespace llvm;
using namespace llvm::prof;

namespace {
  typedef void* Address;

  class Tracer {
  public:
    void writeDataToFile(int fd);
    void traceHead(BasicBlockNumber);
    void tracePath(BasicBlockNumber);
    void traceBreak(FunctionNumber);
    void checkTraceBreak(Address);
    void addFunctionMapping(FunctionNumber FN, Address Addr);
  };

}

Tracer *tracer;


//==============================================================================
// Tracer Implementation
//
void Tracer::writeDataToFile(int fd) {
}

void Tracer::traceHead(BasicBlockNumber BBHead) {
  std::cout << "Trace Head " << BBHead << "\n";
}

void Tracer::tracePath(BasicBlockNumber BBPath) {
  std::cout << "Trace Path " << BBPath << "\n";
}

void Tracer::checkTraceBreak(Address Addr) {
  std::cout << "Check Break: " << Addr << "\n";
}

void Tracer::traceBreak(FunctionNumber FN) {
  std::cout << "Break Trace: " << FN << "\n";
}

void Tracer::addFunctionMapping(FunctionNumber FN, Address Addr) {
}

static void TracerExitHandler(void) {
  if(!tracer) return;

  int fd = getOutFile();
  tracer->writeDataToFile(fd);

  delete tracer;
}

//==============================================================================
// External C interface
//
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
int llvm_start_trace_runtime(int argc,
                             const char **argv,
                             unsigned *_unused,
                             unsigned _unused2) {
  int Ret = save_arguments(argc, argv);
  tracer = new Tracer();
  atexit(TracerExitHandler);
  return Ret;
}
