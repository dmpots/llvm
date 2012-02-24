//===- TraceProfileInfo.cpp -----------------------------------*- C++ -*---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interface used by optimizers to load trace profiles,
// and provides a loader pass which reads a trace profile file.
//
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "trace-profile-info"

#include "llvm/Analysis/TraceProfileInfo.h"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/ProfileInfoTypes.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Profile/ProfilingSupport.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;
using namespace llvm::prof;

STATISTIC(sNumProfileRuns,       "The # of profile runs read.");
STATISTIC(sNumTraces,            "The # of traces read from profiles.");
STATISTIC(sNumBrokenTraces,      "The # of broken traces read from profiles.");

namespace {
  class TraceProfileLoader
    : public ModulePass,
      public TraceProfileInfo
  {
  public:
    TraceProfileLoader() : ModulePass(ID) {
      initializeTraceProfileLoaderPass(*PassRegistry::getPassRegistry());
    }
    ~TraceProfileLoader() {}

    // this pass doesn't change anything (only loads information)
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
    }

    // the full name of the loader pass
    virtual const char* getPassName() const {
      return "Trace Profiling Information Loader";
    }

    // required since this pass implements multiple inheritance
    virtual void *getAdjustedAnalysisPointer(AnalysisID PI) {
      if (PI == &TraceProfileInfo::ID)
        return (TraceProfileInfo*)this;
      return this;
    }

    // entry point to run the pass
    bool runOnModule(Module &M);

    // pass identification
    static char ID;

  private:
    // read input data file, return true if successful
    bool readProfileDataFile();

    // process argument info of a program from the input file
    bool handleArgumentInfo(FILE*);

    // process profile info of a program from the input file
    bool handleTraceProfileInfo(FILE*);

    BasicBlock* getBlock(BasicBlockNumber BN) const;

    // Mapping of Function/Block numbers to the actual object
    prof::FunctionNumbering   FunctionNumbering;
    prof::BasicBlockNumbering BlockNumbering;
  };
}

//----------------------------------------------------------------------------//
//                          Top Level Initialization Stuff
//----------------------------------------------------------------------------//
// command line option for loading trace profiles
static cl::opt<std::string>
TraceProfileInfoFilename("trace-profile-loader-file", cl::init("llvmprof.out"),
  cl::value_desc("filename"),
  cl::desc("Trace profile file loaded by -trace-profile-loader"), cl::Hidden);

INITIALIZE_ANALYSIS_GROUP(TraceProfileInfo,
                          "Trace Profile Information",
                          TraceProfileLoader)

INITIALIZE_AG_PASS(TraceProfileLoader,
                   TraceProfileInfo,
                   "load-trace-profile",
                   "Load trace profile data",
                   false, // Is CFG Only
                   true,  // Is Analysis
                   true)  // Is default Analysis Group Implementation

static bool traceSorter(const TraceProfile& T1, const TraceProfile& T2) {
  return T1.ExecutionPercent > T2.ExecutionPercent;
}

//----------------------------------------------------------------------------//
//                          TraceProfileLoader Implementation
//----------------------------------------------------------------------------//
char TraceProfileLoader::ID = 0;

bool TraceProfileLoader::runOnModule(Module& M) {
  // recompute the basic block and function numbering so we have up-to-date
  // numbers to match with the profile
  prof::computeNumbering(M, &FunctionNumbering, &BlockNumbering, NULL);

  // read profiling data from the input file
  readProfileDataFile();

  // compute percents
  double Total = 0.0;
  for(TraceProfileList::iterator I = TraceProfiles.begin(),
        E = TraceProfiles.end(); I != E; ++I){
    Total += I->ExecutionPercent;
  }
  for(TraceProfileList::iterator I = TraceProfiles.begin(),
        E = TraceProfiles.end(); I != E; ++I){
    I->ExecutionPercent /= Total;
  }

  // sort by most common trace
  std::sort(TraceProfiles.begin(), TraceProfiles.end(), traceSorter);

  // record trace numbers in sorted order
  int i = 0;
  for(TraceProfileList::iterator I = TraceProfiles.begin(),
        E = TraceProfiles.end(); I != E; ++I){
    I->TraceNum = i++;
  }


  return false; // does not modify the module
}

////////////////////////////////////////////////////////////////////////////////
// Read profile data from the input file and populate the Traces data structure
//
bool TraceProfileLoader::readProfileDataFile() {
  // Open the input file
  const std::string& filename = TraceProfileInfoFilename;
  FILE *file = NULL;

  if (!(file = fopen(filename.c_str(), "rb"))) {
    errs () << "error: input '" << filename << "' file does not exist.\n";
    assert(false && "Profiling data file does not exist!");
    return false; // error reading data
  }

  // Loop over all profiling records in the input file and store the results in
  // the counter map
  ProfilingType profType;
  while( fread(&profType, sizeof(ProfilingType), 1, file) ) {
    switch (profType) {
    case ArgumentInfo:
      sNumProfileRuns++;
      if(!handleArgumentInfo(file)) return false;
      break;
    case TraceInfo:
      if(!handleTraceProfileInfo(file)) return false;
      break;
    default:
      errs () << "error: ifc profiling file syntax, unexpected prof type: "
              << profType << "\n";
      fclose(file);
      assert(false && "Unknown ProfilingType in ifc profiling file");
      return false; // error reading data
    }
  }

  fclose(file);
  return true; // data read sucessfully
}

////////////////////////////////////////////////////////////////////////////////
// Read the program argument info stored in the profile data file
//
bool TraceProfileLoader::handleArgumentInfo(FILE* file) {
  // get the argument list's length
  unsigned savedArgsLength;
  if( fread(&savedArgsLength, sizeof(unsigned), 1, file) != 1 ) {
    errs() << "warning: argument info header/data mismatch\n";
    assert(0 && "Unable to read full profiling header data");
    return false;
  }
  // Skip past the stored arguments
  long offset = savedArgsLength;
  if (savedArgsLength & 3)   // byte alignment
    offset += 4-(savedArgsLength&3);

  int err = fseek(file, offset, SEEK_CUR);
  assert(!err && "Unable to seek past header in profile file");
  return true;
}

static bool isBreak(TraceProfileRecord& R) {return R.Tag == TraceGapRecord;}

bool TraceProfileLoader::handleTraceProfileInfo(FILE *file) {

  TraceProfileHeader Header;
  if(fread(&Header, sizeof(TraceProfileHeader), 1, file) != 1) {
    errs() << "error: unable to read trace profile info header";
    assert(0 && "Unable to read full trace profiling header data");
    return false;
  }

  TraceProfileRecord *Records = new TraceProfileRecord[Header.TraceSize];
  if(fread(Records, sizeof(TraceProfileRecord), Header.TraceSize, file)
     != static_cast<size_t>(Header.TraceSize)) {
    errs() << "error: unable to read trace profile info header";
    assert(0 && "Unable to read full trace profiling data");
    return false;
  }

  // Check to see if the trace is broken. If so then we can exit early since we
  // won't be loading the trace
  for(int i = 0; i < Header.TraceSize; i++) {
    if(isBreak(Records[i])) {
      sNumBrokenTraces++;
      NumBrokenTraces++;
      return true; // No error encountered
    }
  }

  // Insert a new empty profile into the end of the profiles vector
  sNumTraces++;
  TraceProfiles.resize(TraceProfiles.size() + 1);
  TraceProfile& Profile = TraceProfiles.back();

  // Create the profile by looking up the basic blocks corresponding to the
  // block numbers contained in the profile records
  Profile.ExecutionPercent = convertToDoubleWithOverflowCheck(Header.NumHits);
  for(int i = 0; i < Header.TraceSize; ++i) {
    BasicBlock *B = getBlock(Records[i].BlockNumber);
    Profile.Blocks.push_back(B);
  }

  return true;
}

BasicBlock* TraceProfileLoader::getBlock(BasicBlockNumber BN) const {
  assert(BN < BlockNumbering.size() && "Unknown block number");
  return BlockNumbering[BN];
}

ModulePass *llvm::createTraceProfileLoaderPass() {
  return new TraceProfileLoader();
}

//----------------------------------------------------------------------------//
//                          TraceProfileInfo Implementation
//----------------------------------------------------------------------------//
char TraceProfileInfo::ID = 0;

TraceProfileInfo::TraceProfileInfo() : NumBrokenTraces(0)  {}
TraceProfileInfo::~TraceProfileInfo() {}

TraceProfileList& TraceProfileInfo::getTraces() {
  return TraceProfiles;
}

int TraceProfileInfo::brokenTraceCount() const {
  return NumBrokenTraces;
}

//----------------------------------------------------------------------------//
//                          TraceProfile Implementation
//----------------------------------------------------------------------------//
TraceProfile::FunctionList& TraceProfile::getFunctions() {
  if(Functions.size() != 0) return Functions;

  Function *Fprev = NULL;
  for(iterator B = Blocks.begin(), BE = Blocks.end(); B != BE; ++B) {
    Function    *F  = (*B)->getParent();
    if(F != Fprev) {
      Fprev = F;
      Functions.push_back(F);
    }
  }

  return Functions;
}
