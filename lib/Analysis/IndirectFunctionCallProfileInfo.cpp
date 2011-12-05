//===- IndirectFunctionCallProfileInfo.cpp --------------------*- C++ -*---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interface used by optimizers to load indirect function
// call profiles profiles, and provides a loader pass which reads an indirect
// function call profile file. It also includes the default implementation for
// the analysis group which reads no input files and provides no profiling
// information.
//
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "ifc-profile-info"

#include "llvm/Analysis/IndirectFunctionCallProfileInfo.h"
#include "llvm/Function.h"
#include "llvm/IndirectFunctionCallProfiling.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/PassRegistry.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/ProfileInfoTypes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cstdio>
#include <functional>
#include <numeric>

using namespace llvm;

STATISTIC(sNumProfileEntries,    "The # of profile entries read.");
STATISTIC(sNumProfileRuns,       "The # of profile runs read.");

namespace {
  class IndirectFunctionCallProfileLoader
    : public ModulePass,
      public IndirectFunctionCallProfileInfo
  {
  public:
    IndirectFunctionCallProfileLoader() : ModulePass(ID) {}

    ~IndirectFunctionCallProfileLoader() {}

    // this pass doesn't change anything (only loads information)
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
    }

    // the full name of the loader pass
    virtual const char* getPassName() const {
      return "IFC Profiling Information Loader";
    }

    // required since this pass implements multiple inheritance
    virtual void *getAdjustedAnalysisPointer(AnalysisID PI) {
      if (PI == &IndirectFunctionCallProfileInfo::ID)
        return (IndirectFunctionCallProfileInfo*)this;
      return this;
    }

    // entry point to run the pass
    bool runOnModule(Module &M);

    // pass identification
    static char ID;

  private:
    typedef std::vector<Function *> FunctionNumbering;
    typedef std::vector<const CallInst *> CallSiteNumbering;
    typedef std::pair<prof::FunctionNumber, prof::BigCounter> CounterPair;
    typedef std::vector<CounterPair> CounterVec;
    typedef std::map<prof::CallSiteNumber, CounterVec> CounterMap;

    // read input data file, return true if successful
    bool readProfileDataFile();

    // process argument info of a program from the input file
    void handleArgumentInfo(FILE*);

    // process profile info of a program from the input file
    void handleIFCProfileInfo(FILE*);

    // compute the correct function and callsite numbers by walking the module
    void computeFunctionAndCallSiteNumbers(Module&);

    // consolidate all the profiling entries into the profile map used by
    // clients of the analysis
    void populateCallProfileMap();

    // Predicate for testing equality of records in the counter vec
    class Target_eq : public std::unary_function<CounterPair, bool> {
      prof::FunctionNumber _fn;
    public:
      explicit Target_eq(prof::FunctionNumber fn) : _fn(fn) {}
      bool operator() (const CounterPair& p) {return _fn == p.first;}
    };

    // Get the actual call instruction or function matching the stored numbers
    // We want to keep the returned functions non-const because they will
    // eventually work their wiay into a IFCProfile record and we want to allow
    // clients to modify the target functions if they so desire.
    const CallInst *getCallInst(prof::CallSiteNumber);
    Function *getFunction(prof::FunctionNumber);

    // Mapping of Function/CallSite numbers to the Function or CallInst object
    FunctionNumbering Functions;
    CallSiteNumbering Calls;

    // Maps call site to all the counters recorded for that call site in the
    // profiling input file.
    CounterMap CallCounterMap;
  };
}


//===----------------------------------------------------------------------===//
//  Initialize analysis group
//

// register PathLoader
INITIALIZE_ANALYSIS_GROUP(IndirectFunctionCallProfileInfo,
                          "Indirect Function Call Profile Information",
                          NoIndirectFunctionCallProfileInfo)


//===----------------------------------------------------------------------===//
//  Profile Loader pass definition
//
INITIALIZE_AG_PASS(IndirectFunctionCallProfileLoader,
                   IndirectFunctionCallProfileInfo,
                   "load-ifc-profile",
                   "Load Indirect Function Call Profile Information",
                   false, // Is CFG Only
                   true,  // Is Analysis
                   false) // Is default Analysis Group Implementation

// command line option for loading ifc profiles
static cl::opt<std::string>
IFCProfileInfoFilename("ifc-profile-loader-file", cl::init("llvmprof.out"),
  cl::value_desc("filename"),
  cl::desc("IFC profile file loaded by -ifc-profile-loader"), cl::Hidden);

////////////////////////////////////////////////////////////////////////////////
// Link IndirectFunctionCallProfileLoaderPass as a pass, and make it available
// as an optimisation
//
char IndirectFunctionCallProfileLoader::ID = 0;
ModulePass *llvm::createIndirectFunctionCallProfileLoaderPass() {
  return new IndirectFunctionCallProfileLoader;
}

////////////////////////////////////////////////////////////////////////////////
// Analysis entry point
//
bool IndirectFunctionCallProfileLoader::runOnModule(Module& M) {
  // Read the profiling data from the input file.
  if(!readProfileDataFile()) {
    return false; // did not modify the module
  }

  // Compute the numbering of functions and call sites by walking the module. We
  // assume that we are given the same module that was used when inserting the
  // profiling callbacks otherwise the numbering we get from this module and the
  // numbering stored in the profiling data file will not match.
  computeFunctionAndCallSiteNumbers(M);

  // Now that we have read all of the profile data we can process it into
  // CallSiteProfile records that will be consumed by clients
  populateCallProfileMap();

  return false; // did not modify the module
}

////////////////////////////////////////////////////////////////////////////////
// Read the profile records from the input data file.
// Return true on sucess.
//
bool IndirectFunctionCallProfileLoader::readProfileDataFile() {
  // Open the input file
  const std::string& filename = IFCProfileInfoFilename;
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
      handleArgumentInfo(file);
      break;
    case IFCInfo:
      handleIFCProfileInfo(file);
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
void IndirectFunctionCallProfileLoader::handleArgumentInfo(FILE* file) {
  // get the argument list's length
  unsigned savedArgsLength;
  if( fread(&savedArgsLength, sizeof(unsigned), 1, file) != 1 ) {
    errs() << "warning: argument info header/data mismatch\n";
    assert(0 && "Unable to read full profiling header data");
    return;
  }
  // Skip past the stored arguments
  long offset = savedArgsLength;
  if (savedArgsLength & 3)   // byte alignment
    offset += 4-(savedArgsLength&3);

  int err = fseek(file, offset, SEEK_CUR);
  assert(!err && "Unable to seek past header in profile file");
}

////////////////////////////////////////////////////////////////////////////////
// Read the indirect function call profile records from the data file
// Update the CallCounterMap with the data from the records
//
void IndirectFunctionCallProfileLoader::handleIFCProfileInfo(FILE* file) {
  uint32_t NumEntries;
  if( fread(&NumEntries, sizeof(uint32_t), 1, file) != 1) {
    errs() << "error: IFC profiling header malformed\n";
    assert(false && "Unable to read number of IFC profile entries");
    return;
  }
  sNumProfileEntries += NumEntries;

  // We need to keep a map that maps a call site to the vector of
  // <Target,BigCounter> entries. Each time we get a profile entry we need to
  // check to see if we have already seen the target for that callsite (this
  // could happen either for multiple UnknownFunction entries in a single
  // profile run or from reading a data file containing multiple profile runs)
  // and add the counts to the record. After we have read all the entries from
  // all the profiling runs we can compute the final CallProfileMap
  // entries. This is done in the populateCallProfileMap function.
  for(uint32_t i = 0; i < NumEntries; ++i) {
    // Temporary storage for the profile record
    prof::IFCProfileRecord Record;

    // Read the record from the data file
    if(fread(&Record, sizeof(prof::IFCProfileRecord), 1, file) != 1) {
      errs() << "error: unable to read IFC record\n";
      assert(0 && "Unable to read IFC record, malformed profile data");
      return;
    }

    // Lookup the corresponding record in the counter map
    CounterVec& V = CallCounterMap[Record.CallSite];
    CounterVec::iterator VI =
      find_if(V.begin(), V.end(), Target_eq(Record.Target));

    // If the target already exists then simply add this record's count
    if(VI != V.end()) {
      VI->second += Record.Count;
    }
    // Otherwise add a new entry for this record
    else {
      V.push_back(std::make_pair(Record.Target, Record.Count));
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// We have to take care that we don't overflow the double value when converting
// from the 64-bit counter. We guard against this by checking for a negative
// value when converting to a double and if we find one then we return the max
// double value;
static double convertToDoubleWithOverflowCheck(prof::BigCounter Counter) {
  double D = static_cast<double>(Counter);
  if(D < 0.0) D = std::numeric_limits<double>::max();
  return D;
}

////////////////////////////////////////////////////////////////////////////////
// Sort two IFCTargets in descending order by Percent
//
static bool sortTargetsByPercent(const IFCTarget &T1, const IFCTarget T2) {
  return T1.Percent > T2.Percent;
}

////////////////////////////////////////////////////////////////////////////////
// After we have read all the profiling data in the input file we can compute
// the entries for the CallProfileMap. For each call site in the CallCounterMap
// we need to compute the percent that each target was taken and then sort the
// vector of targets in descending percent values.
//
void IndirectFunctionCallProfileLoader::populateCallProfileMap() {
  // For each call site in the counter map we compute the percentages that each
  // target was taken and then throw out the the targets that don't map to any
  // known Function in the current module.
  for(CounterMap::iterator CMI = CallCounterMap.begin(),
        CME = CallCounterMap.end(); CMI != CME; ++CMI) {
    // Pull items out of the iterator pair for easy reference
    prof::CallSiteNumber CallSite = CMI->first;
    const CounterVec& Counts = CMI->second;
    DEBUG(dbgs() << "Populating call profile for call site: " << CallSite
          << " with " << Counts.size() << " targets\n");

    // Lookup the call site number to get the call instruction.
    const CallInst *Call = getCallInst(CallSite);
    assert(Call && "Should have a call instruction for each call site number");

    // Get a reference to the fresh vector used to store the IFCTarget entries
    assert(CallProfileMap.find(Call) == CallProfileMap.end() &&
           "Unexpected duplicate call site in profile map");
    CallSiteProfile &Targets = CallProfileMap[Call];

    // Sum the total numuber of times we executed this call site across all of
    // the counters so we can compute individual target percentages
    // We assume that the total will safely fit in a BigCounter (64-bits)
    prof::BigCounter TotalHits = 0;
    for(CounterVec::const_iterator VI = Counts.begin(), VIE = Counts.end();
        VI!=VIE; ++VI) {
      prof::FunctionNumber FN  = VI->first;
      prof::BigCounter Counter = VI->second;

      // Add this targets contribution to the total number of hits for this call
      // site.
      TotalHits += Counter;

      // Get the actual function that corresponds to this function number. There
      // may not be a function for this function number, but that should only
      // happen for those entries that correspond to external function
      // (i.e. calls to functions that were not defined in the module). We still
      // want to add the counts for external functions because we want to be
      // able to detect when the internal functions are infrequently taken
      // (which will show up as a low overall percentage for the internal
      // targets).
      Function *F = getFunction(FN);
      assert((FN == prof::UnknownFunction || F) &&
             "Only unknown function number should not have a corresponding function.");
      if(!F) continue;

      // Make sure this target only appears once in the profile list for this
      // call site. We should never have multiple entries for the same function
      // in the profile target list.
      assert(std::find_if(Targets.begin(),
                          Targets.end(),
                          FunTarget_eq(F)) == Targets.end() &&
             "Unexpected duplicate target function in CallProfile");

      // Insert the target into profile targets vector for this call site
      Targets.push_back(IFCTarget(F, convertToDoubleWithOverflowCheck(Counter)));
    }

    // Check to see that we have at least one known target. If not, then erase
    // this call site from the map and move on to the next call site.
    if(Targets.size() == 0) {
      CallProfileMap.erase(Call);
      continue;
    }

    // Now run through the targets and compute the percentages.
    double dTotal = convertToDoubleWithOverflowCheck(TotalHits);
    for(CallSiteProfile::iterator Target = Targets.begin(), TE = Targets.end();
        Target != TE; ++Target) {
      Target->Percent /= dTotal;
    }

    // Finally sort the targets by percent in decreasing order
    std::sort(Targets.begin(), Targets.end(), sortTargetsByPercent);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Indirect call predicate
// TODO: would be nice to be able to share this among multiple libraries...
//
static bool isIndirectCall(const CallInst& call) {
  // An indirect call will return NULL for the Function
  if(call.getCalledFunction()) return false;


  // Seems that intrinsic fucntions also return null for getCalledFunction
  // try to filter them out by checking that we are not calling a global value.
  if(isa<GlobalValue>(call.getCalledValue())) return false;

  return true;
}

////////////////////////////////////////////////////////////////////////////////
// Compute the correct function and callsite numbers by walking the module
//
void
IndirectFunctionCallProfileLoader::computeFunctionAndCallSiteNumbers(Module& M) {
  // FunctionNumber and CallSiteNumber 0 is used to indicate an invalid value
  assert((Functions.size() == 0) && (Calls.size() == 0) &&
         "Functions and Calls should start out empty");
  Functions.push_back(0);
  Calls.push_back(0);

  // Examine every function and callsite in the program
  for(Module::iterator F = M.begin(), FE = M.end(); F!=FE; ++F) {
    // Map this function to the current index at the end of the vector
    Functions.push_back(F);
    for(Function::iterator B = F->begin(), BE = F->end(); B!=BE; ++B){
      for(BasicBlock::iterator I = B->begin(), IE = B->end(); I!=IE; ++I) {
        CallInst *C = dyn_cast<CallInst>(I);
        if(!C || !isIndirectCall(*C))
          continue;

        // Map this callsite to the current index at the end of the vector
        Calls.push_back(C);
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// Get the CallInst matching a call site number
//
const CallInst*
IndirectFunctionCallProfileLoader::getCallInst(prof::CallSiteNumber CallSite) {
  assert(CallSite < (Calls.size() - 1) && "Invalid call site number");
  return Calls[CallSite];
}

////////////////////////////////////////////////////////////////////////////////
// Get the Function matching a function number
//
Function*
IndirectFunctionCallProfileLoader::getFunction(prof::FunctionNumber FN) {
  assert(FN < (Functions.size() - 1) && "Invalid function number");
  return Functions[FN];
}


//===----------------------------------------------------------------------===//
// IndirectFunctionCallProfileInfo implementation
//
// This section contains the definition of the profile information interface
// used by passes to get the indirect function call profile info.
//
char IndirectFunctionCallProfileInfo::ID = 0;
IndirectFunctionCallProfileInfo::IndirectFunctionCallProfileInfo()  {}
IndirectFunctionCallProfileInfo::~IndirectFunctionCallProfileInfo() {}
const CallSiteProfile IndirectFunctionCallProfileInfo::NoProfileInfo;

const CallSiteProfile&
IndirectFunctionCallProfileInfo::getCallSiteProfile(const CallInst* call) const {
  ProfileMap::const_iterator P = CallProfileMap.find(call);
  if(P == CallProfileMap.end()) return NoProfileInfo;

  return P->second;
}

/// Return true if there is profiling info for this callsite
bool IndirectFunctionCallProfileInfo::hasProfileInfo(const CallInst *call) const {
  return getCallSiteProfile(call).size() > 0;
}

/// Returns the most frequently taken target for this call site
Function*
IndirectFunctionCallProfileInfo::getMostFrequentTarget(const CallInst *call) const {
  if(!hasProfileInfo(call))
    return NULL;
  return getCallSiteProfile(call).front().Target;
}

/// Returns a const reference to the underlying profile map. Useful
/// if you want to perform some action (such as iterating over all
/// call sites) not provided by the more convenient function
/// interface above).
const IndirectFunctionCallProfileInfo::ProfileMap&
IndirectFunctionCallProfileInfo::getProfileMap() const {
  return CallProfileMap;
}

/// Predicate for testing equality of IFCTarget records by Function
/// useful for using with the find_if stl algorithm.
IndirectFunctionCallProfileInfo::FunTarget_eq::FunTarget_eq(Function* fn)
  : _fn(fn) {}
bool IndirectFunctionCallProfileInfo::FunTarget_eq::operator()(const IFCTarget& t)
  {return t.Target == _fn;}


//===----------------------------------------------------------------------===//
// NoProfile PathProfileInfo implementation
//
// The definition of the default implementatio of IFC profile info which
// contains no profile info and does not attempt to read it from any files.
//
namespace {
  class NoIndirectFunctionCallProfileInfo
    : public ImmutablePass,
      public IndirectFunctionCallProfileInfo {
  public:
    static char ID; // Class identification, replacement for typeinfo
    NoIndirectFunctionCallProfileInfo() : ImmutablePass(ID) {}

    /// getAdjustedAnalysisPointer - This method is used when a pass implements
    /// an analysis interface through multiple inheritance.  If needed, it
    /// should override this to adjust the this pointer as needed for the
    /// specified pass info.
    virtual void *getAdjustedAnalysisPointer(AnalysisID PI) {
      if (PI == &IndirectFunctionCallProfileInfo::ID)
        return (IndirectFunctionCallProfileInfo*)this;
      return this;
    }

    virtual const char *getPassName() const {
      return "NoIndirectFunctionCallProfileInfo";
    }
  };
}  // End of anonymous namespace

char NoIndirectFunctionCallProfileInfo::ID = 0;
// Register the NoIndirectFunctionCallProfileInfo pass...
INITIALIZE_AG_PASS(NoIndirectFunctionCallProfileInfo,
                   IndirectFunctionCallProfileInfo,
                   "no-ifc-profile",
                   "No Indirect Function Call Profile Information",
                   false, // Is CFG Only
                   true,  // Is Analysis
                   true)  // Is default Analysis Group Implementation

// Link NoIndirectFunctionCallProfileInfo as a pass, and make it available as an
// optimisation
ImmutablePass *llvm::createNoIndirectFunctionCallProfileInfoPass() {
  return new NoIndirectFunctionCallProfileInfo;
}
