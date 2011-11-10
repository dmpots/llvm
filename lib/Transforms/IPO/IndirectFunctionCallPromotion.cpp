//===-- IndirectFunctionCallPromotion.cpp - Make indirect calls direct -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This transform is designed to promote indirect function calls to direct
// function calls. It uses profiling information to find potential targets of
// and indirect function call. It then changes the indirect call to a test
// against the actual target and a direct call if it matches one of the profiled
// targets. If none of the targets match it falls back to an indirect call.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ifcp"
#include "llvm/BasicBlock.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/InitializePasses.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <set>
using namespace llvm;

STATISTIC(NumPromotions, "Number of functions promoted");

namespace {
  class IndirectFunctionCallPromotion : public ModulePass {
  public:
    static char ID; // Pass identification, replacement for typeid
    IndirectFunctionCallPromotion() : ModulePass(ID) {
      initializeIndirectFunctionCallPromotionPass
        (*PassRegistry::getPassRegistry());
    }

    // run - Do the IFCP pass on the specified module
    //
    bool runOnModule(Module &M);
    
  private:
    typedef std::vector<Function *> TargetList;
    typedef std::pair<CallInst *, TargetList *> PromotionCandidate;
    typedef std::vector<PromotionCandidate> PromotionSet;

    void findPromotionCandidates(Module *M, PromotionSet *toPromote);
    void promoteIndirectCall(const PromotionCandidate& candidate);
  };

  typedef IndirectFunctionCallPromotion IFCP;
}

//==============================================================================
// Module Initialization
//
char IFCP::ID = 0;
INITIALIZE_PASS(IndirectFunctionCallPromotion, "ifcp",
                "Indirect Function Call Promotion", false, false)

ModulePass *llvm::createIndirectFunctionCallPromotionPass() { 
  return new IFCP(); 
}

//==============================================================================
// Helper Functions
//
static bool isIndirectCall(const CallInst& call) {
  // An indirect call will return NULL for the Function
  return call.getCalledFunction();
}

//==============================================================================
// IFCP Implementation
//
bool IFCP::runOnModule(Module &M) {
  PromotionSet toPromote;
  findPromotionCandidates(&M, &toPromote);

  for(PromotionSet::iterator I = toPromote.begin(), E = toPromote.end(); I != E; ++I) {
    NumPromotions++;
    DEBUG(dbgs() << "Promoting Function Call: " << *I->first << "\n");
    promoteIndirectCall(*I);
  }
  
  return true;
}

////////////////////////////////////////////////////////////////////////////////
/// Find indirect function calls that can be promoted to direct calls.
///
/// We use profiling information to find indirect calls that we have seen in a
/// profile run and have recorded some targets of the indirect call.
///
void IFCP::findPromotionCandidates(Module *M, PromotionSet *toPromote){
  // Loop over the module, looking for indirect function calls
  for (Module::iterator I = M->begin(), E = M->end(); I != E; ++I) {
    
    // Look at all the functions in the Module
    Function *F = dyn_cast<Function>(I);
    if(!F) continue;
    if(F->isDeclaration()) continue;
    
    // Look at all the calls in this function
    for (Function::iterator b = F->begin(), be = F->end(); b != be; ++b) {
      for (BasicBlock::iterator i = b->begin(), ie = b->end(); i != ie; ++i) {
        if (CallInst* call = dyn_cast<CallInst>(&*i)) {
          // Skip over direct function calls and non-tail calls
          if(isIndirectCall(*call) || !call->isTailCall())
            continue;

          // Get the defintion location for the indirect call
          Instruction *target = dyn_cast<Instruction>(call->getCalledValue());
          if(!target) continue;

          DEBUG(dbgs() 
                << "Indirect Tail Call in Function: " << F->getName() << " "
                << "to value: " << target->getName()
                << "\n");
          
          // Decide if we can try to promote this function
          // TODO: read profile info to see if we know any targets
          if(target->getName() == "ln1L9"){
            TargetList *targets = new TargetList(1, M->getFunction("s1C0_entry"));
            toPromote->push_back(make_pair(call, targets));
          }
        }
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
/// Promote an indirect function call to a series of tests to potential direct
/// targets with a fallback to the indirect function call if none of the direct
/// candidates match the current target.
///
/// Currently we only test for the first direct target in the TargetList, but
/// this could be expanded to test multiple targets.
///
void IFCP::promoteIndirectCall(const PromotionCandidate& candidate) {
  // Pull apart the candidate and set some useful top level values
  CallInst *call = candidate.first;
  TargetList *targets = candidate.second;
  BasicBlock *BB = call->getParent();
  Function *F = BB->getParent();
  LLVMContext &C = F->getContext();
  
  assert(!targets->empty() && "Should have a target to promote call");
  Function *directTarget = targets->front();
  Value    *indirectTarget = call->getCalledValue();

  // Create new block for direct call
  BasicBlock *bbDirect = BasicBlock::Create(C, "ifcp.direct", F);
  CallInst *directCall = cast<CallInst>(call->clone());
  directCall->setCalledFunction(directTarget);
  bbDirect->getInstList().push_back(directCall);
  bbDirect->getInstList().push_back(BB->getTerminator()->clone());
  
  // Create default block for falling back to indirect call
  BasicBlock *bbFallback = BasicBlock::Create(C, "ifcp.fallback", F);
  bbFallback->getInstList().push_back(call->clone());
  bbFallback->getInstList().push_back(BB->getTerminator()->clone());

  // Create cmp instruction for potential targets
  ICmpInst *cmp = new ICmpInst(call, CmpInst::ICMP_EQ, 
                               indirectTarget, directTarget, "ifcp.hit");
  
  // Create br instruction to jump to correct landing pad
  BranchInst::Create(bbDirect, bbFallback, cmp, call);

  // Erase old call and terminator instructions
  call->eraseFromParent();
  BB->getTerminator()->eraseFromParent();
}
