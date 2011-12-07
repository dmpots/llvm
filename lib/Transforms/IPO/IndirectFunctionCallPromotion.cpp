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
#include "llvm/Analysis/IndirectFunctionCallProfileInfo.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
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
    void getAnalysisUsage(AnalysisUsage &AU) const;
    
  private:
    typedef std::vector<Function *> TargetList;
    typedef std::pair<CallInst *, TargetList *> PromotionCandidate;
    typedef std::vector<PromotionCandidate> PromotionList;

    void findPromotionCandidates(Module *M,
                                 PromotionList *toPromote,
                                 const IndirectFunctionCallProfileInfo &PI);
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
// IFCP Implementation
//
bool IFCP::runOnModule(Module &M) {
  PromotionList toPromote;
  const IndirectFunctionCallProfileInfo &PI =
    getAnalysis<IndirectFunctionCallProfileInfo>();
  findPromotionCandidates(&M, &toPromote, PI);

  for(PromotionList::iterator I = toPromote.begin(), E = toPromote.end(); I != E; ++I) {
    NumPromotions++;
    promoteIndirectCall(*I);

    // Free temporary memory allocated in this pass for the TargetList
    delete I->second;
    I->second = NULL;
  }

  // return true if we inserted any promotions
  return (NumPromotions > 0);
}

void IFCP::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<IndirectFunctionCallProfileInfo>();
}

////////////////////////////////////////////////////////////////////////////////
/// Find indirect function calls that can be promoted to direct calls.
///
/// We use profiling information to find indirect calls that we have seen in a
/// profile run and have recorded some targets of the indirect call.
///
void IFCP::findPromotionCandidates
(Module *M,
 PromotionList *toPromote,
 const IndirectFunctionCallProfileInfo &ProfileInfo)
{
  const double PromotionThreshold = 0.30;

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
          // skip over any call sites where we don't have profiling info
          if(!ProfileInfo.hasProfileInfo(call))
            continue;

          // skip over non-tail calls we do this so that the promotion
          // transformation is easy to manage. With a tail call we can simply
          // place the target test at the end of the block. If the call was a
          // non-tail call then we would have to clone the remaninder of the
          // block after the indirect call when creating the new direct call.
          if(!call->isTailCall())
            continue;
          
          // Decide if we want to promote this function call and which targets
          // we should include in the promotion
          TargetList *Promotions = new TargetList();
          const CallSiteProfile& ProfileTargets =
            ProfileInfo.getCallSiteProfile(call);

          // Go through all potential targets and pick the ones that meet the
          // threshold
          for(CallSiteProfile::const_iterator T = ProfileTargets.begin(),
                TE = ProfileTargets.end(); T!=TE; ++T) {
            if(T->Percent >= PromotionThreshold) {
              Promotions->push_back(T->Target);
            }
          }

          // Add an entry to the promotions list for this call site if we have
          // any "good" targets.
          if(Promotions->size() == 0) {
            delete Promotions;
          }
          else {
            toPromote->push_back(make_pair(call, Promotions));
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
  const TargetList *targets = candidate.second;

  DEBUG(dbgs()
        << "Promoting call " << call->getCalledValue()->getName() << "() "
        << "in function @" <<call->getParent()->getParent()->getName()
        << " with " << targets->size() << " targets\n");

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
