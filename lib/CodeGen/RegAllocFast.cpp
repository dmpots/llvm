//===-- RegAllocFast.cpp - A fast register allocator for debug code -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This register allocator allocates registers to a basic block at a time,
// attempting to keep values in registers and reusing registers as appropriate.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "regalloc"
#include "llvm/CodeGen/Passes.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SparseSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/CodeGen/RegisterClassInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetMachine.h"
#include <algorithm>
using namespace llvm;

STATISTIC(NumStores, "Number of stores added");
STATISTIC(NumLoads , "Number of loads added");
STATISTIC(NumCopies, "Number of copies coalesced");

static RegisterRegAlloc
  fastRegAlloc("fast", "fast register allocator", createFastRegisterAllocator);

namespace {
  class RAFast : public MachineFunctionPass {
  public:
    static char ID;
    RAFast() : MachineFunctionPass(ID), StackSlotForVirtReg(-1),
               isBulkSpilling(false) {}
  private:
    const TargetMachine *TM;
    MachineFunction *MF;
    MachineRegisterInfo *MRI;
    const TargetRegisterInfo *TRI;
    const TargetInstrInfo *TII;
    RegisterClassInfo RegClassInfo;

    // Basic block currently being allocated.
    MachineBasicBlock *MBB;

    // StackSlotForVirtReg - Maps virtual regs to the frame index where these
    // values are spilled.
    IndexedMap<int, VirtReg2IndexFunctor> StackSlotForVirtReg;

    // Everything we know about a live virtual register.
    struct LiveReg {
      MachineInstr *LastUse;    // Last instr to use reg.
      unsigned VirtReg;         // Virtual register number.
      unsigned PhysReg;         // Currently held here.
      unsigned short LastOpNum; // OpNum on LastUse.
      bool Dirty;               // Register needs spill.

      explicit LiveReg(unsigned v)
        : LastUse(0), VirtReg(v), PhysReg(0), LastOpNum(0), Dirty(false) {}

      unsigned getSparseSetIndex() const {
        return TargetRegisterInfo::virtReg2Index(VirtReg);
      }
    };

    typedef SparseSet<LiveReg> LiveRegMap;

    // LiveVirtRegs - This map contains entries for each virtual register
    // that is currently available in a physical register.
    LiveRegMap LiveVirtRegs;

    DenseMap<unsigned, SmallVector<MachineInstr *, 4> > LiveDbgValueMap;

    // RegState - Track the state of a physical register.
    enum RegState {
      // A disabled register is not available for allocation, but an alias may
      // be in use. A register can only be moved out of the disabled state if
      // all aliases are disabled.
      regDisabled,

      // A free register is not currently in use and can be allocated
      // immediately without checking aliases.
      regFree,

      // A reserved register has been assigned explicitly (e.g., setting up a
      // call parameter), and it remains reserved until it is used.
      regReserved

      // A register state may also be a virtual register number, indication that
      // the physical register is currently allocated to a virtual register. In
      // that case, LiveVirtRegs contains the inverse mapping.
    };

    // PhysRegState - One of the RegState enums, or a virtreg.
    std::vector<unsigned> PhysRegState;

    typedef SparseSet<unsigned> UsedInInstrSet;

    // UsedInInstr - Set of physregs that are used in the current instruction,
    // and so cannot be allocated.
    UsedInInstrSet UsedInInstr;

    // SkippedInstrs - Descriptors of instructions whose clobber list was
    // ignored because all registers were spilled. It is still necessary to
    // mark all the clobbered registers as used by the function.
    SmallPtrSet<const MCInstrDesc*, 4> SkippedInstrs;

    // isBulkSpilling - This flag is set when LiveRegMap will be cleared
    // completely after spilling all live registers. LiveRegMap entries should
    // not be erased.
    bool isBulkSpilling;

    enum {
      spillClean = 1,
      spillDirty = 100,
      spillImpossible = ~0u
    };
  public:
    virtual const char *getPassName() const {
      return "Fast Register Allocator";
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

  private:
    bool runOnMachineFunction(MachineFunction &Fn);
    void AllocateBasicBlock();
    void handleThroughOperands(MachineInstr *MI,
                               SmallVectorImpl<unsigned> &VirtDead);
    int getStackSpaceFor(unsigned VirtReg, const TargetRegisterClass *RC);
    bool isLastUseOfLocalReg(MachineOperand&);

    void addKillFlag(const LiveReg&);
    void killVirtReg(LiveRegMap::iterator);
    void killVirtReg(unsigned VirtReg);
    void spillVirtReg(MachineBasicBlock::iterator MI, LiveRegMap::iterator);
    void spillVirtReg(MachineBasicBlock::iterator MI, unsigned VirtReg);

    void usePhysReg(MachineOperand&);
    void definePhysReg(MachineInstr *MI, unsigned PhysReg, RegState NewState);
    unsigned calcSpillCost(unsigned PhysReg) const;
    void assignVirtToPhysReg(LiveReg&, unsigned PhysReg);
    LiveRegMap::iterator findLiveVirtReg(unsigned VirtReg) {
      return LiveVirtRegs.find(TargetRegisterInfo::virtReg2Index(VirtReg));
    }
    LiveRegMap::const_iterator findLiveVirtReg(unsigned VirtReg) const {
      return LiveVirtRegs.find(TargetRegisterInfo::virtReg2Index(VirtReg));
    }
    LiveRegMap::iterator assignVirtToPhysReg(unsigned VReg, unsigned PhysReg);
    LiveRegMap::iterator allocVirtReg(MachineInstr *MI, LiveRegMap::iterator,
                                      unsigned Hint);
    LiveRegMap::iterator defineVirtReg(MachineInstr *MI, unsigned OpNum,
                                       unsigned VirtReg, unsigned Hint);
    LiveRegMap::iterator reloadVirtReg(MachineInstr *MI, unsigned OpNum,
                                       unsigned VirtReg, unsigned Hint);
    void spillAll(MachineBasicBlock::iterator MI);
    bool setPhysReg(MachineInstr *MI, unsigned OpNum, unsigned PhysReg);
    void addRetOperands(MachineBasicBlock *MBB);
  };
  char RAFast::ID = 0;
}

/// getStackSpaceFor - This allocates space for the specified virtual register
/// to be held on the stack.
int RAFast::getStackSpaceFor(unsigned VirtReg, const TargetRegisterClass *RC) {
  // Find the location Reg would belong...
  int SS = StackSlotForVirtReg[VirtReg];
  if (SS != -1)
    return SS;          // Already has space allocated?

  // Allocate a new stack object for this spill location...
  int FrameIdx = MF->getFrameInfo()->CreateSpillStackObject(RC->getSize(),
                                                            RC->getAlignment());

  // Assign the slot.
  StackSlotForVirtReg[VirtReg] = FrameIdx;
  return FrameIdx;
}

/// isLastUseOfLocalReg - Return true if MO is the only remaining reference to
/// its virtual register, and it is guaranteed to be a block-local register.
///
bool RAFast::isLastUseOfLocalReg(MachineOperand &MO) {
  // If the register has ever been spilled or reloaded, we conservatively assume
  // it is a global register used in multiple blocks.
  if (StackSlotForVirtReg[MO.getReg()] != -1)
    return false;

  // Check that the use/def chain has exactly one operand - MO.
  MachineRegisterInfo::reg_nodbg_iterator I = MRI->reg_nodbg_begin(MO.getReg());
  if (&I.getOperand() != &MO)
    return false;
  return ++I == MRI->reg_nodbg_end();
}

/// addKillFlag - Set kill flags on last use of a virtual register.
void RAFast::addKillFlag(const LiveReg &LR) {
  if (!LR.LastUse) return;
  MachineOperand &MO = LR.LastUse->getOperand(LR.LastOpNum);
  if (MO.isUse() && !LR.LastUse->isRegTiedToDefOperand(LR.LastOpNum)) {
    if (MO.getReg() == LR.PhysReg)
      MO.setIsKill();
    else
      LR.LastUse->addRegisterKilled(LR.PhysReg, TRI, true);
  }
}

/// killVirtReg - Mark virtreg as no longer available.
void RAFast::killVirtReg(LiveRegMap::iterator LRI) {
  addKillFlag(*LRI);
  assert(PhysRegState[LRI->PhysReg] == LRI->VirtReg &&
         "Broken RegState mapping");
  PhysRegState[LRI->PhysReg] = regFree;
  // Erase from LiveVirtRegs unless we're spilling in bulk.
  if (!isBulkSpilling)
    LiveVirtRegs.erase(LRI);
}

/// killVirtReg - Mark virtreg as no longer available.
void RAFast::killVirtReg(unsigned VirtReg) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "killVirtReg needs a virtual register");
  LiveRegMap::iterator LRI = findLiveVirtReg(VirtReg);
  if (LRI != LiveVirtRegs.end())
    killVirtReg(LRI);
}

/// spillVirtReg - This method spills the value specified by VirtReg into the
/// corresponding stack slot if needed.
void RAFast::spillVirtReg(MachineBasicBlock::iterator MI, unsigned VirtReg) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Spilling a physical register is illegal!");
  LiveRegMap::iterator LRI = findLiveVirtReg(VirtReg);
  assert(LRI != LiveVirtRegs.end() && "Spilling unmapped virtual register");
  spillVirtReg(MI, LRI);
}

/// spillVirtReg - Do the actual work of spilling.
void RAFast::spillVirtReg(MachineBasicBlock::iterator MI,
                          LiveRegMap::iterator LRI) {
  LiveReg &LR = *LRI;
  assert(PhysRegState[LR.PhysReg] == LRI->VirtReg && "Broken RegState mapping");

  if (LR.Dirty) {
    // If this physreg is used by the instruction, we want to kill it on the
    // instruction, not on the spill.
    bool SpillKill = LR.LastUse != MI;
    LR.Dirty = false;
    DEBUG(dbgs() << "Spilling " << PrintReg(LRI->VirtReg, TRI)
                 << " in " << PrintReg(LR.PhysReg, TRI));
    const TargetRegisterClass *RC = MRI->getRegClass(LRI->VirtReg);
    int FI = getStackSpaceFor(LRI->VirtReg, RC);
    DEBUG(dbgs() << " to stack slot #" << FI << "\n");
    TII->storeRegToStackSlot(*MBB, MI, LR.PhysReg, SpillKill, FI, RC, TRI);
    ++NumStores;   // Update statistics

    // If this register is used by DBG_VALUE then insert new DBG_VALUE to
    // identify spilled location as the place to find corresponding variable's
    // value.
    SmallVector<MachineInstr *, 4> &LRIDbgValues =
      LiveDbgValueMap[LRI->VirtReg];
    for (unsigned li = 0, le = LRIDbgValues.size(); li != le; ++li) {
      MachineInstr *DBG = LRIDbgValues[li];
      const MDNode *MDPtr =
        DBG->getOperand(DBG->getNumOperands()-1).getMetadata();
      int64_t Offset = 0;
      if (DBG->getOperand(1).isImm())
        Offset = DBG->getOperand(1).getImm();
      DebugLoc DL;
      if (MI == MBB->end()) {
        // If MI is at basic block end then use last instruction's location.
        MachineBasicBlock::iterator EI = MI;
        DL = (--EI)->getDebugLoc();
      }
      else
        DL = MI->getDebugLoc();
      if (MachineInstr *NewDV =
          TII->emitFrameIndexDebugValue(*MF, FI, Offset, MDPtr, DL)) {
        MachineBasicBlock *MBB = DBG->getParent();
        MBB->insert(MI, NewDV);
        DEBUG(dbgs() << "Inserting debug info due to spill:" << "\n" << *NewDV);
      }
    }
    // Now this register is spilled there is should not be any DBG_VALUE
    // pointing to this register because they are all pointing to spilled value
    // now.
    LRIDbgValues.clear();
    if (SpillKill)
      LR.LastUse = 0; // Don't kill register again
  }
  killVirtReg(LRI);
}

/// spillAll - Spill all dirty virtregs without killing them.
void RAFast::spillAll(MachineBasicBlock::iterator MI) {
  if (LiveVirtRegs.empty()) return;
  isBulkSpilling = true;
  // The LiveRegMap is keyed by an unsigned (the virtreg number), so the order
  // of spilling here is deterministic, if arbitrary.
  for (LiveRegMap::iterator i = LiveVirtRegs.begin(), e = LiveVirtRegs.end();
       i != e; ++i)
    spillVirtReg(MI, i);
  LiveVirtRegs.clear();
  isBulkSpilling = false;
}

/// usePhysReg - Handle the direct use of a physical register.
/// Check that the register is not used by a virtreg.
/// Kill the physreg, marking it free.
/// This may add implicit kills to MO->getParent() and invalidate MO.
void RAFast::usePhysReg(MachineOperand &MO) {
  unsigned PhysReg = MO.getReg();
  assert(TargetRegisterInfo::isPhysicalRegister(PhysReg) &&
         "Bad usePhysReg operand");

  switch (PhysRegState[PhysReg]) {
  case regDisabled:
    break;
  case regReserved:
    PhysRegState[PhysReg] = regFree;
    // Fall through
  case regFree:
    UsedInInstr.insert(PhysReg);
    MO.setIsKill();
    return;
  default:
    // The physreg was allocated to a virtual register. That means the value we
    // wanted has been clobbered.
    llvm_unreachable("Instruction uses an allocated register");
  }

  // Maybe a superregister is reserved?
  for (MCRegAliasIterator AI(PhysReg, TRI, false); AI.isValid(); ++AI) {
    unsigned Alias = *AI;
    switch (PhysRegState[Alias]) {
    case regDisabled:
      break;
    case regReserved:
      assert(TRI->isSuperRegister(PhysReg, Alias) &&
             "Instruction is not using a subregister of a reserved register");
      // Leave the superregister in the working set.
      PhysRegState[Alias] = regFree;
      UsedInInstr.insert(Alias);
      MO.getParent()->addRegisterKilled(Alias, TRI, true);
      return;
    case regFree:
      if (TRI->isSuperRegister(PhysReg, Alias)) {
        // Leave the superregister in the working set.
        UsedInInstr.insert(Alias);
        MO.getParent()->addRegisterKilled(Alias, TRI, true);
        return;
      }
      // Some other alias was in the working set - clear it.
      PhysRegState[Alias] = regDisabled;
      break;
    default:
      llvm_unreachable("Instruction uses an alias of an allocated register");
    }
  }

  // All aliases are disabled, bring register into working set.
  PhysRegState[PhysReg] = regFree;
  UsedInInstr.insert(PhysReg);
  MO.setIsKill();
}

/// definePhysReg - Mark PhysReg as reserved or free after spilling any
/// virtregs. This is very similar to defineVirtReg except the physreg is
/// reserved instead of allocated.
void RAFast::definePhysReg(MachineInstr *MI, unsigned PhysReg,
                           RegState NewState) {
  UsedInInstr.insert(PhysReg);
  switch (unsigned VirtReg = PhysRegState[PhysReg]) {
  case regDisabled:
    break;
  default:
    spillVirtReg(MI, VirtReg);
    // Fall through.
  case regFree:
  case regReserved:
    PhysRegState[PhysReg] = NewState;
    return;
  }

  // This is a disabled register, disable all aliases.
  PhysRegState[PhysReg] = NewState;
  for (MCRegAliasIterator AI(PhysReg, TRI, false); AI.isValid(); ++AI) {
    unsigned Alias = *AI;
    switch (unsigned VirtReg = PhysRegState[Alias]) {
    case regDisabled:
      break;
    default:
      spillVirtReg(MI, VirtReg);
      // Fall through.
    case regFree:
    case regReserved:
      PhysRegState[Alias] = regDisabled;
      if (TRI->isSuperRegister(PhysReg, Alias))
        return;
      break;
    }
  }
}


// calcSpillCost - Return the cost of spilling clearing out PhysReg and
// aliases so it is free for allocation.
// Returns 0 when PhysReg is free or disabled with all aliases disabled - it
// can be allocated directly.
// Returns spillImpossible when PhysReg or an alias can't be spilled.
unsigned RAFast::calcSpillCost(unsigned PhysReg) const {
  if (UsedInInstr.count(PhysReg)) {
    DEBUG(dbgs() << PrintReg(PhysReg, TRI) << " is already used in instr.\n");
    return spillImpossible;
  }
  switch (unsigned VirtReg = PhysRegState[PhysReg]) {
  case regDisabled:
    break;
  case regFree:
    return 0;
  case regReserved:
    DEBUG(dbgs() << PrintReg(VirtReg, TRI) << " corresponding "
                 << PrintReg(PhysReg, TRI) << " is reserved already.\n");
    return spillImpossible;
  default: {
    LiveRegMap::const_iterator I = findLiveVirtReg(VirtReg);
    assert(I != LiveVirtRegs.end() && "Missing VirtReg entry");
    return I->Dirty ? spillDirty : spillClean;
  }
  }

  // This is a disabled register, add up cost of aliases.
  DEBUG(dbgs() << PrintReg(PhysReg, TRI) << " is disabled.\n");
  unsigned Cost = 0;
  for (MCRegAliasIterator AI(PhysReg, TRI, false); AI.isValid(); ++AI) {
    unsigned Alias = *AI;
    if (UsedInInstr.count(Alias))
      return spillImpossible;
    switch (unsigned VirtReg = PhysRegState[Alias]) {
    case regDisabled:
      break;
    case regFree:
      ++Cost;
      break;
    case regReserved:
      return spillImpossible;
    default: {
      LiveRegMap::const_iterator I = findLiveVirtReg(VirtReg);
      assert(I != LiveVirtRegs.end() && "Missing VirtReg entry");
      Cost += I->Dirty ? spillDirty : spillClean;
      break;
    }
    }
  }
  return Cost;
}


/// assignVirtToPhysReg - This method updates local state so that we know
/// that PhysReg is the proper container for VirtReg now.  The physical
/// register must not be used for anything else when this is called.
///
void RAFast::assignVirtToPhysReg(LiveReg &LR, unsigned PhysReg) {
  DEBUG(dbgs() << "Assigning " << PrintReg(LR.VirtReg, TRI) << " to "
               << PrintReg(PhysReg, TRI) << "\n");
  PhysRegState[PhysReg] = LR.VirtReg;
  assert(!LR.PhysReg && "Already assigned a physreg");
  LR.PhysReg = PhysReg;
}

RAFast::LiveRegMap::iterator
RAFast::assignVirtToPhysReg(unsigned VirtReg, unsigned PhysReg) {
  LiveRegMap::iterator LRI = findLiveVirtReg(VirtReg);
  assert(LRI != LiveVirtRegs.end() && "VirtReg disappeared");
  assignVirtToPhysReg(*LRI, PhysReg);
  return LRI;
}

/// allocVirtReg - Allocate a physical register for VirtReg.
RAFast::LiveRegMap::iterator RAFast::allocVirtReg(MachineInstr *MI,
                                                  LiveRegMap::iterator LRI,
                                                  unsigned Hint) {
  const unsigned VirtReg = LRI->VirtReg;

  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Can only allocate virtual registers");

  const TargetRegisterClass *RC = MRI->getRegClass(VirtReg);

  // Ignore invalid hints.
  if (Hint && (!TargetRegisterInfo::isPhysicalRegister(Hint) ||
               !RC->contains(Hint) || !MRI->isAllocatable(Hint)))
    Hint = 0;

  // Take hint when possible.
  if (Hint) {
    // Ignore the hint if we would have to spill a dirty register.
    unsigned Cost = calcSpillCost(Hint);
    if (Cost < spillDirty) {
      if (Cost)
        definePhysReg(MI, Hint, regFree);
      // definePhysReg may kill virtual registers and modify LiveVirtRegs.
      // That invalidates LRI, so run a new lookup for VirtReg.
      return assignVirtToPhysReg(VirtReg, Hint);
    }
  }

  ArrayRef<MCPhysReg> AO = RegClassInfo.getOrder(RC);

  // First try to find a completely free register.
  for (ArrayRef<MCPhysReg>::iterator I = AO.begin(), E = AO.end(); I != E; ++I){
    unsigned PhysReg = *I;
    if (PhysRegState[PhysReg] == regFree && !UsedInInstr.count(PhysReg)) {
      assignVirtToPhysReg(*LRI, PhysReg);
      return LRI;
    }
  }

  DEBUG(dbgs() << "Allocating " << PrintReg(VirtReg) << " from "
               << RC->getName() << "\n");

  unsigned BestReg = 0, BestCost = spillImpossible;
  for (ArrayRef<MCPhysReg>::iterator I = AO.begin(), E = AO.end(); I != E; ++I){
    unsigned Cost = calcSpillCost(*I);
    DEBUG(dbgs() << "\tRegister: " << PrintReg(*I, TRI) << "\n");
    DEBUG(dbgs() << "\tCost: " << Cost << "\n");
    DEBUG(dbgs() << "\tBestCost: " << BestCost << "\n");
    // Cost is 0 when all aliases are already disabled.
    if (Cost == 0) {
      assignVirtToPhysReg(*LRI, *I);
      return LRI;
    }
    if (Cost < BestCost)
      BestReg = *I, BestCost = Cost;
  }

  if (BestReg) {
    definePhysReg(MI, BestReg, regFree);
    // definePhysReg may kill virtual registers and modify LiveVirtRegs.
    // That invalidates LRI, so run a new lookup for VirtReg.
    return assignVirtToPhysReg(VirtReg, BestReg);
  }

  // Nothing we can do. Report an error and keep going with a bad allocation.
  MI->emitError("ran out of registers during register allocation");
  definePhysReg(MI, *AO.begin(), regFree);
  return assignVirtToPhysReg(VirtReg, *AO.begin());
}

/// defineVirtReg - Allocate a register for VirtReg and mark it as dirty.
RAFast::LiveRegMap::iterator
RAFast::defineVirtReg(MachineInstr *MI, unsigned OpNum,
                      unsigned VirtReg, unsigned Hint) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Not a virtual register");
  LiveRegMap::iterator LRI;
  bool New;
  tie(LRI, New) = LiveVirtRegs.insert(LiveReg(VirtReg));
  if (New) {
    // If there is no hint, peek at the only use of this register.
    if ((!Hint || !TargetRegisterInfo::isPhysicalRegister(Hint)) &&
        MRI->hasOneNonDBGUse(VirtReg)) {
      const MachineInstr &UseMI = *MRI->use_nodbg_begin(VirtReg);
      // It's a copy, use the destination register as a hint.
      if (UseMI.isCopyLike())
        Hint = UseMI.getOperand(0).getReg();
    }
    LRI = allocVirtReg(MI, LRI, Hint);
  } else if (LRI->LastUse) {
    // Redefining a live register - kill at the last use, unless it is this
    // instruction defining VirtReg multiple times.
    if (LRI->LastUse != MI || LRI->LastUse->getOperand(LRI->LastOpNum).isUse())
      addKillFlag(*LRI);
  }
  assert(LRI->PhysReg && "Register not assigned");
  LRI->LastUse = MI;
  LRI->LastOpNum = OpNum;
  LRI->Dirty = true;
  UsedInInstr.insert(LRI->PhysReg);
  return LRI;
}

/// reloadVirtReg - Make sure VirtReg is available in a physreg and return it.
RAFast::LiveRegMap::iterator
RAFast::reloadVirtReg(MachineInstr *MI, unsigned OpNum,
                      unsigned VirtReg, unsigned Hint) {
  assert(TargetRegisterInfo::isVirtualRegister(VirtReg) &&
         "Not a virtual register");
  LiveRegMap::iterator LRI;
  bool New;
  tie(LRI, New) = LiveVirtRegs.insert(LiveReg(VirtReg));
  MachineOperand &MO = MI->getOperand(OpNum);
  if (New) {
    LRI = allocVirtReg(MI, LRI, Hint);
    const TargetRegisterClass *RC = MRI->getRegClass(VirtReg);
    int FrameIndex = getStackSpaceFor(VirtReg, RC);
    DEBUG(dbgs() << "Reloading " << PrintReg(VirtReg, TRI) << " into "
                 << PrintReg(LRI->PhysReg, TRI) << "\n");
    TII->loadRegFromStackSlot(*MBB, MI, LRI->PhysReg, FrameIndex, RC, TRI);
    ++NumLoads;
  } else if (LRI->Dirty) {
    if (isLastUseOfLocalReg(MO)) {
      DEBUG(dbgs() << "Killing last use: " << MO << "\n");
      if (MO.isUse())
        MO.setIsKill();
      else
        MO.setIsDead();
    } else if (MO.isKill()) {
      DEBUG(dbgs() << "Clearing dubious kill: " << MO << "\n");
      MO.setIsKill(false);
    } else if (MO.isDead()) {
      DEBUG(dbgs() << "Clearing dubious dead: " << MO << "\n");
      MO.setIsDead(false);
    }
  } else if (MO.isKill()) {
    // We must remove kill flags from uses of reloaded registers because the
    // register would be killed immediately, and there might be a second use:
    //   %foo = OR %x<kill>, %x
    // This would cause a second reload of %x into a different register.
    DEBUG(dbgs() << "Clearing clean kill: " << MO << "\n");
    MO.setIsKill(false);
  } else if (MO.isDead()) {
    DEBUG(dbgs() << "Clearing clean dead: " << MO << "\n");
    MO.setIsDead(false);
  }
  assert(LRI->PhysReg && "Register not assigned");
  LRI->LastUse = MI;
  LRI->LastOpNum = OpNum;
  UsedInInstr.insert(LRI->PhysReg);
  return LRI;
}

// setPhysReg - Change operand OpNum in MI the refer the PhysReg, considering
// subregs. This may invalidate any operand pointers.
// Return true if the operand kills its register.
bool RAFast::setPhysReg(MachineInstr *MI, unsigned OpNum, unsigned PhysReg) {
  MachineOperand &MO = MI->getOperand(OpNum);
  bool Dead = MO.isDead();
  if (!MO.getSubReg()) {
    MO.setReg(PhysReg);
    return MO.isKill() || Dead;
  }

  // Handle subregister index.
  MO.setReg(PhysReg ? TRI->getSubReg(PhysReg, MO.getSubReg()) : 0);
  MO.setSubReg(0);

  // A kill flag implies killing the full register. Add corresponding super
  // register kill.
  if (MO.isKill()) {
    MI->addRegisterKilled(PhysReg, TRI, true);
    return true;
  }

  // A <def,read-undef> of a sub-register requires an implicit def of the full
  // register.
  if (MO.isDef() && MO.isUndef())
    MI->addRegisterDefined(PhysReg, TRI);

  return Dead;
}

// Handle special instruction operand like early clobbers and tied ops when
// there are additional physreg defines.
void RAFast::handleThroughOperands(MachineInstr *MI,
                                   SmallVectorImpl<unsigned> &VirtDead) {
  DEBUG(dbgs() << "Scanning for through registers:");
  SmallSet<unsigned, 8> ThroughRegs;
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg()) continue;
    unsigned Reg = MO.getReg();
    if (!TargetRegisterInfo::isVirtualRegister(Reg))
      continue;
    if (MO.isEarlyClobber() || MI->isRegTiedToDefOperand(i) ||
        (MO.getSubReg() && MI->readsVirtualRegister(Reg))) {
      if (ThroughRegs.insert(Reg))
        DEBUG(dbgs() << ' ' << PrintReg(Reg));
    }
  }

  // If any physreg defines collide with preallocated through registers,
  // we must spill and reallocate.
  DEBUG(dbgs() << "\nChecking for physdef collisions.\n");
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg() || !MO.isDef()) continue;
    unsigned Reg = MO.getReg();
    if (!Reg || !TargetRegisterInfo::isPhysicalRegister(Reg)) continue;
    for (MCRegAliasIterator AI(Reg, TRI, true); AI.isValid(); ++AI) {
      UsedInInstr.insert(*AI);
      if (ThroughRegs.count(PhysRegState[*AI]))
        definePhysReg(MI, *AI, regFree);
    }
  }

  SmallVector<unsigned, 8> PartialDefs;
  DEBUG(dbgs() << "Allocating tied uses.\n");
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg()) continue;
    unsigned Reg = MO.getReg();
    if (!TargetRegisterInfo::isVirtualRegister(Reg)) continue;
    if (MO.isUse()) {
      unsigned DefIdx = 0;
      if (!MI->isRegTiedToDefOperand(i, &DefIdx)) continue;
      DEBUG(dbgs() << "Operand " << i << "("<< MO << ") is tied to operand "
        << DefIdx << ".\n");
      LiveRegMap::iterator LRI = reloadVirtReg(MI, i, Reg, 0);
      unsigned PhysReg = LRI->PhysReg;
      setPhysReg(MI, i, PhysReg);
      // Note: we don't update the def operand yet. That would cause the normal
      // def-scan to attempt spilling.
    } else if (MO.getSubReg() && MI->readsVirtualRegister(Reg)) {
      DEBUG(dbgs() << "Partial redefine: " << MO << "\n");
      // Reload the register, but don't assign to the operand just yet.
      // That would confuse the later phys-def processing pass.
      LiveRegMap::iterator LRI = reloadVirtReg(MI, i, Reg, 0);
      PartialDefs.push_back(LRI->PhysReg);
    }
  }

  DEBUG(dbgs() << "Allocating early clobbers.\n");
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg()) continue;
    unsigned Reg = MO.getReg();
    if (!TargetRegisterInfo::isVirtualRegister(Reg)) continue;
    if (!MO.isEarlyClobber())
      continue;
    // Note: defineVirtReg may invalidate MO.
    LiveRegMap::iterator LRI = defineVirtReg(MI, i, Reg, 0);
    unsigned PhysReg = LRI->PhysReg;
    if (setPhysReg(MI, i, PhysReg))
      VirtDead.push_back(Reg);
  }

  // Restore UsedInInstr to a state usable for allocating normal virtual uses.
  UsedInInstr.clear();
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg() || (MO.isDef() && !MO.isEarlyClobber())) continue;
    unsigned Reg = MO.getReg();
    if (!Reg || !TargetRegisterInfo::isPhysicalRegister(Reg)) continue;
    DEBUG(dbgs() << "\tSetting " << PrintReg(Reg, TRI)
                 << " as used in instr\n");
    UsedInInstr.insert(Reg);
  }

  // Also mark PartialDefs as used to avoid reallocation.
  for (unsigned i = 0, e = PartialDefs.size(); i != e; ++i)
    UsedInInstr.insert(PartialDefs[i]);
}

/// addRetOperand - ensure that a return instruction has an operand for each
/// value live out of the function.
///
/// Things marked both call and return are tail calls; do not do this for them.
/// The tail callee need not take the same registers as input that it produces
/// as output, and there are dependencies for its input registers elsewhere.
///
/// FIXME: This should be done as part of instruction selection, and this helper
/// should be deleted. Until then, we use custom logic here to create the proper
/// operand under all circumstances. We can't use addRegisterKilled because that
/// doesn't make sense for undefined values. We can't simply avoid calling it
/// for undefined values, because we must ensure that the operand always exists.
void RAFast::addRetOperands(MachineBasicBlock *MBB) {
  if (MBB->empty() || !MBB->back().isReturn() || MBB->back().isCall())
    return;

  MachineInstr *MI = &MBB->back();

  for (MachineRegisterInfo::liveout_iterator
         I = MBB->getParent()->getRegInfo().liveout_begin(),
         E = MBB->getParent()->getRegInfo().liveout_end(); I != E; ++I) {
    unsigned Reg = *I;
    assert(TargetRegisterInfo::isPhysicalRegister(Reg) &&
           "Cannot have a live-out virtual register.");

    bool hasDef = PhysRegState[Reg] == regReserved;

    // Check if this register already has an operand.
    bool Found = false;
    for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg() || !MO.isUse())
        continue;

      unsigned OperReg = MO.getReg();
      if (!TargetRegisterInfo::isPhysicalRegister(OperReg))
        continue;

      if (OperReg == Reg || TRI->isSuperRegister(OperReg, Reg)) {
        // If the ret already has an operand for this physreg or a superset,
        // don't duplicate it. Set the kill flag if the value is defined.
        if (hasDef && !MO.isKill())
          MO.setIsKill();
        Found = true;
        break;
      }
    }
    if (!Found)
      MachineInstrBuilder(*MF, MI)
        .addReg(Reg, llvm::RegState::Implicit | getKillRegState(hasDef));
  }
}

void RAFast::AllocateBasicBlock() {
  DEBUG(dbgs() << "\nAllocating " << *MBB);

  PhysRegState.assign(TRI->getNumRegs(), regDisabled);
  assert(LiveVirtRegs.empty() && "Mapping not cleared from last block?");

  MachineBasicBlock::iterator MII = MBB->begin();

  // Add live-in registers as live.
  for (MachineBasicBlock::livein_iterator I = MBB->livein_begin(),
         E = MBB->livein_end(); I != E; ++I)
    if (MRI->isAllocatable(*I))
      definePhysReg(MII, *I, regReserved);

  SmallVector<unsigned, 8> VirtDead;
  SmallVector<MachineInstr*, 32> Coalesced;

  // Otherwise, sequentially allocate each instruction in the MBB.
  while (MII != MBB->end()) {
    MachineInstr *MI = MII++;
    const MCInstrDesc &MCID = MI->getDesc();
    DEBUG({
        dbgs() << "\n>> " << *MI << "Regs:";
        for (unsigned Reg = 1, E = TRI->getNumRegs(); Reg != E; ++Reg) {
          if (PhysRegState[Reg] == regDisabled) continue;
          dbgs() << " " << TRI->getName(Reg);
          switch(PhysRegState[Reg]) {
          case regFree:
            break;
          case regReserved:
            dbgs() << "*";
            break;
          default: {
            dbgs() << '=' << PrintReg(PhysRegState[Reg]);
            LiveRegMap::iterator I = findLiveVirtReg(PhysRegState[Reg]);
            assert(I != LiveVirtRegs.end() && "Missing VirtReg entry");
            if (I->Dirty)
              dbgs() << "*";
            assert(I->PhysReg == Reg && "Bad inverse map");
            break;
          }
          }
        }
        dbgs() << '\n';
        // Check that LiveVirtRegs is the inverse.
        for (LiveRegMap::iterator i = LiveVirtRegs.begin(),
             e = LiveVirtRegs.end(); i != e; ++i) {
           assert(TargetRegisterInfo::isVirtualRegister(i->VirtReg) &&
                  "Bad map key");
           assert(TargetRegisterInfo::isPhysicalRegister(i->PhysReg) &&
                  "Bad map value");
           assert(PhysRegState[i->PhysReg] == i->VirtReg && "Bad inverse map");
        }
      });

    // Debug values are not allowed to change codegen in any way.
    if (MI->isDebugValue()) {
      bool ScanDbgValue = true;
      while (ScanDbgValue) {
        ScanDbgValue = false;
        for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
          MachineOperand &MO = MI->getOperand(i);
          if (!MO.isReg()) continue;
          unsigned Reg = MO.getReg();
          if (!TargetRegisterInfo::isVirtualRegister(Reg)) continue;
          LiveRegMap::iterator LRI = findLiveVirtReg(Reg);
          if (LRI != LiveVirtRegs.end())
            setPhysReg(MI, i, LRI->PhysReg);
          else {
            int SS = StackSlotForVirtReg[Reg];
            if (SS == -1) {
              // We can't allocate a physreg for a DebugValue, sorry!
              DEBUG(dbgs() << "Unable to allocate vreg used by DBG_VALUE");
              MO.setReg(0);
            }
            else {
              // Modify DBG_VALUE now that the value is in a spill slot.
              int64_t Offset = MI->getOperand(1).getImm();
              const MDNode *MDPtr =
                MI->getOperand(MI->getNumOperands()-1).getMetadata();
              DebugLoc DL = MI->getDebugLoc();
              if (MachineInstr *NewDV =
                  TII->emitFrameIndexDebugValue(*MF, SS, Offset, MDPtr, DL)) {
                DEBUG(dbgs() << "Modifying debug info due to spill:" <<
                      "\t" << *MI);
                MachineBasicBlock *MBB = MI->getParent();
                MBB->insert(MBB->erase(MI), NewDV);
                // Scan NewDV operands from the beginning.
                MI = NewDV;
                ScanDbgValue = true;
                break;
              } else {
                // We can't allocate a physreg for a DebugValue; sorry!
                DEBUG(dbgs() << "Unable to allocate vreg used by DBG_VALUE");
                MO.setReg(0);
              }
            }
          }
          LiveDbgValueMap[Reg].push_back(MI);
        }
      }
      // Next instruction.
      continue;
    }

    // If this is a copy, we may be able to coalesce.
    unsigned CopySrc = 0, CopyDst = 0, CopySrcSub = 0, CopyDstSub = 0;
    if (MI->isCopy()) {
      CopyDst = MI->getOperand(0).getReg();
      CopySrc = MI->getOperand(1).getReg();
      CopyDstSub = MI->getOperand(0).getSubReg();
      CopySrcSub = MI->getOperand(1).getSubReg();
    }

    // Track registers used by instruction.
    UsedInInstr.clear();

    // First scan.
    // Mark physreg uses and early clobbers as used.
    // Find the end of the virtreg operands
    unsigned VirtOpEnd = 0;
    bool hasTiedOps = false;
    bool hasEarlyClobbers = false;
    bool hasPartialRedefs = false;
    bool hasPhysDefs = false;
    for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      // Make sure MRI knows about registers clobbered by regmasks.
      if (MO.isRegMask()) {
        MRI->addPhysRegsUsedFromRegMask(MO.getRegMask());
        continue;
      }
      if (!MO.isReg()) continue;
      unsigned Reg = MO.getReg();
      if (!Reg) continue;
      if (TargetRegisterInfo::isVirtualRegister(Reg)) {
        VirtOpEnd = i+1;
        if (MO.isUse()) {
          hasTiedOps = hasTiedOps ||
                              MCID.getOperandConstraint(i, MCOI::TIED_TO) != -1;
        } else {
          if (MO.isEarlyClobber())
            hasEarlyClobbers = true;
          if (MO.getSubReg() && MI->readsVirtualRegister(Reg))
            hasPartialRedefs = true;
        }
        continue;
      }
      if (!MRI->isAllocatable(Reg)) continue;
      if (MO.isUse()) {
        usePhysReg(MO);
      } else if (MO.isEarlyClobber()) {
        definePhysReg(MI, Reg, (MO.isImplicit() || MO.isDead()) ?
                               regFree : regReserved);
        hasEarlyClobbers = true;
      } else
        hasPhysDefs = true;
    }

    // The instruction may have virtual register operands that must be allocated
    // the same register at use-time and def-time: early clobbers and tied
    // operands. If there are also physical defs, these registers must avoid
    // both physical defs and uses, making them more constrained than normal
    // operands.
    // Similarly, if there are multiple defs and tied operands, we must make
    // sure the same register is allocated to uses and defs.
    // We didn't detect inline asm tied operands above, so just make this extra
    // pass for all inline asm.
    if (MI->isInlineAsm() || hasEarlyClobbers || hasPartialRedefs ||
        (hasTiedOps && (hasPhysDefs || MCID.getNumDefs() > 1))) {
      handleThroughOperands(MI, VirtDead);
      // Don't attempt coalescing when we have funny stuff going on.
      CopyDst = 0;
      // Pretend we have early clobbers so the use operands get marked below.
      // This is not necessary for the common case of a single tied use.
      hasEarlyClobbers = true;
    }

    // Second scan.
    // Allocate virtreg uses.
    for (unsigned i = 0; i != VirtOpEnd; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg()) continue;
      unsigned Reg = MO.getReg();
      if (!TargetRegisterInfo::isVirtualRegister(Reg)) continue;
      if (MO.isUse()) {
        LiveRegMap::iterator LRI = reloadVirtReg(MI, i, Reg, CopyDst);
        unsigned PhysReg = LRI->PhysReg;
        CopySrc = (CopySrc == Reg || CopySrc == PhysReg) ? PhysReg : 0;
        if (setPhysReg(MI, i, PhysReg))
          killVirtReg(LRI);
      }
    }

    for (UsedInInstrSet::iterator
         I = UsedInInstr.begin(), E = UsedInInstr.end(); I != E; ++I)
      MRI->setPhysRegUsed(*I);

    // Track registers defined by instruction - early clobbers and tied uses at
    // this point.
    UsedInInstr.clear();
    if (hasEarlyClobbers) {
      for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
        MachineOperand &MO = MI->getOperand(i);
        if (!MO.isReg()) continue;
        unsigned Reg = MO.getReg();
        if (!Reg || !TargetRegisterInfo::isPhysicalRegister(Reg)) continue;
        // Look for physreg defs and tied uses.
        if (!MO.isDef() && !MI->isRegTiedToDefOperand(i)) continue;
        for (MCRegAliasIterator AI(Reg, TRI, true); AI.isValid(); ++AI)
          UsedInInstr.insert(*AI);
      }
    }

    unsigned DefOpEnd = MI->getNumOperands();
    if (MI->isCall()) {
      // Spill all virtregs before a call. This serves two purposes: 1. If an
      // exception is thrown, the landing pad is going to expect to find
      // registers in their spill slots, and 2. we don't have to wade through
      // all the <imp-def> operands on the call instruction.
      DefOpEnd = VirtOpEnd;
      DEBUG(dbgs() << "  Spilling remaining registers before call.\n");
      spillAll(MI);

      // The imp-defs are skipped below, but we still need to mark those
      // registers as used by the function.
      SkippedInstrs.insert(&MCID);
    }

    // Third scan.
    // Allocate defs and collect dead defs.
    for (unsigned i = 0; i != DefOpEnd; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg() || !MO.isDef() || !MO.getReg() || MO.isEarlyClobber())
        continue;
      unsigned Reg = MO.getReg();

      if (TargetRegisterInfo::isPhysicalRegister(Reg)) {
        if (!MRI->isAllocatable(Reg)) continue;
        definePhysReg(MI, Reg, (MO.isImplicit() || MO.isDead()) ?
                               regFree : regReserved);
        continue;
      }
      LiveRegMap::iterator LRI = defineVirtReg(MI, i, Reg, CopySrc);
      unsigned PhysReg = LRI->PhysReg;
      if (setPhysReg(MI, i, PhysReg)) {
        VirtDead.push_back(Reg);
        CopyDst = 0; // cancel coalescing;
      } else
        CopyDst = (CopyDst == Reg || CopyDst == PhysReg) ? PhysReg : 0;
    }

    // Kill dead defs after the scan to ensure that multiple defs of the same
    // register are allocated identically. We didn't need to do this for uses
    // because we are crerating our own kill flags, and they are always at the
    // last use.
    for (unsigned i = 0, e = VirtDead.size(); i != e; ++i)
      killVirtReg(VirtDead[i]);
    VirtDead.clear();

    for (UsedInInstrSet::iterator
         I = UsedInInstr.begin(), E = UsedInInstr.end(); I != E; ++I)
      MRI->setPhysRegUsed(*I);

    if (CopyDst && CopyDst == CopySrc && CopyDstSub == CopySrcSub) {
      DEBUG(dbgs() << "-- coalescing: " << *MI);
      Coalesced.push_back(MI);
    } else {
      DEBUG(dbgs() << "<< " << *MI);
    }
  }

  // Spill all physical registers holding virtual registers now.
  DEBUG(dbgs() << "Spilling live registers at end of block.\n");
  spillAll(MBB->getFirstTerminator());

  // Erase all the coalesced copies. We are delaying it until now because
  // LiveVirtRegs might refer to the instrs.
  for (unsigned i = 0, e = Coalesced.size(); i != e; ++i)
    MBB->erase(Coalesced[i]);
  NumCopies += Coalesced.size();

  // addRetOperands must run after we've seen all defs in this block.
  addRetOperands(MBB);

  DEBUG(MBB->dump());
}

/// runOnMachineFunction - Register allocate the whole function
///
bool RAFast::runOnMachineFunction(MachineFunction &Fn) {
  DEBUG(dbgs() << "********** FAST REGISTER ALLOCATION **********\n"
               << "********** Function: " << Fn.getName() << '\n');
  MF = &Fn;
  MRI = &MF->getRegInfo();
  TM = &Fn.getTarget();
  TRI = TM->getRegisterInfo();
  TII = TM->getInstrInfo();
  MRI->freezeReservedRegs(Fn);
  RegClassInfo.runOnMachineFunction(Fn);
  UsedInInstr.clear();
  UsedInInstr.setUniverse(TRI->getNumRegs());

  assert(!MRI->isSSA() && "regalloc requires leaving SSA");

  // initialize the virtual->physical register map to have a 'null'
  // mapping for all virtual registers
  StackSlotForVirtReg.resize(MRI->getNumVirtRegs());
  LiveVirtRegs.setUniverse(MRI->getNumVirtRegs());

  // Loop over all of the basic blocks, eliminating virtual register references
  for (MachineFunction::iterator MBBi = Fn.begin(), MBBe = Fn.end();
       MBBi != MBBe; ++MBBi) {
    MBB = &*MBBi;
    AllocateBasicBlock();
  }

  // Add the clobber lists for all the instructions we skipped earlier.
  for (SmallPtrSet<const MCInstrDesc*, 4>::const_iterator
       I = SkippedInstrs.begin(), E = SkippedInstrs.end(); I != E; ++I)
    if (const uint16_t *Defs = (*I)->getImplicitDefs())
      while (*Defs)
        MRI->setPhysRegUsed(*Defs++);

  // All machine operands and other references to virtual registers have been
  // replaced. Remove the virtual registers.
  MRI->clearVirtRegs();

  SkippedInstrs.clear();
  StackSlotForVirtReg.clear();
  LiveDbgValueMap.clear();
  return true;
}

FunctionPass *llvm::createFastRegisterAllocator() {
  return new RAFast();
}
