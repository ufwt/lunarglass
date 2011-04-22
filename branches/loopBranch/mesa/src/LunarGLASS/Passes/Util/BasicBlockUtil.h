//===- BasicBlockUtil.h - Utility functions for basic blocks --------------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2011 LunarG, Inc.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; version 2 of the
// License.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301, USA.
//
//===----------------------------------------------------------------------===//
//
// Author: Michael Ilseman, LunarG
//
// Provides utility functions for BasicBlocks
//
//===----------------------------------------------------------------------===//

#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Support/CFG.h"

namespace llvm {

    // Whether a basic block has no constituent instructions, other than
    // it's phi-nodes and terminator.
    inline bool IsEmptyBB(const BasicBlock* bb)
    {
        return bb->getFirstNonPHIOrDbg() == bb->getTerminator();
    }

    inline bool AreEmptyBB(SmallVectorImpl<const BasicBlock*>& bbs)
    {
        for (SmallVectorImpl<const BasicBlock*>::iterator i = bbs.begin(), e = bbs.end(); i != e; ++i)
            if (! IsEmptyBB(*i)) {
                errs() << "\n================================================================================\n" << **i << "\n================================================================================\n";
                return false;
            }

        return true;
    }

    // Whether from unconditionaly branches to to.
    inline bool UncondBranchesTo(BasicBlock* from, BasicBlock* to)
    {
        BranchInst* bi = dyn_cast<BranchInst>(from->getTerminator());
        return bi && bi->isUnconditional() && (bi->getSuccessor(0) == to);
    }

    // Whether the block terminates in a conditional branch
    inline bool IsConditional(const BasicBlock* bb)
    {
        if (const BranchInst* bi = dyn_cast<BranchInst>(bb->getTerminator()))
            return bi->isConditional();

        return false;
    }

    // Whether the block terminates in a unconditional branch
    inline bool IsUnconditional(const BasicBlock* bb)
    {
        return !IsConditional(bb);
    }

    // Whether block A is a predecessor of B
    inline bool IsPredecessor(const BasicBlock* pred, const BasicBlock* succ)
    {
        for (const_pred_iterator i = pred_begin(succ), e = pred_end(succ); i != e; ++i)
            if (*i == pred)
                return true;

        return false;
    }

    // Collect all the phi nodes in bb into a phis
    template<unsigned Size>
    inline void getPHINodes(const BasicBlock* bb, SmallPtrSet<const PHINode*, Size>& phis)
    {
        for (BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i)
            if (const PHINode* pn = dyn_cast<PHINode>(i))
                phis.insert(pn);
    }

    // A is an indirect predecessor of B if A branches to some block that
    // unconditionally branches to B.
    inline bool IsIndirectPredecessor(const BasicBlock* pred, const BasicBlock* succ)
    {
        for (const_pred_iterator i = pred_begin(succ), e = pred_end(succ); i != e; ++i)
            if (IsUnconditional(*i))
                if (IsPredecessor(pred, *i))
                    return true;

        return false;
    }

    // If the passed block has no predecessors, remove it, updating and
    // unlinking everything that needs to be updated/unlinked. Will not remove
    // the entry block
    inline bool RemoveNoPredecessorBlock(BasicBlock* bb)
    {
        if (&*bb->getParent()->begin() == bb || pred_begin(bb) != pred_end(bb))
            return false;

        for (succ_iterator sI = succ_begin(bb), sE = succ_end(bb); sI != sE; ++sI) {
            (*sI)->removePredecessor(bb);
        }
        bb->dropAllReferences();
        bb->getParent()->getBasicBlockList().erase(bb);

        return true;
    }

    // Gather up all the children of the passed basic block that are dominated
    // by it.
    inline void GetDominatedChildren(const DominatorTree* dt, const BasicBlock* bb, SmallVectorImpl<const BasicBlock*>& children)
    {
        for (df_iterator<const BasicBlock*> i = df_begin(bb), e = df_end(bb); i != e; ++i) {
            if (! dt->dominates(bb, *i))
                continue;

            children.push_back(*i);
        }
    }

    // Return the single merge point of the given basic blocks.  Returns null if
    // there is no merge point, or if there are more than 1 merge points.  Note
    // that the presense of backedges or exitedges may cause there to be
    // multiple potential merge points.
    BasicBlock* GetSingleMergePoint(SmallVectorImpl<BasicBlock*>&, DominanceFrontier&);


} // end namespace llvm