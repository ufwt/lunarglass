//===- IdentifyCondtionals.cpp - Identify the structural conditionals -----===//
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
//===----------------------------------------------------------------------===//
//
// Identify the structural conditionals, and collect information about them,
// including their classification (e.g. if they're an if-then-else), and their
// merge points.
//
//===----------------------------------------------------------------------===//

#include "IdentifyConditionals.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Support/raw_ostream.h"

#include "Passes/Util/BasicBlockUtil.h"

using namespace llvm;

bool IdentifyConditionals::runOnFunction(Function &F)
{
    DominanceFrontier& domFront = getAnalysis<DominanceFrontier>();
    DominatorTree& domTree      = getAnalysis<DominatorTree>();

    for (Function::iterator bb = F.begin(), e = F.end(); bb != e; ++bb) {

        // First, exclude all the non conditional branches
        const BranchInst* branchInst = dyn_cast<BranchInst>(bb->getTerminator());
        if (!branchInst)
            continue;
        if (branchInst->isUnconditional())
            continue;

        BasicBlock* left  = branchInst->getSuccessor(0);
        BasicBlock* right = branchInst->getSuccessor(1);

        // If there's not a single merge point exclude this bb
        SmallVector<BasicBlock*, 2> points;
        points.push_back(left);
        points.push_back(right);


        BasicBlock* merge = GetSingleMergePoint(points, domFront);
        if (!merge)
            continue;


        std::pair<const BasicBlock*, const Conditional*> pair(bb, new Conditional(bb, merge, left, right,
                                                                                  &domFront, &domTree));

        conditionals.insert(pair);
    }

    return false;
}


void IdentifyConditionals::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominanceFrontier>();
    AU.addRequired<DominatorTree>();
    AU.setPreservesAll();
    return;
}

void IdentifyConditionals::print(raw_ostream&, const Module*) const
{
    return;
}

void IdentifyConditionals::releaseMemory()
{
    for (DenseMap<const BasicBlock*, const Conditional*>::iterator i = conditionals.begin(), e = conditionals.end(); i != e; ++i) {
        delete i->second;
    }
    conditionals.clear();
}

bool Conditional::isEmptyConditional() const
{
    if (!isSelfContained())
        return false;

    SmallVector<const BasicBlock*, 16> leftGraph;
    SmallVector<const BasicBlock*, 16> rightGraph;

    GetDominatedChildren(domTree, left, leftGraph);
    GetDominatedChildren(domTree, right, rightGraph);

    leftGraph.push_back(left);
    rightGraph.push_back(right);

    bool isLeftEmpty  = (left  == merge) || AreEmptyBB(leftGraph);
    bool isRightEmpty = (right == merge) || AreEmptyBB(rightGraph);

    return isLeftEmpty && isRightEmpty;
}

bool Conditional::isSelfContained() const
{
    DominanceFrontier::DomSetType leftDomFront  = domFront->find(left)->second;
    DominanceFrontier::DomSetType rightDomFront = domFront->find(right)->second;

    bool leftPure  = (left  == merge) || (leftDomFront.count(merge)  && leftDomFront.size() == 1);
    bool rightPure = (right == merge) || (rightDomFront.count(merge) && rightDomFront.size() == 1);

    return leftPure && rightPure;

}

char IdentifyConditionals::ID = 0;
INITIALIZE_PASS(IdentifyConditionals,
                "identify-conditionals",
                "Identify the conditional expressions",
                true,  // Whether it preserves the CFG
                true); // Whether it is an analysis pass