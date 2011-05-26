//===- LunarGLASSManager.h - Public interface to using LunarGLASS ----========//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2011 LunarG, Inc.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice (including the next
// paragraph) shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//
//===----------------------------------------------------------------------===//
//
// Author: John Kessenich, LunarG
//
// Public interface to LunarGLASS.
//
// Don't include Mesa or other user's headers here.  LunarGLASS is not 
// Mesa-dependent nor dependent on other uses of it.
//
// Don't include LLVM headers here.  At the highest level use of LunarGLASS,
// LLVM headers are neither needed nor desired.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef LunarGLASSManager_H
#define LunarGLASSManager_H

namespace llvm {
    class Module;
    class Value;
};

namespace gla {

    // Abstract class of external manager of translations within LunarGLASS.
    // Use getManager() to get a concrete manager, which should be derived 
    // from gla::PrivateManager.
    class Manager {
    public:
        virtual ~Manager() { }

        virtual void setModule(llvm::Module* m) { module = m; }
        virtual void translateTopToBottom() = 0;
        virtual void translateBottomToTarget() = 0;

    protected:
        Manager() : module(0) { }

        llvm::Module* module;
    };

    Manager* getManager();
};

#endif /* LunarGLASSManager_H */