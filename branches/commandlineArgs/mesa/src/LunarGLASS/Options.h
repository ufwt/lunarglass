//===- Options.h - Global run-time options --------------------------------===//
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
// Global run-time options
//
//===----------------------------------------------------------------------===//

#include <iostream>
#include <vector>
#include <cstdio>
#include <cstdlib>


namespace gla {
    enum Backend { GLSL      // GLSL backed
                 , TGSI      // TGSI backend
                 };

  // Anonymous Options struct
    struct OptionsType {
        bool dumpAst;
        bool obfuscate;
        Backend backend;
    };

    // Handle the commandline arguments to the program
    // Returns the index of the first non-flag argument
    // Assumes that all option/flagged arguments come before non-flagged arguments
    int handleArgs(int, char**);

    // Print out description and help
    void printHelp();

    extern OptionsType Options;
}

