cmake_minimum_required(VERSION 2.8)

set(CMAKE_INSTALL_PREFIX "install" CACHE STRING "prefix" FORCE)

project(LunarGLASS)

set(LLVM_BUILD ${CMAKE_CURRENT_SOURCE_DIR}/Core/LLVM/llvm-3.4/build CACHE PATH "LLVM build directory")

set(GLSLANG_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../glslang CACHE PATH "glslang directory")

if(WIN32)
    include(Core/LLVM/llvm-3.4/cmake/modules/ChooseMSVCCRT.cmake)
    foreach(build_type ${CMAKE_CONFIGURATION_TYPES} ${CMAKE_BUILD_TYPE})
        string(TOUPPER "${build_type}" build)
        message("CMAKE_CXX_FLAGS_${build} are ${CMAKE_CXX_FLAGS_${build}}")
    endforeach(build_type)

    set(LLVMINCLUDES ${LLVM_BUILD}/install/include CACHE PATH "LLVM include directory")
    set(LLVMLIBS ${LLVM_BUILD}/install/lib CACHE PATH "LLVM library directory")
    set(GLSLANGLIBS ${GLSLANG_PATH}/build/install/lib/ CACHE PATH "glslang library directory")
    link_directories(${GLSLANGLIBS} ${LLVMLIBS})
elseif(UNIX)
    set(LLVMINCLUDES ${LLVM_BUILD}/install/usr/local/include CACHE PATH "LLVM include directory")
    set(LLVMLIBS ${LLVM_BUILD}/install/usr/local/lib CACHE PATH "LLVM library directory") 
    set(GLSLANGLIBS ${GLSLANG_PATH}/build/install/lib/ CACHE PATH "glslang library directory")
    link_directories(${GLSLANGLIBS} ${LLVMLIBS})
else(WIN32)
    message("unknown platform")
endif(WIN32)

set(GLSLANGINCLUDES ${GLSLANG_PATH} CACHE PATH "glslang include directory")

set(LLVMLIBS
    LLVMAsmParser
    LLVMLinker
    LLVMipo
    LLVMScalarOpts
    LLVMInstCombine
    LLVMTransformUtils
    LLVMipa
    LLVMAnalysis
    LLVMTarget
    LLVMCore
    LLVMSupport)

if(WIN32)
    set(SYSTEM_LIBS "")
elseif(UNIX)
    # Get system library dependencies from llvm-config.
    execute_process(COMMAND ${LLVM_BUILD}/install/usr/local/bin/llvm-config --ldflags OUTPUT_VARIABLE LLVM_LD_FLAGS)
    # Convert to a list of link flags.
    string(REPLACE " " ";" LLVM_LD_FLAGS_LIST ${LLVM_LD_FLAGS})
    foreach(LIB ${LLVM_LD_FLAGS_LIST})
      if (${LIB} MATCHES "^-l")
        # Get the library's name.
        string(SUBSTRING ${LIB} 2 -1 LIBNAME)
        list(APPEND SYSTEM_LIBS ${LIBNAME})
      endif()
    endforeach()
endif(WIN32)

set(GLSLANGLIBS
    glslang
    OSDependent
    OGLCompiler
    SPIRV)

if(WIN32)
    add_definitions(-D_SCL_SECURE_NO_WARNINGS)
elseif(UNIX)
    execute_process(COMMAND ${LLVM_BUILD}/install/usr/local/bin/llvm-config --cxxflags OUTPUT_VARIABLE LLVM_CXX_FLAGS)
    add_definitions(${LLVM_CXX_FLAGS})
endif(WIN32)

add_subdirectory(Frontends)
add_subdirectory(Core)
add_subdirectory(Backends)
add_subdirectory(Standalone)
