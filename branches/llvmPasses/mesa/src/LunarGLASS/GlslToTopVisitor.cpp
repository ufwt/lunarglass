//===- GlslToTopVisitor.cpp - Help Translate GLSL IR to LunarGLASS Top IR -===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright � 2011, LunarG, Inc.
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
// Author: Cody Northrop, LunarG
//
//===----------------------------------------------------------------------===//

#include "GlslToTopVisitor.h"
#include "LunarGLASSTopIR.h"
#include "mtypes.h"
#include "Exceptions.h"
#include "Options.h"

void GlslToTop(struct gl_shader* glShader, llvm::Module* module)
{
    GlslToTopVisitor v(glShader, module);

    foreach_iter(exec_list_iterator, iter, *glShader->ir) {
        ir_instruction *ir = (ir_instruction *)iter.get();

        ir->accept(&v);
    }
}

GlslToTopVisitor::GlslToTopVisitor(struct gl_shader* s, llvm::Module* m)
    : context(llvm::getGlobalContext()), builder(context), module(m), glShader(s), interpIndex(0), shaderEntry(0)
{
}

GlslToTopVisitor::~GlslToTopVisitor()
{
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_variable *variable)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_constant *constant)
{
    lastValue = createLLVMConstant(constant);

    return visit_continue;
}

llvm::Value* GlslToTopVisitor::createLLVMConstant(ir_constant* constant)
{
    // XXXX Ints are 32-bit, bools are 1-bit
    llvm::Constant* llvmConstant;

    unsigned vecCount = constant->type->vector_elements;
    unsigned baseType = constant->type->base_type;

    if(vecCount > 1) {
        //Vectors require
        std::vector<llvm::Constant*> vals;
        const llvm::VectorType *destVecTy;

        switch(baseType)
        {
        case GLSL_TYPE_UINT:
            destVecTy = llvm::VectorType::get((llvm::Type*)llvm::Type::getInt32Ty(context), vecCount);
            for(unsigned int i = 0; i < vecCount; ++i)
                vals.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.u[i], false)));
            break;
        case GLSL_TYPE_INT:
            destVecTy = llvm::VectorType::get((llvm::Type*)llvm::Type::getInt32Ty(context), vecCount);
            for(unsigned int i = 0; i < vecCount; ++i)
                vals.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.i[i], true)));
            break;
        case GLSL_TYPE_FLOAT:
            destVecTy = llvm::VectorType::get((llvm::Type*)llvm::Type::getFloatTy(context), vecCount);
            for(unsigned int i = 0; i < vecCount; ++i)
                vals.push_back(llvm::ConstantFP::get(context, llvm::APFloat(constant->value.f[i])));
            break;
        case GLSL_TYPE_BOOL:
            destVecTy = llvm::VectorType::get((llvm::Type*)llvm::Type::getInt1Ty(context), vecCount);
            for(unsigned int i = 0; i < vecCount; ++i)
                vals.push_back(llvm::ConstantInt::get(context, llvm::APInt(1, (uint64_t)constant->value.u[i], false)));
            break;
        case GLSL_TYPE_SAMPLER:
        case GLSL_TYPE_ARRAY:
        case GLSL_TYPE_STRUCT:
        case GLSL_TYPE_VOID:
        case GLSL_TYPE_ERROR:
        default:
            gla::UnsupportedFunctionality("Basic vector type: ", baseType);
            break;
        }

        llvmConstant = llvm::ConstantVector::get(destVecTy, vals);
    }
    else {
        switch(baseType)
        {
        case GLSL_TYPE_UINT:
            llvmConstant = llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.u[0], false));
            break;
        case GLSL_TYPE_INT:
            llvmConstant = llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.i[0], true));
            break;
        case GLSL_TYPE_FLOAT:
            llvmConstant = llvm::ConstantFP::get(context, llvm::APFloat(constant->value.f[0]));
            break;
        case GLSL_TYPE_BOOL:
           llvmConstant = llvm::ConstantInt::get(context, llvm::APInt(1, (uint64_t)constant->value.u[0], false));
            break;
        case GLSL_TYPE_SAMPLER:
        case GLSL_TYPE_ARRAY:
        case GLSL_TYPE_STRUCT:
        case GLSL_TYPE_VOID:
        case GLSL_TYPE_ERROR:
        default:
            gla::UnsupportedFunctionality("Basic type: ", baseType);
            break;
        }
    }

    return llvmConstant;
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_loop_jump *ir)
{
    gla::UnsupportedFunctionality("Loops (loop jump found)");

    return visit_continue;
}

int GlslToTopVisitor::getNextInterpIndex(ir_variable* var)
{
    // Get the index for this interpolant, or create a new unique one
    std::map<ir_variable*, int>::iterator iter;
    iter = interpMap.find(var);

    if (interpMap.end() == iter) {
        interpMap[var] = interpIndex++;
    }

    return interpMap[var];
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_dereference_variable *derefVariable)
{
    // Grab the actual variable
    ir_variable *var = derefVariable->variable_referenced();

    // for pipeline reads, we don't create an LLVM variable storage
    if (var->mode != ir_var_in) {

        // Search our value map for existing entry
        std::map<ir_variable*, llvm::Value*>::iterator iter;
        iter = namedValues.find(var);

        if (namedValues.end() == iter) {

            // it was not found, create it
            namedValues[var] = createLLVMVariable(var);

            // For pipeline outputs, we must still maintain a non-pipeline
            // variable for reading/writing that happens before the final
            // copy out.  Make this current variable be that non-pipeline
            // normal variable, but track it as one that now needs a copy out on
            // shader exit.
            if (var->mode == ir_var_out) {
                // Track our copy-out for pipe write
                glslOuts.push_back(namedValues[var]);
            }
        }

        // Track the current value
        lastValue = namedValues[var];
    }

    if (in_assignee)
    {
        // Track the dest so we can Store later
        lValue = lastValue;
    }
    else
    {
        if (var->mode == ir_var_in) {
            // For pipeline inputs, and we will generate a fresh pipeline read at each reference,
            // which we will optimize later.
            llvm::Function *intrinsicName = 0;
            const char *name = NULL;

            // Give each interpolant a temporary unique index
            int paramCount = 0;
            llvm::Constant *interpLoc    = llvm::ConstantInt::get(context, llvm::APInt(32, getNextInterpIndex(var), true));
            llvm::Constant *interpOffset = llvm::ConstantInt::get(context, llvm::APInt(32, 0, true));

            // Select intrinsic based on target stage
            if(glShader->Type == GL_FRAGMENT_SHADER) {
                llvm::Intrinsic::ID intrinsicID;
                llvm::Type* readType = convertGLSLToLLVMType(var->type);
                switch(getLLVMBaseType(readType)) {
                case llvm::Type::IntegerTyID:   intrinsicID = llvm::Intrinsic::gla_readData;            paramCount = 1; break;
                case llvm::Type::FloatTyID:     intrinsicID = llvm::Intrinsic::gla_fReadInterpolant;    paramCount = 2; break;
                }
                intrinsicName = getLLVMIntrinsicFunction1(intrinsicID, readType);
                name = var->name;
            } else {
                gla::UnsupportedFunctionality("non-fragment shaders");
            }

            // Call the selected intrinsic
            switch(paramCount) {
            case 2:  lastValue = builder.CreateCall2 (intrinsicName, interpLoc, interpOffset, name); break;
            case 1:  lastValue = builder.CreateCall  (intrinsicName, interpLoc, name);               break;
            }
        } else {
            lastValue = builder.CreateLoad(lastValue);
        }
    }

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_loop *ir)
{
    gla::UnsupportedFunctionality("Loops");

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_loop *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_function_signature *)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_function_signature *)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_function *f)
{
    if (f->has_user_signature()) {
        if (strcmp(f->name, "main") == 0)
            builder.SetInsertPoint(getShaderEntry());
        else {
            //?? still need to use correct signature
            llvm::FunctionType *functionType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), false);
            llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, f->name, module);
            llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(context, "entry", function);
            builder.SetInsertPoint(entryBlock);
        }
    }

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_function *f)
{
    if (f->has_user_signature()) {
        if(strcmp(f->name, "main") == 0) {
            llvm::Intrinsic::ID intrinsicID;
                
            //Call writeData intrinsic on our outs
            while(!glslOuts.empty()) {
                llvm::Value* loadVal = builder.CreateLoad(glslOuts.front());

                switch(getLLVMBaseType(loadVal)) {
                case llvm::Type::IntegerTyID:   intrinsicID = llvm::Intrinsic::gla_writeData;   break;
                case llvm::Type::FloatTyID:     intrinsicID = llvm::Intrinsic::gla_fWriteData;  break;
                }

                llvm::Function *intrinsicName = getLLVMIntrinsicFunction1(intrinsicID, loadVal->getType());

                lastValue = builder.CreateCall2 (intrinsicName,
                                                 llvm::ConstantInt::get(context, llvm::APInt(32, 0, true)),
                                                 loadVal);

                glslOuts.pop_front();
            }
        }

        //?? need to return the return value from the right places with the right type, this
        // is good just for main
        builder.CreateRet(0);

        if (gla::Options.debug)
            module->dump();
    }

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_expression *expression)
{
    int numOperands = expression->get_num_operands();
    assert(numOperands <= 2);
    llvm::Value* operands[2];

    for (int i = 0; i < numOperands; ++i) {
        expression->operands[i]->accept(this);
        operands[i] = lastValue;
    }

    // Check for operand width consistency
    if(numOperands > 1)
        findAndSmearScalars(operands, numOperands);

    lastValue = expandGLSLOp(expression->operation, operands);

    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_expression *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_texture *ir)
{
    gla::UnsupportedFunctionality("texturing");
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_texture *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_swizzle *swiz)
{
    lastValue = expandGLSLSwizzle(swiz);

    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_swizzle *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_dereference_array *ir)
{
    gla::UnsupportedFunctionality("array dereference");
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_dereference_array *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_dereference_record *ir)
{
    gla::UnsupportedFunctionality("structure dereference");
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_dereference_record *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_assignment *assignment)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_assignment *assignment)
{
    //Handle writemask
    if(!assignment->whole_variable_written()) {
        llvm::Value* targetVector;
        int writeMask = assignment->write_mask;
        int sourceElement = 0;
        llvm::Type::TypeID sourceType = lastValue->getType()->getTypeID();

        // Load our target vector
        targetVector = builder.CreateLoad(lValue);

        // Check each channel of the writemask
        for(int i = 0; i < 4; ++i) {
            if(writeMask & (1 << i)) {
                if(llvm::Type::VectorTyID == sourceType) {
                    // Extract an element to a scalar, then immediately insert to our target
                    targetVector = builder.CreateInsertElement(targetVector, 
                                            builder.CreateExtractElement(lastValue, 
                                                    llvm::ConstantInt::get(context, llvm::APInt(32, sourceElement++, false))),
                                            llvm::ConstantInt::get(context, llvm::APInt(32, i, false)));
                } else {
                    // Insert the scalar target
                    targetVector = builder.CreateInsertElement(targetVector, 
                                            lastValue,
                                            llvm::ConstantInt::get(context, llvm::APInt(32, i, false)));
                }
            }
        }
        // Track the last use of our extract/insert location to be stored
        lastValue = targetVector;
    }

    //Store the result, using dest we track in base class
    llvm::StoreInst *store = builder.CreateStore(lastValue, lValue);
    lastValue = store;

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_call *call)
{
    if(call->get_callee()->function()->has_user_signature()) {
        gla::UnsupportedFunctionality("user functions");
    } else {
        exec_list_iterator iter = call->actual_parameters.iterator();

        // Stick this somewhere that makes sense, with a real value
        #define GLA_MAX_PARAMETERS 10

        llvm::Value* llvmParams[GLA_MAX_PARAMETERS];
        ir_rvalue *param = NULL;
        int paramCount = 0;

        // Build a list of actual parameters
        while(iter.has_next())
        {
            param = (ir_rvalue *) iter.get();
            param->accept(this);
            llvmParams[paramCount] = lastValue;
            paramCount++;
            iter.next();
        }

        assert(paramCount < GLA_MAX_PARAMETERS);

        llvm::Value* returnValue = 0;

        if(!strcmp(call->callee_name(), "mod")) {
            returnValue = expandGLSLOp(ir_binop_mod, llvmParams);
        }
        if(!strcmp(call->callee_name(), "mix")) {
            if(llvm::Type::IntegerTyID == getLLVMBaseType(llvmParams[0]))
                returnValue = builder.CreateSelect(llvmParams[2], llvmParams[0], llvmParams[1]);
        }
        if(!strcmp(call->callee_name(), "lessThan")) {
            returnValue = expandGLSLOp(ir_binop_less, llvmParams);
        }
        if(!strcmp(call->callee_name(), "lessThanEqual")) {
            returnValue = expandGLSLOp(ir_binop_lequal, llvmParams);
        }
        if(!strcmp(call->callee_name(), "greaterThan")) {
            returnValue = expandGLSLOp(ir_binop_greater, llvmParams);
        }
        if(!strcmp(call->callee_name(), "greaterThanEqual")) {
            returnValue = expandGLSLOp(ir_binop_gequal, llvmParams);
        }
        if(!strcmp(call->callee_name(), "equal")) {
            returnValue = expandGLSLOp(ir_binop_equal, llvmParams);
        }
        if(!strcmp(call->callee_name(), "notEqual")) {
            returnValue = expandGLSLOp(ir_binop_nequal, llvmParams);
        }
        if(!strcmp(call->callee_name(), "not")) {
            returnValue = builder.CreateNot(llvmParams[0]);
        }

        // If this call requires an intrinsic
        if(!returnValue)
            returnValue = createLLVMIntrinsic(call, llvmParams, paramCount);

        // Track the return value for to be consumed by next instruction
        lastValue = returnValue;
    }

    return visit_continue_with_parent;
}

llvm::Value* GlslToTopVisitor::createLLVMIntrinsic(ir_call *call, llvm::Value** llvmParams, int paramCount)
{
    llvm::Function *intrinsicName = 0;
    const char *name = NULL;
    gla::ETextureFlags texFlags = {0};
    llvm::Type* resultType = convertGLSLToLLVMType(call->type);

    #define GLA_MAX_PARAMETER 5
    llvm::Value* outParams[GLA_MAX_PARAMETER];
    for(int i = 0; i < GLA_MAX_PARAMETER; i++)
        outParams[i] = llvmParams[i];

    bool isBiased = false;
    llvm::Intrinsic::ID textureIntrinsicID = llvm::Intrinsic::gla_fTextureSample;
    gla::ESamplerType   samplerType;

    // Based on the name of the callee, create the appropriate intrinsicID
    if(!strcmp(call->callee_name(), "radians"))                 { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fRadians, resultType, llvmParams[0]->getType());  name = "radians";  }
    else if(!strcmp(call->callee_name(), "degrees"))            { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fDegrees, resultType, llvmParams[0]->getType()); name = "degrees"; }
    else if(!strcmp(call->callee_name(), "sin"))                { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fSin, resultType, llvmParams[0]->getType()); name = "sin"; }
    else if(!strcmp(call->callee_name(), "cos"))                { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fCos, resultType, llvmParams[0]->getType()); name = "cos"; }
    else if(!strcmp(call->callee_name(), "tan"))                { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fTan, resultType, llvmParams[0]->getType()); name = "tan"; }
    else if(!strcmp(call->callee_name(), "asin"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fAsin, resultType, llvmParams[0]->getType()); name = "asin"; }
    else if(!strcmp(call->callee_name(), "acos"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fAcos, resultType, llvmParams[0]->getType()); name = "acos"; }
    else if(!strcmp(call->callee_name(), "atan"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fAtan, resultType, llvmParams[0]->getType()); name = "atan"; }
    else if(!strcmp(call->callee_name(), "sinh"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fSinh, resultType, llvmParams[0]->getType()); name = "sinh"; }
    else if(!strcmp(call->callee_name(), "cosh"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fCosh, resultType, llvmParams[0]->getType()); name = "cosh"; }
    else if(!strcmp(call->callee_name(), "tanh"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fTanh, resultType, llvmParams[0]->getType()); name = "tanh"; }
    else if(!strcmp(call->callee_name(), "asinh"))              { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fAsinh, resultType, llvmParams[0]->getType()); name = "asinh"; }
    else if(!strcmp(call->callee_name(), "acosh"))              { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fAcosh, resultType, llvmParams[0]->getType()); name = "acosh"; }
    else if(!strcmp(call->callee_name(), "atanh"))              { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fAtanh, resultType, llvmParams[0]->getType()); name = "atanh"; }
    else if(!strcmp(call->callee_name(), "pow"))                { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_fPow, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "pow";  }
    else if(!strcmp(call->callee_name(), "exp"))                { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fExp, resultType, llvmParams[0]->getType()); name = "exp"; }
    else if(!strcmp(call->callee_name(), "log"))                { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fLog, resultType, llvmParams[0]->getType()); name = "log"; }
    else if(!strcmp(call->callee_name(), "exp2"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fExp2, resultType, llvmParams[0]->getType()); name = "exp2"; }
    else if(!strcmp(call->callee_name(), "log2"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fLog2, resultType, llvmParams[0]->getType()); name = "log2"; }
    else if(!strcmp(call->callee_name(), "sqrt"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fSqrt, resultType, llvmParams[0]->getType()); name = "sqrt"; }
    else if(!strcmp(call->callee_name(), "inversesqrt"))        { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fInverseSqrt, resultType, llvmParams[0]->getType()); name = "inversesqrt"; }
    else if(!strcmp(call->callee_name(), "floor"))              { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fFloor, resultType, llvmParams[0]->getType()); name = "floor"; }
    //else if(!strcmp(call->callee_name(), "trunc"))            { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::xxxx, resultType); name = "xxxx"; }  // defect in glsl2
    else if(!strcmp(call->callee_name(), "round"))              { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fRoundFast, resultType, llvmParams[0]->getType()); name = "round"; }
    else if(!strcmp(call->callee_name(), "roundEven"))          { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fRoundEven, resultType, llvmParams[0]->getType()); name = "roundEven"; }
    else if(!strcmp(call->callee_name(), "ceil"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fCeiling, resultType, llvmParams[0]->getType()); name = "ceil"; }
    else if(!strcmp(call->callee_name(), "fract"))              { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fFraction, resultType, llvmParams[0]->getType()); name = "fract"; }
    else if(!strcmp(call->callee_name(), "modf"))               { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_fModF, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "modf";  }
    else if(!strcmp(call->callee_name(), "mix"))                { intrinsicName = getLLVMIntrinsicFunction4(llvm::Intrinsic::gla_fMix, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); name = "mix"; }
    else if(!strcmp(call->callee_name(), "step"))               { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_fStep, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "step";  }
    else if(!strcmp(call->callee_name(), "smoothstep"))         { intrinsicName = getLLVMIntrinsicFunction4(llvm::Intrinsic::gla_fSmoothStep, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); name = "smoothstep"; }
    else if(!strcmp(call->callee_name(), "intBitsToFloat"))     { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_fIntBitsTofloat, resultType); name = "intBitsToFloat"; }
    else if(!strcmp(call->callee_name(), "uintBitsToFloat"))    { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_fIntBitsTofloat, resultType); name = "uintBitsToFloat"; }
    else if(!strcmp(call->callee_name(), "fma"))                { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_fFma, resultType); name = "fma"; }
    else if(!strcmp(call->callee_name(), "frexp"))              { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_fFrexp, resultType); name = "frexp"; }
    else if(!strcmp(call->callee_name(), "ldexp"))              { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_fLdexp, resultType); name = "ldexp"; }
    else if(!strcmp(call->callee_name(), "unpackUnorm2x16"))    { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_fUnpackUnorm2x16, resultType); name = "unpackUnorm2x16"; }
    else if(!strcmp(call->callee_name(), "unpackUnorm4x8"))     { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_fUnpackUnorm4x8, resultType); name = "unpackUnorm4x8"; }
    else if(!strcmp(call->callee_name(), "unpackSnorm4x8"))     { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_fUnpackSnorm4x8, resultType); name = "unpackSnorm4x8"; }
    else if(!strcmp(call->callee_name(), "length"))             { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_fLength, llvmParams[0]->getType()); name = "length"; }
    else if(!strcmp(call->callee_name(), "distance"))           { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fDistance, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "distance";  }
    else if(!strcmp(call->callee_name(), "dot"))                { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fDot, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "dot";  }
    else if(!strcmp(call->callee_name(), "cross"))              { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_fCross, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "cross";  }
    else if(!strcmp(call->callee_name(), "normalize"))          { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fNormalize, resultType, llvmParams[0]->getType()); name = "normalize"; }
    else if(!strcmp(call->callee_name(), "ftransform"))         { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_fFixedTransform, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "ftransform";  }
    else if(!strcmp(call->callee_name(), "faceforward"))        { intrinsicName = getLLVMIntrinsicFunction4(llvm::Intrinsic::gla_fFaceForward, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); name = "faceforward"; }
    else if(!strcmp(call->callee_name(), "reflect"))            { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_fReflect, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "reflect";  }
    else if(!strcmp(call->callee_name(), "refract"))            { intrinsicName = getLLVMIntrinsicFunction4(llvm::Intrinsic::gla_fRefract, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); name = "refract"; }
    else if(!strcmp(call->callee_name(), "dFdx"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fDFdx, resultType, llvmParams[0]->getType()); name = "dFdx"; }
    else if(!strcmp(call->callee_name(), "dFdy"))               { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fDFdy, resultType, llvmParams[0]->getType()); name = "dFdy"; }
    else if(!strcmp(call->callee_name(), "fwidth"))             { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fFilterWidth, resultType, llvmParams[0]->getType()); name = "fwidth"; }
    else if(!strcmp(call->callee_name(), "any"))                { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_any, llvmParams[0]->getType()); name = "any"; }
    else if(!strcmp(call->callee_name(), "all"))                { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_all, llvmParams[0]->getType()); name = "all"; }

    // Select intrinsic based on parameter types
    else if(!strcmp(call->callee_name(), "abs"))                { 
        switch(getLLVMBaseType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_abs, resultType, llvmParams[0]->getType()); name = "abs";   break;  }
        case llvm::Type::FloatTyID:                             { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fAbs, resultType, llvmParams[0]->getType()); name = "abs";  break;  }  }  }
    else if(!strcmp(call->callee_name(), "sign"))               { 
        switch(getLLVMBaseType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { gla::UnsupportedFunctionality("Integer sign() ");  break;  }
        case llvm::Type::FloatTyID:                             { intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fSign, resultType, llvmParams[0]->getType()); name = "sign"; break;  }  }  }
    else if(!strcmp(call->callee_name(), "min"))                { 
        switch(getLLVMBaseType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_sMin, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "min";  break;  }
        case llvm::Type::FloatTyID:                             { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_fMin, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "min";  break;  }  }  }
    else if(!strcmp(call->callee_name(), "max"))                { 
        switch(getLLVMBaseType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_sMax, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "max";  break;  }
        case llvm::Type::FloatTyID:                             { intrinsicName = getLLVMIntrinsicFunction3(llvm::Intrinsic::gla_fMax, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); name = "max";  break;  }  }  }
    else if(!strcmp(call->callee_name(), "clamp"))              { 
        switch(getLLVMBaseType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { intrinsicName = getLLVMIntrinsicFunction4(llvm::Intrinsic::gla_sClamp, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); name = "clamp";  break;  }
        case llvm::Type::FloatTyID:                             { intrinsicName = getLLVMIntrinsicFunction4(llvm::Intrinsic::gla_fClamp, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); name = "clamp";  break;  }  }  }

    // Unsupported calls
    //noise*")) { }
    //else if(!strcmp(call->callee_name(), "matrixCompMult"))   { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::xxxx, resultType); name = "xxxx"; }
    //else if(!strcmp(call->callee_name(), "outerProduct"))     { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::xxxx, resultType); name = "xxxx"; }
    //else if(!strcmp(call->callee_name(), "transpose"))        { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::xxxx, resultType); name = "xxxx"; }
    //else if(!strcmp(call->callee_name(), "determinant"))      { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::xxxx, resultType); name = "xxxx"; }
    //else if(!strcmp(call->callee_name(), "inverse"))          { intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::xxxx, resultType); name = "xxxx"; }

    // Texture calls
    else if(!strcmp(call->callee_name(), "texture1D")) {
        samplerType    = gla::ESampler1D;
        texFlags.EBias = (paramCount > 2) ? true : false;
        name           = "texture1DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture1DProj")) {
        samplerType         = gla::ESampler1D;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "texture1DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture1DLod")) {
        textureIntrinsicID   = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType   = gla::ESampler1D;
        texFlags.ELod = true;
        name          = "texture1DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture1DProjLod")) {
        textureIntrinsicID   = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType   = gla::ESampler1D;
        texFlags.ELod = true;
        name          = "texture1DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture2D")) {
        samplerType      = gla::ESampler2D;
        texFlags.EBias   = (paramCount > 2) ? true : false;
        name             = "texture2DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture2DProj")) {
        samplerType         = gla::ESampler2D;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "texture2DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture2DLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler2D;
        texFlags.ELod       = true;
        name                = "texture2DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture2DProjLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler2D;
        texFlags.EProjected = true;
        texFlags.ELod       = true;
        name                = "texture2DProjLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture3D")) {
        samplerType     = gla::ESampler3D;
        texFlags.EBias  = (paramCount > 2) ? true : false;
        name            = "texture3DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture3DProj")) {
        samplerType         = gla::ESampler3D;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "texture3DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture3DLod")) {
        samplerType     = gla::ESampler3D;
        texFlags.ELod   = true;
        name            = "texture3DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture3DProjLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler3D;
        texFlags.EProjected = true;
        texFlags.ELod       = true;
        name                = "texture3DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "textureCube")) {
        samplerType     = gla::ESamplerCube;
        texFlags.EBias  = (paramCount > 2) ? true : false;
        name            = "textureCubeTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "textureCubeLod ")) {
        textureIntrinsicID = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType        = gla::ESamplerCube;
        texFlags.ELod      = true;
        name               = "textureCubeLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow1D")) {
        samplerType     = gla::ESampler1DShadow;
        texFlags.EBias  = (paramCount > 2) ? true : false;
        name            = "shadow1DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow2D")) {
        samplerType     = gla::ESampler2DShadow;
        texFlags.EBias  = (paramCount > 2) ? true : false;
        name            = "shadow2DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow1DProj")) {
        samplerType         = gla::ESampler1DShadow;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "shadow1DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow2DProj")) {
        samplerType         = gla::ESampler2DShadow;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "shadow2DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow1DLod")) {
        textureIntrinsicID = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType        = gla::ESampler1DShadow;
        texFlags.ELod      = true;
        name               = "shadow1DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow2DLod")) {
        textureIntrinsicID = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType        = gla::ESampler2DShadow;
        texFlags.ELod      = true;
        name               = "shadow2DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow1DProjLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler1DShadow;
        texFlags.EProjected = true;
        texFlags.ELod       = true;
        name                = "shadow1DProjLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow2DProjLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler2DShadow;
        texFlags.EProjected = true;
        texFlags.ELod       = true;
        name                = "shadow2DProjLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else {
        gla::UnsupportedFunctionality("Built-in function: ", gla::EATContinue);
        gla::UnsupportedFunctionality(call->callee_name());
    }

    llvm::CallInst *callInst = 0;

    // Create a call to it
    switch(paramCount)
    {
    case 5:
        callInst = builder.CreateCall5(intrinsicName, outParams[0] , outParams[1], outParams[2], outParams[3], outParams[4], name);
        break;
    case 4:
        callInst = builder.CreateCall4(intrinsicName, outParams[0] , outParams[1], outParams[2], outParams[3], name);
        break;
    case 3:
        callInst = builder.CreateCall3(intrinsicName, outParams[0] , outParams[1], outParams[2], name);
        break;
    case 2:
        callInst = builder.CreateCall2(intrinsicName, outParams[0], outParams[1], name);
        break;
    case 1:
        callInst = builder.CreateCall (intrinsicName, outParams[0], name);
        break;
    default:
        assert(! "Unsupported parameter count");
    }

   return callInst;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_call *call)
{
    (void) call;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_return *ir)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_return *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_discard *ir)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_discard *ir)
{
    (void) ir;
    return visit_continue;

}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_if *ifNode)
{
    // emit condition into current block
    lastValue = 0;
    ifNode->condition->accept(this);
    llvm::Value *condValue = lastValue;
    assert(condValue != 0);

    llvm::Function *function = builder.GetInsertBlock()->getParent();

    // make the blocks, but only put the then-block into the function,
    // the else-block and merge-block will be added later, in order, after
    // earlier code is emitted
    bool haveElse = ! ifNode->else_instructions.is_empty();
    llvm::BasicBlock  *ThenBB = llvm::BasicBlock::Create(context, "then", function);
    llvm::BasicBlock  *ElseBB = haveElse ? llvm::BasicBlock::Create(context, "else") : 0;
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(context, "ifmerge");

    // make the flow control split
    if (haveElse)
        builder.CreateCondBr(condValue, ThenBB, ElseBB);
    else
        builder.CreateCondBr(condValue, ThenBB, MergeBB);

    builder.SetInsertPoint(ThenBB);

    // emit the then statement
    visit_list_elements(this, &(ifNode->then_instructions));

    // jump to the merge block
    builder.CreateBr(MergeBB);

    // emitting the then-block could change the current block, update
    ThenBB = builder.GetInsertBlock();

    // add else block to the function
    if (haveElse) {
        function->getBasicBlockList().push_back(ElseBB);
        builder.SetInsertPoint(ElseBB);

        // emit the else statement
        visit_list_elements(this, &(ifNode->else_instructions));

        // jump to the merge block
        builder.CreateBr(MergeBB);

        // emitting the else block could change the current block, update
        ElseBB = builder.GetInsertBlock();
    }

    // add the merge block to the function
    function->getBasicBlockList().push_back(MergeBB);
    builder.SetInsertPoint(MergeBB);

    // The glsl "value" of an if-else should never be taken (share code with "?:" though?)
    lastValue = 0;

    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_if *ir)
{
    (void) ir;
    return visit_continue;
}

llvm::Value* GlslToTopVisitor::createLLVMVariable(ir_variable* var)
{
    unsigned int addressSpace = gla::GlobalAddressSpace;
    bool constant = var->read_only;
    bool local = false;
    llvm::Constant* initializer = 0;
    llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalVariable::InternalLinkage;
    llvm::Type *llvmVarType = convertGLSLToLLVMType(var->type);    
    llvm::Value* value = 0;

    const char* typePrefix = 0;
    if (var->type->base_type == GLSL_TYPE_SAMPLER)
        typePrefix = getSamplerDeclaration(var);

    switch (var->mode) {
    case ir_var_auto:
        //?? want to distinguish between globals and locals
        //?? this will break for a global accessed by multiple functions
        // a global needs an initializer so it will get eliminated by LLVM
        local = true;
        break;

    case ir_var_uniform:
        // ?? need to generalize to N objects (constant buffers) for higher shader models
        // ?? link:  we need link info to know how large the memory object is
        addressSpace = gla::UniformAddressSpace;
        assert(var->read_only == true);
        linkage = llvm::GlobalVariable::ExternalLinkage;
        break;

    case ir_var_in:
        // inputs should all be pipeline reads
        assert(! "no memory allocations for inputs");
        return 0;

    case ir_var_out:
        // keep internal linkage, because epilogue will to the write out to the pipe
        // internal linkage helps with global optimizations, so does having an initializer
        initializer = llvm::Constant::getNullValue(llvmVarType);
        break;

    case ir_var_inout:
        // can only be for function parameters
        local = true;
        break;

    case ir_var_temporary:
        local = true;
        break;

    default:
        assert(! "Unhandled var->mode");
        break;
    }

    //?? still need to consume the following
    // var->max_array_access;
    // var->centroid;
    // var->invariant;
    // var->interpolation;
    // var->origin_upper_left;
    // var->pixel_center_integer;
    // var->location;
    // var->constant_value;

    if (local) {
        //?? if global scope
            llvm::BasicBlock* entryBlock = getShaderEntry();
        //?? else.
            //llvm::BasicBlock* entryBlock = &builder.GetInsertBlock()->getParent()->getEntryBlock();
        
        llvm::IRBuilder<> entryBuilder(entryBlock, entryBlock->begin());
        value = entryBuilder.CreateAlloca(llvmVarType, 0, var->name);
    } else {
        if (strcmp(var->name, "gl_FragDepth") == 0)
            gla::UnsupportedFunctionality("gl_FragDepth");

        if (strcmp(var->name, "gl_FragData") == 0)
            gla::UnsupportedFunctionality("gl_FragData");

        std::string name = var->name;
        if (gla::Options.backend == gla::GLSL && typePrefix) {
            name = typePrefix;
            name.append(" ");
            name.append(var->name);
        } else
            name = var->name;

        llvm::GlobalVariable* globalValue = new llvm::GlobalVariable(llvmVarType, constant, linkage,
                                         initializer, name, false /* ThreadLocal */, addressSpace);
        module->getGlobalList().push_back(globalValue);
        value = globalValue;
    }

    return value;
}

const char* GlslToTopVisitor::getSamplerDeclaration(ir_variable* var)
{
    if (var->type->sampler_shadow) {
        switch (var->type->sampler_dimensionality) {
        case GLSL_SAMPLER_DIM_1D:   return "sampler1DShadow";
        case GLSL_SAMPLER_DIM_2D:   return "sampler2DShadow";
        case GLSL_SAMPLER_DIM_3D:   return "sampler3DShadow";
        case GLSL_SAMPLER_DIM_CUBE: return "samplerCubeShadow";
        case GLSL_SAMPLER_DIM_RECT: return "sampler2DRectShadow";
        default:
            gla::UnsupportedFunctionality("shadow sampler type");
        }
    } else {
        switch (var->type->sampler_dimensionality) {
        case GLSL_SAMPLER_DIM_1D:   return "sampler1D";
        case GLSL_SAMPLER_DIM_2D:   return "sampler2D";
        case GLSL_SAMPLER_DIM_3D:   return "sampler3D";
        case GLSL_SAMPLER_DIM_CUBE: return "samplerCube";
        case GLSL_SAMPLER_DIM_RECT: return "sampler2DRect";
        default:
            gla::UnsupportedFunctionality("sampler type");
        }
    }

    return 0;
}

llvm::Value* GlslToTopVisitor::expandGLSLOp(ir_expression_operation glslOp, llvm::Value** operands)
{
    llvm::Value* result = 0;
    const llvm::Type* varType;
    const llvm::VectorType* vectorType;

    vectorType = llvm::dyn_cast<llvm::VectorType>(operands[0]->getType());

    switch(glslOp) {

    case ir_unop_f2i:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getInt32Ty(context), vectorType->getNumElements());
        else            varType = llvm::Type::getInt32Ty(context);
        return          builder.CreateFPToUI(operands[0], varType);
    case ir_unop_i2f:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getFloatTy(context), vectorType->getNumElements());
        else            varType = llvm::Type::getFloatTy(context);
        return          builder.CreateSIToFP(operands[0], varType);
    case ir_unop_f2b:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getInt1Ty(context), vectorType->getNumElements());
        else            varType = llvm::Type::getInt1Ty(context);
        return          builder.CreateFPToUI(operands[0], varType);
    case ir_unop_b2f:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getFloatTy(context), vectorType->getNumElements());
        else            varType = llvm::Type::getFloatTy(context);
        return          builder.CreateUIToFP(operands[0], varType);
    case ir_unop_i2b:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getInt1Ty(context), vectorType->getNumElements());
        else            varType = llvm::Type::getInt1Ty(context);
        return          builder.CreateIntCast(operands[0], varType, false);
    case ir_unop_b2i:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getInt32Ty(context), vectorType->getNumElements());
        else            varType = llvm::Type::getInt32Ty(context);
        return          builder.CreateIntCast(operands[0], varType, true);
    case ir_unop_u2f:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getFloatTy(context), vectorType->getNumElements());
        else            varType = llvm::Type::getFloatTy(context);
        return          builder.CreateUIToFP(operands[0], varType);
    case ir_binop_add:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFAdd(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateAdd (operands[0], operands[1]);
        }
    case ir_binop_sub:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFSub(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateSub (operands[0], operands[1]);
        }
    case ir_binop_mul:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFMul(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateMul (operands[0], operands[1]);
        }
    case ir_binop_div:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFDiv(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateSDiv(operands[0], operands[1]);
        }
    case ir_binop_less:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFCmpOLT(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateICmpSLT(operands[0], operands[1]);
        }
    case ir_binop_greater:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFCmpOGT(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateICmpSGT(operands[0], operands[1]);
        }
    case ir_binop_lequal:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFCmpOLE(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateICmpSLE(operands[0], operands[1]);
        }
    case ir_binop_gequal:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFCmpOGE(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateICmpSGE(operands[0], operands[1]);
        }
    case ir_binop_equal:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFCmpOEQ(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateICmpEQ (operands[0], operands[1]);
        }
    case ir_binop_nequal:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFCmpONE(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateICmpNE (operands[0], operands[1]);
        }
    case ir_binop_lshift:
        return builder.CreateLShr(operands[0], operands[1]);
    case ir_binop_rshift:
        return builder.CreateLShr(operands[0], operands[1]);
    case ir_binop_logic_and:
        return builder.CreateAnd(operands[0], operands[1]);
    case ir_binop_logic_xor:
        return builder.CreateXor(operands[0], operands[1]);
    case ir_binop_logic_or:
        return builder.CreateOr(operands[0], operands[1]);
    case ir_binop_mod:
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         return builder.CreateFRem(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return builder.CreateSRem(operands[0], operands[1]);
        }
    case ir_binop_all_equal:
        // Returns single boolean for whether all components of operands[0] equal the
        // components of operands[1]
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         result = builder.CreateFCmpOEQ(operands[0], operands[1]);  break;
        case llvm::Type::IntegerTyID:       result = builder.CreateICmpEQ (operands[0], operands[1]);  break;
        }
        if(vectorType)  return builder.CreateCall(getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_all, result->getType()), result);
        else            return result;

    case ir_binop_any_nequal:
        // Returns single boolean for whether any component of operands[0] is 
        // not equal to the corresponding component of operands[1].
        switch(getLLVMBaseType(operands[0])) {
        case llvm::Type::FloatTyID:         result = builder.CreateFCmpONE(operands[0], operands[1]);  break;
        case llvm::Type::IntegerTyID:       result = builder.CreateICmpNE (operands[0], operands[1]);  break;
        }
        if(vectorType)  return builder.CreateCall(getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_any, result->getType()), result);
        else            return result;
    case ir_binop_min:
    case ir_binop_max:
    case ir_binop_pow:
    case ir_binop_dot:
    case ir_binop_bit_and:
    case ir_binop_bit_xor:
    case ir_binop_bit_or:

    default:
        gla::UnsupportedFunctionality("Binary Operation: ", glslOp);
    }

    return result;
}

int makeSwizzle(ir_swizzle_mask mask) {
    return (((mask.x)<<0) | ((mask.y)<<2) | ((mask.z)<<4) | ((mask.w)<<6));
}

llvm::Value* GlslToTopVisitor::expandGLSLSwizzle(ir_swizzle* swiz)
{
    llvm::Value* operand;

    // traverse the tree we're swizzling
    swiz->val->accept(this);
    operand = lastValue;

    // convert our GLSL mask to an int
    int swizVal = makeSwizzle(swiz->mask);

    // swizzle needs the source type and dest type
    llvm::Intrinsic::ID intrinsicID;
    switch(getLLVMBaseType(operand)) {
    case llvm::Type::IntegerTyID:   intrinsicID = llvm::Intrinsic::gla_swizzle;     break;
    case llvm::Type::FloatTyID:     intrinsicID = llvm::Intrinsic::gla_fSwizzle;    break;
    }
    
    llvm::Function *intrinsicName = getLLVMIntrinsicFunction2(intrinsicID,
                                                            convertGLSLToLLVMType(swiz->type),
                                                            operand->getType());

    llvm::CallInst *callInst = builder.CreateCall2 (intrinsicName,
                                                    operand,
                                                    llvm::ConstantInt::get(context, llvm::APInt(32, swizVal, true)),
                                                    "swizzleTmp");

    return callInst;
}

llvm::Type* GlslToTopVisitor::convertGLSLToLLVMType(const glsl_type* type)
{
    const unsigned varType = type->base_type;
    unsigned isMatrix = type->is_matrix();

    if (isMatrix)
        gla::UnsupportedFunctionality("matrices");

    llvm::Type *llvmVarType;

    switch(varType) {
    case GLSL_TYPE_UINT:
        //?? How to do unsigned var in LLVM?
        llvmVarType = (llvm::Type*)llvm::Type::getInt32Ty(context);
        break;
    case GLSL_TYPE_INT:
        llvmVarType = (llvm::Type*)llvm::Type::getInt32Ty(context);
        break;
    case GLSL_TYPE_FLOAT:
        llvmVarType = (llvm::Type*)llvm::Type::getFloatTy(context);
        break;
    case GLSL_TYPE_BOOL:
        llvmVarType = (llvm::Type*)llvm::Type::getInt1Ty(context);
        break;
    case GLSL_TYPE_SAMPLER:
        //Treating sampler as an integer for now
        llvmVarType = (llvm::Type*)llvm::Type::getInt32Ty(context);
        break;
    case GLSL_TYPE_ARRAY:     gla::UnsupportedFunctionality("arrays");
    case GLSL_TYPE_STRUCT:    gla::UnsupportedFunctionality("structures");
    case GLSL_TYPE_VOID:      gla::UnsupportedFunctionality("void type");
    case GLSL_TYPE_ERROR:     assert(! "type error");
    default:
        gla::UnsupportedFunctionality("basic type");
        break;
    }

    // If this variable has a vector element count greater than 1, create an LLVM vector
    unsigned vecCount = type->vector_elements;
    if(vecCount > 1)
        llvmVarType = llvm::VectorType::get(llvmVarType, vecCount);

    return llvmVarType;
}

//llvm::Function* GlslToTopVisitor::getLLVMIntrinsicFunction(llvm::Intrinsic::ID ID, const llvm::Type* resultType, llvm::Value** paramTypes, int paramCount)
//{
//    int intrinsicTypeCount = paramCount;
//    const llvm::Type* intrinsicTypes[GLA_MAX_PARAMETERS] = {0};
//
//    intrinsicTypes[0] = resultType;
//
//    for(int i = 0; i < paramCount; ++i)
//        intrinsicTypes[i + 1] = paramTypes[i]->getType();
//
//    // Look up the intrinsic
//    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, intrinsicTypeCount);
//}

llvm::Function* GlslToTopVisitor::getLLVMIntrinsicFunction1(llvm::Intrinsic::ID ID, const llvm::Type* type1)
{
    int intrinsicTypeCount = 1;
    const llvm::Type* intrinsicTypes[1] = {0};

    intrinsicTypes[0] = type1;

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, intrinsicTypeCount);
}

llvm::Function* GlslToTopVisitor::getLLVMIntrinsicFunction2(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2)
{
    int intrinsicTypeCount = 2;
    const llvm::Type* intrinsicTypes[2] = {0};

    intrinsicTypes[0] = type1;
    intrinsicTypes[1] = type2;

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, intrinsicTypeCount);
}

llvm::Function* GlslToTopVisitor::getLLVMIntrinsicFunction3(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2, const llvm::Type* type3)
{
    int intrinsicTypeCount = 3;
    const llvm::Type* intrinsicTypes[3] = {0};

    intrinsicTypes[0] = type1;
    intrinsicTypes[1] = type2;
    intrinsicTypes[2] = type3;

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, intrinsicTypeCount);
}

llvm::Function* GlslToTopVisitor::getLLVMIntrinsicFunction4(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2, const llvm::Type* type3, const llvm::Type* type4)
{
    int intrinsicTypeCount = 4;
    const llvm::Type* intrinsicTypes[4] = {0};

    intrinsicTypes[0] = type1;
    intrinsicTypes[1] = type2;
    intrinsicTypes[2] = type3;
    intrinsicTypes[3] = type4;

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, intrinsicTypeCount);
}

void GlslToTopVisitor::createLLVMTextureIntrinsic(llvm::Function* &intrinsicName, int &paramCount,
                                                  llvm::Value** outParams, llvm::Value** llvmParams, llvm::Type* resultType,
                                                  llvm::Intrinsic::ID intrinsicID, gla::ESamplerType samplerType, gla::ETextureFlags texFlags)
{
    bool isBiased = texFlags.EBias;

    switch(intrinsicID) {
    case llvm::Intrinsic::gla_fTextureSample:
            outParams[0] = llvm::ConstantInt::get(context, llvm::APInt(32, samplerType, true));
            outParams[1] = llvmParams[0];
            outParams[2] = llvm::ConstantInt::get(context, llvm::APInt(32, *(int*)&texFlags, true));  //flag enum
            outParams[3] = llvmParams[1];

            if (isBiased) {
                //Bias requires SampleLod with EBias flag
                intrinsicID = llvm::Intrinsic::gla_fTextureSampleLod;
                outParams[4] = llvmParams[2];
            }

            paramCount += 2;
            intrinsicName = getLLVMIntrinsicFunction2(intrinsicID, resultType, llvmParams[1]->getType());
            break;
    case llvm::Intrinsic::gla_fTextureSampleLod:
            outParams[0] = llvm::ConstantInt::get(context, llvm::APInt(32, samplerType, true));
            outParams[1] = llvmParams[0];
            outParams[2] = llvm::ConstantInt::get(context, llvm::APInt(32,  *(int*)&texFlags, true));  //flag enum
            outParams[3] = llvmParams[1];
            outParams[4] = llvmParams[2];
            paramCount += 2;
            intrinsicName = getLLVMIntrinsicFunction2(intrinsicID, resultType, llvmParams[1]->getType());
            break;
    default:
        gla::UnsupportedFunctionality("texture: ", intrinsicID);
    }

    return;
}

llvm::Type::TypeID GlslToTopVisitor::getLLVMBaseType(llvm::Value* value)
{
    if(llvm::Type::VectorTyID == value->getType()->getTypeID())
        return value->getType()->getContainedType(0)->getTypeID();
    else
        return value->getType()->getTypeID();
}

llvm::Type::TypeID GlslToTopVisitor::getLLVMBaseType(llvm::Type* type)
{
    if(llvm::Type::VectorTyID == type->getTypeID())
        return type->getContainedType(0)->getTypeID();
    else
        return type->getTypeID();
}

void GlslToTopVisitor::findAndSmearScalars(llvm::Value** operands, int numOperands) 
{    
    assert(numOperands == 2);

    int vectorSize = 0;
    int scalarIndex = 0;
    int vectorIndex = 0;
    llvm::Value* scalarVal = 0;
    llvm::Value* vectorVal = 0;
    const llvm::VectorType* vectorType[2];

    // Find the scalar index and vector size
    for (int i = 0; i < numOperands; ++i) {
        vectorType[i] = llvm::dyn_cast<llvm::VectorType>(operands[i]->getType());
        if(vectorType[i]) {
            vectorSize = vectorType[i]->getNumElements();
            vectorIndex = i;
        } else {
            scalarVal = operands[i];
            scalarIndex = i;
        }
    }

    // If both were vectors or both were scalar, just return
    if( (vectorType[0] && vectorType[1]) || (!vectorType[0] && !vectorType[1]) )
        return;

    // Create a new, same size vector
    //?? if local scope
        llvm::BasicBlock* entryBlock = &builder.GetInsertBlock()->getParent()->getEntryBlock();
    //?? else
        //llvm::BasicBlock* entryBlock = getShaderEntry();
        
    llvm::IRBuilder<> entryBuilder(entryBlock, entryBlock->begin());
    operands[scalarIndex] = builder.CreateLoad(entryBuilder.CreateAlloca(vectorType[vectorIndex], 0));

    // Use a swizzle to expand the scalar to a vector
    llvm::Intrinsic::ID intrinsicID;
    llvm::Type::TypeID scalarType = getLLVMBaseType(scalarVal);
    switch(scalarType) {
    case llvm::Type::IntegerTyID:   intrinsicID = llvm::Intrinsic::gla_swizzle;     break;
    case llvm::Type::FloatTyID:     intrinsicID = llvm::Intrinsic::gla_fSwizzle;    break;
    }
    
    llvm::Function *intrinsicName = getLLVMIntrinsicFunction2(intrinsicID, operands[vectorIndex]->getType(), scalarVal->getType());

    // Broadcast x
    int swizVal = 0;

    llvm::CallInst *callInst = builder.CreateCall2 (intrinsicName,
                                                    scalarVal,
                                                    llvm::ConstantInt::get(context, llvm::APInt(32, swizVal, true)));

    operands[scalarIndex] = callInst;

}

llvm::BasicBlock* GlslToTopVisitor::getShaderEntry()
{
    if (shaderEntry)
        return shaderEntry;

    llvm::FunctionType *functionType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), false);
    llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, "main", module);
    shaderEntry = llvm::BasicBlock::Create(context, "entry", function);
    builder.SetInsertPoint(shaderEntry);

    return shaderEntry;
}