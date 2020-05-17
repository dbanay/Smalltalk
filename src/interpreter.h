//
//  interpreter.h
//  Smalltalk-80
//
//  Created by Dan Banay on 3/31/20.
//  Copyright © 2020 Dan Banay. All rights reserved.
//
//  MIT License
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.
//

#pragma once
#include <string>
#include "objmemory.h"
#include "filesystem.h"
#include "hal.h"

// Add some helpful methods if defined
//#define DEBUGGING_SUPPORT

// implement optional primitiveNext
#define IMPLEMENT_PRIMITIVE_NEXT

// implement optional primitiveAtEnd
#define IMPLEMENT_PRIMITIVE_AT_END

// implement optional primitiveNextPut
#define IMPLEMENT_PRIMITIVE_NEXT_PUT

// implement optional primitiveScanCharacters
#define IMPLEMENT_PRIMITIVE_SCANCHARS

class Interpreter
#ifdef GC_MARK_SWEEP
    : IGCNotification
#endif
{
public:
    
    Interpreter(IHardwareAbstractionLayer *halInterface, IFileSystem *fileSystemInterface);
    
    bool init();
    
 
    // cycle
    void cycle();
    
    inline void checkLowMemoryConditions()
    {
        checkLowMemory = true;
    }
    
    // asynchronousSignal:
    void asynchronousSignal(int aSemaphore);
    
    
    // Debug/testing
    int lastBytecode() { return currentBytecode; }
    
    
    int getDisplayBits(int width, int height);
    
    // Allow read-only access to display form data
    inline int fetchWord_ofDislayBits(int wordIndex, int displayBits)
    {
        return memory.fetchWord_ofObject(wordIndex, displayBits);
    }
    
 

private:
    
    void error(const char *message);

    // --- ArrayStrmPrims ---
    
    // primitiveAtEnd
    void primitiveAtEnd();
    
    // checkIndexableBoundsOf:in:
    void checkIndexableBoundsOf_in(int index, int array);
    
    // primitiveNextPut
    void primitiveNextPut();
    
    // lengthOf:
    int lengthOf(int array);
    
    // primitiveNext
    void primitiveNext();
    
    // dispatchSubscriptAndStreamPrimitives
    void dispatchSubscriptAndStreamPrimitives();
    
    // primitiveStringAt
    void primitiveStringAt();
    
    // primitiveAt
    void primitiveAt();
    
    // primitiveSize
    void primitiveSize();
    
    // primitiveStringAtPut
    void primitiveStringAtPut();
    
    // subscript:with:
    int subscript_with(int array, int index);
    
    // primitiveAtPut
    void primitiveAtPut();
    
    // subscript:with:storing:
    void subscript_with_storing(int array, int index, int value);
    
    
    // --- Contexts ---
    
    // storeContextRegisters
    void storeContextRegisters();
    
    // unPop:
    inline void unPop(int number)
    {
       /* "source"
       	stackPointer <- stackPointer + number
       */
    
        stackPointer = stackPointer + number;
    }
    
    // isBlockContext:
    bool isBlockContext(int contextPointer);
    
    // push:
    inline void push(int object)
    {
       /* "source"
       	stackPointer <- stackPointer + 1.
       	memory storePointer: stackPointer
       		ofObject: activeContext
       		withValue: object
       */
    
        stackPointer = stackPointer + 1;
        memory.storePointer_ofObject_withValue(stackPointer, activeContext, object);
    
    }
    
    // instructionPointerOfContext:
    inline int instructionPointerOfContext(int contextPointer)
    {
       /* "source"
       	^self fetchInteger: InstructionPointerIndex
       		ofObject: contextPointer
       */
    
        return fetchInteger_ofObject(InstructionPointerIndex, contextPointer);
    }
    
    // newActiveContext:
    void newActiveContext(int aContext);
    
    // argumentCountOfBlock:
    inline int argumentCountOfBlock(int blockPointer)
    {
       /* "source"
       	^self fetchInteger: BlockArgumentCountIndex
       		ofObject: blockPointer
       */
    
        return fetchInteger_ofObject(BlockArgumentCountIndex, blockPointer);
    }
    
    // literal:
    inline int literal(int offset)
    {
       /* "source"
       	^self literal: offset
       		ofMethod: method
       */
    
        return literal_ofMethod(offset, method);
    }
    
    // sender
    inline int sender()
    {
       /* "source"
       	^memory fetchPointer: SenderIndex
       		ofObject: homeContext
       */
    
        return memory.fetchPointer_ofObject(SenderIndex, homeContext);
    }
    
    // temporary:
    inline int temporary(int offset)
    {
       /* "source"
       	^memory fetchPointer: offset + TempFrameStart
       		ofObject: homeContext
       */
    
        return memory.fetchPointer_ofObject(offset + TempFrameStart, homeContext);
    }
    
    // caller
    inline int caller()
    {
       /* "source"
       	^memory fetchPointer: SenderIndex
       		ofObject: activeContext
       */
        return memory.fetchPointer_ofObject(SenderIndex, activeContext);
    }
    
    // pop:
    inline void pop(int number)
    {
       /* "source"
       	stackPointer <- stackPointer - number
       */
    
        stackPointer = stackPointer - number;
    }
    
    // storeStackPointerValue:inContext:
    inline void storeStackPointerValue_inContext(int value, int contextPointer)
    {
       /* "source"
       	self storeInteger: StackPointerIndex
       		ofObject: contextPointer
       		withValue: value
       */
    
        storeInteger_ofObject_withValue(StackPointerIndex, contextPointer, value);
    }
    
    // stackValue:
    inline int stackValue(int offset)
    {
       /* "source"
       	^memory fetchPointer: stackPointer - offset
       		ofObject: activeContext
       */
        return memory.fetchPointer_ofObject(stackPointer - offset, activeContext);
    }
    
    // stackTop
    inline int stackTop()
    {
       /* "source"
       	^memory fetchPointer: stackPointer
       		ofObject: activeContext
       */
    
        return memory.fetchPointer_ofObject(stackPointer, activeContext);
    }
    
    // popStack
    inline int popStack()
    {
       int stackTop;
    
       /* "source"
       	stackTop <- memory fetchPointer: stackPointer
       			ofObject: activeContext.
       	stackPointer <- stackPointer - 1.
       	^stackTop
       */
    
        stackTop = memory.fetchPointer_ofObject(stackPointer, activeContext);
        stackPointer = stackPointer - 1;
        return stackTop;
    }
    
    // fetchContextRegisters
    void fetchContextRegisters();
    
    // storeInstructionPointerValue:inContext:
    inline void storeInstructionPointerValue_inContext(int value, int contextPointer)
    {
       /* "source"
       	self storeInteger: InstructionPointerIndex
       		ofObject: contextPointer
       		withValue: value
       */
    
        storeInteger_ofObject_withValue(InstructionPointerIndex, contextPointer, value);
    }
    
    // stackPointerOfContext:
    inline int stackPointerOfContext(int contextPointer)
    {
       /* "source"
       	^self fetchInteger: StackPointerIndex
       		ofObject: contextPointer
       */
    
        return fetchInteger_ofObject(StackPointerIndex, contextPointer);
    }
    
    
    // --- IOPrims ---
    
    // dispatchInputOutputPrimitives
    void dispatchInputOutputPrimitives();
    
    void  primitiveMousePoint();
    void  primitiveCursorLocPut();
    void  primitiveCursorLink();
    void  primitiveInputSemaphore();
    void  primitiveSampleInterval();
    void  primitiveInputWord();
    void  updateDisplay(int destForm, int updatedHeight, int updatedWidth, int updatedX, int updatedY);
    
    void  primitiveCopyBits();
    void  primitiveSnapshot();
    void  primitiveTimeWordsInto();
    void  primitiveTickWordsInto();
    void  primitiveSignalAtTick();
    void  primitiveBeCursor();
    void  primitiveBeDisplay();
    void  primitiveScanCharacters();
    void  primitiveDrawLoop();
    void  primitiveStringReplace();
    
    
    // --- Classes ---
    
    // lookupMethodInDictionary:
    bool lookupMethodInDictionary(int dictionary);
    
    // isPointers:
    inline bool isPointers(int classPointer)
    {
       int pointersFlag;
    
       /* "source"
       	pointersFlag <- self extractBits: 0 to: 0
       			of: (self instanceSpecificationOf: classPointer).
       	^pointersFlag = 1
       */
        pointersFlag = extractBits_to_of(0, 0, instanceSpecificationOf(classPointer));
        return pointersFlag == 1;
    }
    
    // superclassOf:
    inline int superclassOf(int classPointer)
    {
       /* "source"
       	^memory fetchPointer: SuperclassIndex
       		ofObject: classPointer
       */
        return memory.fetchPointer_ofObject(SuperclassIndex, classPointer);
    }
    
    // fixedFieldsOf:
    inline int fixedFieldsOf(int classPointer)
    {
       /* "source"
       	^self extractBits: 4 to: 14
       		of: (self instanceSpecificationOf: classPointer)
       */
    
        return extractBits_to_of(4, 14, instanceSpecificationOf(classPointer));
    }
    
    // isWords:
    inline bool isWords(int classPointer)
    {
       int wordsFlag;
    
       /* "source"
       	wordsFlag <- self extractBits: 1 to: 1
       			of: (self instanceSpecificationOf: classPointer).
       	^wordsFlag = 1
       */
    
        wordsFlag = extractBits_to_of(1, 1, instanceSpecificationOf(classPointer));
        return wordsFlag == 1;
    }
    
    // hash:
    inline int hash(int objectPointer)
    {
       /* "source"
       	^objectPointer bitShift: -1
       */
    
        return objectPointer >> 1;
    
    }
    
    // isIndexable:
    inline bool isIndexable(int classPointer)
    {
       int indexableFlag;
    
       /* "source"
       	indexableFlag <- self extractBits: 2 to: 2
       				of: (self instanceSpecificationOf: classPointer).
       	^indexableFlag = 1
       */
    
        indexableFlag = extractBits_to_of(2, 2, instanceSpecificationOf(classPointer));
        return indexableFlag == 1;
    }
    
    // instanceSpecificationOf:
    inline int instanceSpecificationOf(int classPointer)
    {
       /* "source"
       	^memory fetchPointer: InstanceSpecificationIndex
       		ofObject: classPointer
       */
    
       return memory.fetchPointer_ofObject(InstanceSpecificationIndex, classPointer);
    }
    
    // createActualMessage
    void createActualMessage();
    
    // lookupMethodInClass:
    bool lookupMethodInClass(int cls);
    
    
    // --- ReturnBytecode ---
    
    // returnToActiveContext:
    void returnToActiveContext(int aContext);
    
    // returnBytecode
    void returnBytecode();
    
    // nilContextFields
    void nilContextFields();
    
    // returnValue:to:
    void returnValue_to(int resultPointer, int contextPointer);
    
    
    // --- ControlPrims ---
    
    // synchronousSignal:
    void synchronousSignal(int aSemaphore);
    
    // primitiveBlockCopy
    void primitiveBlockCopy();
    
    // primitiveResume
    void primitiveResume();
    
    // primitivePerformWithArgs
    void primitivePerformWithArgs();
    
    // wakeHighestPriority
    int wakeHighestPriority();
    
    // primitivePerform
    void primitivePerform();
    
    // primitiveValueWithArgs
    void primitiveValueWithArgs();
    
    // removeFirstLinkOfList:
    int removeFirstLinkOfList(int aLinkedList);
    
    // primitiveWait
    void primitiveWait();
    
    // primitiveFlushCache
    void primitiveFlushCache();
    
    // suspendActive
    void suspendActive();
    
    // activeProcess
    int activeProcess();
    
    // schedulerPointer
    inline int schedulerPointer()
    {
       /* "source"
       	^memory fetchPointer: ValueIndex
       		ofObject: SchedulerAssociationPointer
       */
        return memory.fetchPointer_ofObject(ValueIndex, SchedulerAssociationPointer);
    }
    
    // addLastLink:toList:
    void addLastLink_toList(int aLink, int aLinkedList);
    
    // dispatchControlPrimitives
    void dispatchControlPrimitives();
    
    // checkProcessSwitch
    void checkProcessSwitch();
    
    // primitiveSignal
    void primitiveSignal();
    
    // isEmptyList:
    int isEmptyList(int aLinkedList);
    
    // primitiveSuspend
    void primitiveSuspend();
    
    // primitiveValue
    void primitiveValue();
    
    // firstContext
    int firstContext();
    
    
    // transferTo:
    void transferTo(int aProcess);
    
    // resume:
    void resume(int aProcess);
    
    // sleep:
    void sleep(int aProcess);
    
    
    // --- SystemPrims ---
    
    // primitiveClass
    void primitiveClass();
    
    // dispatchSystemPrimitives
    void dispatchSystemPrimitives();
    
    // primitiveEquivalent
    void primitiveEquivalent();
    
    void primitiveCoreLeft();
    void primitiveQuit();
    void primitiveExitToDebugger();
    void primitiveOopsLeft();
    void primitiveSignalAtOopsLeftWordsLeft();
    
    
    // Vendor specific
    void dispatchPrivatePrimitives();
    
    // Posix filesystem primitives -- dbanay
    void primitiveBeSnapshotFile();
    void primitivePosixFileOperation();
    void primitivePosixDirectoryOperation();
    void primitivePosixLastErrorOperation();
    void primitivePosixErrorStringOperation();

    
    // --- PrimitiveTest ---
    
    // success:
    inline void success(bool successValue)
    {
        /* "source"
         success <- successValue & success
         */
        successFlag = successFlag && successValue;
    }
    
    // dispatchPrimitives
    void dispatchPrimitives();
    
    // positive16BitValueOf:
    int positive16BitValueOf(int integerPointer);
    std::uint32_t positive32BitValueOf(int integerPointer);
    
    // initPrimitive
    inline void initPrimitive()
    {
        /* "source"
         success <- true
         */
    
        successFlag = true;
    }
    
    // success
    inline bool success()
    {
        /* "source"
         ^success
         */
        
        return successFlag;
    }
    
    // primitiveResponse
    bool primitiveResponse();
    
    // quickInstanceLoad
    void quickInstanceLoad();
    
    // arithmeticSelectorPrimitive
    void arithmeticSelectorPrimitive();
    
    // primitiveFail
    int primitiveFail()
    {
       /* "source"
        success <- false
       */
        
       successFlag = false;
       return 0; // invalid oop
    }
    
    // pushInteger:
    inline void pushInteger(int integerValue)
    {
       /* "source"
       	self push: (memory integerObjectOf: integerValue)
       */
        push(memory.integerObjectOf(integerValue));
    }
    
    // quickReturnSelf
    inline void quickReturnSelf()
    {
       /* "source"
       */
    
       // self is on stack top
    }
    
    // positive16BitIntegerFor:
    int positive16BitIntegerFor(int integerValue);
    int positive32BitIntegerFor(int integerValue);
    
    // popInteger
    int popInteger();
    
    // specialSelectorPrimitiveResponse
    int specialSelectorPrimitiveResponse();
    
    // commonSelectorPrimitive
    void commonSelectorPrimitive();
    
    
    // --- Initialization ---
    
    
    // initializeMethodCache
    void initializeMethodCache();
    
    
    // --- ArithmeticPrim ---
    
    // primitiveMod
    void primitiveMod();
    
    // dispatchArithmeticPrimitives
    void dispatchArithmeticPrimitives();
    
    // primitiveEqual
    void primitiveEqual();
    
    // primitiveBitOr
    void primitiveBitOr();
    
    // primitiveDivide
    void primitiveDivide();
    
    // primitiveMultiply
    void primitiveMultiply();
    
    // dispatchLargeIntegerPrimitives
    inline void dispatchLargeIntegerPrimitives()
    {
       /* "source"
       	self primitiveFail
       */
    
        primitiveFail();
    }
    
    // primitiveBitAnd
    void primitiveBitAnd();
    
    // primitiveSubtract
    void primitiveSubtract();
    
    // dispatchIntegerPrimitives
    void dispatchIntegerPrimitives();
    
    // primitiveGreaterOrEqual
    void primitiveGreaterOrEqual();
    
    // primitiveAdd
    void primitiveAdd();
    
    // primitiveNotEqual
    void primitiveNotEqual();
    
    // primitiveQuo
    void primitiveQuo();
    
    // dispatchFloatPrimitives
    void dispatchFloatPrimitives();
    
    void primitiveAsFloat();
    void primitiveFloatAdd();
    void primitiveFloatSubtract();
    void primitiveFloatLessThan();
    void primitiveFloatGreaterThan();
    void primitiveFloatLessOrEqual();
    void primitiveFloatGreaterOrEqual();
    void primitiveFloatEqual();
    void primitiveFloatNotEqual();
    void primitiveFloatMultiply();
    void primitiveFloatDivide();
    void primitiveTruncated();
    void primitiveFractionalPart();
    inline void primitiveExponent()
    {
        primitiveFail(); // optional
    }
    inline void primitiveTimesTwoPower()
    {
        primitiveFail(); // optional
    }
    
    // primitiveLessOrEqual
    void primitiveLessOrEqual();
    
    // primitiveMakePoint
    void primitiveMakePoint();
    
    // primitiveBitXor
    void primitiveBitXor();
    
    // primitiveLessThan
    void primitiveLessThan();
    
    // primitiveBitShift
    void primitiveBitShift();
    
    // primitiveGreaterThan
    void primitiveGreaterThan();
    
    // primitiveDiv
    void primitiveDiv();
    
    
    // --- SendBytecodes ---
    
    // sendSelector:argumentCount:
    void sendSelector_argumentCount(int selector, int count);
    
    // findNewMethodInClass:
    void findNewMethodInClass(int cls);
    
    // activateNewMethod
    void activateNewMethod();
    
    // sendSpecialSelectorBytecode
    void sendSpecialSelectorBytecode();
    
    // doubleExtendedSuperBytecode
    void doubleExtendedSuperBytecode();
    
    // sendBytecode
    void sendBytecode();
    
    // doubleExtendedSendBytecode
    void doubleExtendedSendBytecode();
    
    // sendSelectorToClass:
    void sendSelectorToClass(int classPointer);
    
    // sendLiteralSelectorBytecode
    void sendLiteralSelectorBytecode();
    
    // singleExtendedSuperBytecode
    void singleExtendedSuperBytecode();
    
    // singleExtendedSendBytecode
    void singleExtendedSendBytecode();
    
    // extendedSendBytecode
    void extendedSendBytecode();
    
    // executeNewMethod
    void executeNewMethod();
    
    
    // --- MainLoop ---
    
    // dispatchOnThisBytecode
    void dispatchOnThisBytecode();
    
    // fetchByte
    int fetchByte();

    // interpret
    void interpret();
    
private:
    // --- CompiledMethod ---
    
    // headerOf:
    inline int headerOf(int methodPointer)
    {
       /* "source"
       	^memory fetchPointer: HeaderIndex
       		ofObject: methodPointer
       */
    
        return memory.fetchPointer_ofObject(HeaderIndex, methodPointer);
    }
    
    // literalCountOf:
    inline int literalCountOf(int methodPointer)
    {
       /* "source"
       	^self literalCountOfHeader: (self headerOf: methodPointer)
       */
    
        return literalCountOfHeader(headerOf(methodPointer));
    }
    
    // primitiveIndexOf:
    int primitiveIndexOf(int methodPointer);
    
    // argumentCountOf:
    int argumentCountOf(int methodPointer);
    
    // literalCountOfHeader:
    inline int literalCountOfHeader(int headerPointer)
    {
       /* "source"
       	^self extractBits: 9 to: 14
       		of: headerPointer
       */
    
        return extractBits_to_of(9, 14, headerPointer);
    }
    
    // fieldIndexOf:
    inline int fieldIndexOf(int methodPointer)
    {
       /* "source"
       	^self extractBits: 3 to: 7
       		of: (self headerOf: methodPointer)
       */
        return extractBits_to_of(3, 7, headerOf(methodPointer));
    }
    
    // methodClassOf:
    int methodClassOf(int methodPointer);
    
    // literal:ofMethod:
    inline int literal_ofMethod(int offset, int methodPointer)
    {
       /* "source"
       	^memory fetchPointer: offset + LiteralStart
       		ofObject: methodPointer
       */
    
        return memory.fetchPointer_ofObject(offset + LiteralStart, methodPointer);
    }
    
    // temporaryCountOf:
    inline int temporaryCountOf(int methodPointer)
    {
       /* "source"
       	^self extractBits: 3 to: 7
       		of: (self headerOf: methodPointer)
       */
    
        return extractBits_to_of(3, 7, headerOf(methodPointer));
    }
    
    // largeContextFlagOf:
    inline int largeContextFlagOf(int methodPointer)
    {
       /* "source"
       	^self extractBits: 8 to: 8
       		of: (self headerOf: methodPointer)
       */
    
       return extractBits_to_of(8, 8, headerOf(methodPointer));
    }
    
    // objectPointerCountOf:
    inline int objectPointerCountOf(int methodPointer)
    {
       /* "source"
       	^(self literalCountOf: methodPointer) + LiteralStart
       */
    
        return literalCountOf(methodPointer) + LiteralStart;
    }
    
    // headerExtensionOf:
    inline int headerExtensionOf(int methodPointer)
    {
       int literalCount;
    
       /* "source"
       	literalCount <- self literalCountOf: methodPointer.
       	^self literal: literalCount - 2
       		ofMethod: methodPointer
       */
    
        literalCount = literalCountOf(methodPointer);
        return literal_ofMethod(literalCount - 2, methodPointer);
    }
    
    // flagValueOf:
    inline int flagValueOf(int methodPointer)
    {
       /* "source"
       	^self extractBits: 0 to: 2
       		of: (self headerOf: methodPointer)
       */
    
        return extractBits_to_of(0, 2, headerOf(methodPointer));
    }
    
    // initialInstructionPointerOfMethod:
    inline int initialInstructionPointerOfMethod(int methodPointer)
    {
       /* "source"
       	^((self literalCountOf: methodPointer) + LiteralStart) * 2 + 1
       */
    
        return (literalCountOf(methodPointer) + LiteralStart) * 2 + 1;
    }
    
    
    // --- StackBytecodes ---
    
    // pushLiteralVariableBytecode
    inline void pushLiteralVariableBytecode()
    {
       int fieldIndex;
    
       /* "source"
       	fieldIndex <- self extractBits: 11 to: 15
       			of: currentBytecode.
       	self pushLiteralVariable: fieldIndex
       */
        fieldIndex = extractBits_to_of(11, 15, currentBytecode);
        pushLiteralVariable(fieldIndex);
    }
    
    // pushLiteralConstant:
    inline void pushLiteralConstant(int literalIndex)
    {
       /* "source"
       	self push: (self literal: literalIndex)
       */
    
        push(literal(literalIndex));
    }
    
    // popStackBytecode
    inline void popStackBytecode()
    {
       /* "source"
       	self popStack
       */
        popStack();
    }
    
    // storeAndPopReceiverVariableBytecode
    void storeAndPopReceiverVariableBytecode();
    
    // extendedStoreBytecode
    void extendedStoreBytecode();
    
    // pushLiteralConstantBytecode
    void pushLiteralConstantBytecode();
    
    // storeAndPopTemporaryVariableBytecode
    void storeAndPopTemporaryVariableBytecode();
    
    // extendedStoreAndPopBytecode
    void extendedStoreAndPopBytecode();
    
    // pushReceiverBytecode
    inline void pushReceiverBytecode()
    {
       /* "source"
       	self push: receiver
       */
    
        push(receiver);
    }
    
    // duplicateTopBytecode
    inline void duplicateTopBytecode()
    {
       /* "source"
       	^self push: self stackTop
       */
    
        push(stackTop());
    }
    
    // pushReceiverVariableBytecode
    inline void pushReceiverVariableBytecode()
    {
       int fieldIndex;
    
       /* "source"
       	fieldIndex <- self extractBits: 12 to: 15
       			of: currentBytecode.
       	self pushReceiverVariable: fieldIndex
       */
    
        fieldIndex = extractBits_to_of(12, 15, currentBytecode);
        pushReceiverVariable(fieldIndex);
    }
    
    // pushActiveContextBytecode
    inline void pushActiveContextBytecode()
    {
       /* "source"
       	self push: activeContext
       */
        push(activeContext);
    }
    
    // stackBytecode
    void stackBytecode();
    
    // extendedPushBytecode
    void extendedPushBytecode();
    
    // pushTemporaryVariable:
    inline void pushTemporaryVariable(int temporaryIndex)
    {
       /* "source"
       	self push: (self temporary: temporaryIndex)
       */
    
        push(temporary(temporaryIndex));
    }
    
    // pushReceiverVariable:
    inline void pushReceiverVariable(int fieldIndex)
    {
       /* "source"
       	self push: (memory fetchPointer: fieldIndex
       			ofObject: receiver)
       */
        push(memory.fetchPointer_ofObject(fieldIndex, receiver));
    }
    
    // pushConstantBytecode
    void pushConstantBytecode();
    
    // pushTemporaryVariableBytecode
    inline void pushTemporaryVariableBytecode()
    {
       int fieldIndex;
    
       /* "source"
       	fieldIndex <- self extractBits: 12 to: 15
       			of: currentBytecode.
       	self pushTemporaryVariable: fieldIndex
       */
    
        fieldIndex = extractBits_to_of(12, 15, currentBytecode);
        pushTemporaryVariable(fieldIndex);
    }
    
    // pushLiteralVariable:
    inline void pushLiteralVariable(int literalIndex)
    {
       int association;
    
       /* "source"
       	association <- self literal: literalIndex.
       	self push: (memory fetchPointer: ValueIndex
       			ofObject: association)
       */
        association = literal(literalIndex);
        push(memory.fetchPointer_ofObject(ValueIndex, association));
    }
    
    
    // --- JumpBytecodes ---
    
    // sendMustBeBoolean
    inline void sendMustBeBoolean()
    {
       /* "source"
       	self sendSelector: MustBeBooleanSelector
       		argumentCount: 0
       */
    
        sendSelector_argumentCount(MustBeBooleanSelector, 0);
    }
    
    // shortUnconditionalJump
    inline void shortUnconditionalJump()
    {
       int offset;
    
       /* "source"
       	offset <- self extractBits: 13 to: 15
       			of: currentBytecode.
       	self jump: offset + 1
       */
    
        offset = extractBits_to_of(13, 15, currentBytecode);
        jump(offset + 1);
    }
    
    // jump:
    inline void jump(int offset)
    {
       /* "source"
       	instructionPointer <- instructionPointer + offset
       */
    
        instructionPointer = instructionPointer + offset;
    }
    
    // jumpIf:by:
    void jumpIf_by(int condition, int offset);
    
    // longConditionalJump
    void longConditionalJump();
    
    // longUnconditionalJump
    inline void longUnconditionalJump()
    {
       int offset;
    
       /* "source"
       	offset <- self extractBits: 13 to: 15
       			of: currentBytecode.
       	self jump: offset - 4 * 256 + self fetchByte
       */
        offset = extractBits_to_of(13, 15, currentBytecode);
        jump((offset - 4) * 256 + fetchByte());
    }
    
    // jumpBytecode
    void jumpBytecode();
    
    // shortConditionalJump
    inline void shortConditionalJump()
    {
       int offset;
    
       /* "source"
       	offset <- self extractBits: 13 to: 15
       			of: currentBytecode.
       	self jumpIf: FalsePointer
       		by: offset + 1
       */
    
        offset = extractBits_to_of(13, 15, currentBytecode);
        jumpIf_by(FalsePointer, offset + 1);
    }
    
    
    // --- IntegerAccess ---
    
    // storeInteger:ofObject:withValue:
    void storeInteger_ofObject_withValue(int fieldIndex, int objectPointer, int integerValue);
    
    // extractBits:to:of:
    inline int extractBits_to_of(int firstBitIndex, int lastBitIndex, int anInteger)
    {
        /* "source"
         ^(anInteger bitShift: lastBitIndex - 15)
         bitAnd: (2 raisedTo: lastBitIndex - firstBitIndex + 1) - 1
         */
        
        std::uint16_t mask = (1 << (lastBitIndex - firstBitIndex + 1)) - 1;
        std::uint16_t shift = anInteger >> (15-lastBitIndex);
        return shift & mask;
        
    }
    
    // transfer:fromIndex:ofObject:toIndex:ofObject:
    void transfer_fromIndex_ofObject_toIndex_ofObject(
                                                      int count,
                                                      int firstFrom,
                                                      int fromOop,
                                                      int firstTo,
                                                      int toOop
                                                      );
    
    // lowByteOf:
    inline int lowByteOf(int anInteger)
    {
       /* "source"
       	^self extractBits: 8 to: 15
       		of: anInteger
       */
    
        return extractBits_to_of(8, 15, anInteger);
    }
    
    // fetchInteger:ofObject:
    int fetchInteger_ofObject(int fieldIndex, int objectPointer);

    // highByteOf:
    inline int highByteOf(int anInteger)
    {
       /* "source"
       	^self extractBits: 0 to: 7
       		of: anInteger
       */
        return extractBits_to_of(0, 7, anInteger);
    }
    
    
    // --- StoreMgmtPrims ---
    
    // checkInstanceVariableBoundsOf:in:
    inline void checkInstanceVariableBoundsOf_in(int index, int object)
    {    
       /* "source"
       	class <- memory fetchClassOf: object.
       	self success: index >= 1.
       	self success: index <= (self lengthOf: object)
       */
        //cls = memory.fetchClassOf(object);
        success(index >= 1);
        success(index <= lengthOf(object));
    }
    
    // primitiveNewMethod
    void primitiveNewMethod();
    
    // primitiveAsOop
    void primitiveAsOop();
    
    // primitiveSomeInstance
    void primitiveSomeInstance();
    
    // primitiveObjectAt
    void primitiveObjectAt();
    
    // primitiveNextInstance
    void primitiveNextInstance();
    
    // primitiveNew
    void primitiveNew();
    
    // primitiveAsObject
    void primitiveAsObject();
    
    // primitiveNewWithArg
    void primitiveNewWithArg();
    
    // primitiveInstVarAtPut
    void primitiveInstVarAtPut();
    
    // primitiveObjectAtPut
    void primitiveObjectAtPut();
    
    // primitiveInstVarAt
    void primitiveInstVarAt();
    
    // primitiveBecome
    void primitiveBecome();
    
    // dispatchStorageManagementPrimitives
    void dispatchStorageManagementPrimitives();
    
    // --Float Access--
    void pushFloat(float f);
    
    inline float extractFloat(int objectPointer)
    {
        std::uint32_t uint32 = (memory.fetchWord_ofObject(1, objectPointer) << 16) | memory.fetchWord_ofObject(0, objectPointer);
        return * (float *) &uint32;
    }

    float popFloat();
    
    
    bool isInLowMemoryCondition();
    
#ifdef GC_MARK_SWEEP
    void prepareForCollection();
    void collectionCompleted();
#endif
    
private:
    // initializePointIndices
    static const int XIndex = 0;
    static const int YIndex = 1;
    static const int ClassPointSize = 2;
    
    // initializeStreamIndices
    static const int StreamArrayIndex = 0;
    static const int StreamIndexIndex = 1;
    static const int StreamReadLimitIndex = 2;
    static const int StreamWriteLimitIndex = 3;
    
    // initializeSchedulerIndices
    // Class ProcessorScheduler
    static const int ProcessListsIndex = 0;
    static const int ActiveProcessIndex = 1;
    // Class LinkedList
    static const int FirstLinkIndex = 0;
    static const int LastLinkIndex = 1;
    // Class Semaphore
    static const int ExcessSignalsIndex = 2;
    // Class Link
    static const int NextLinkIndex = 0;
    // Class Process
    static const int SuspendedContextIndex = 1;
    static const int PriorityIndex = 2;
    static const int MyListIndex = 3;
    
    // initializeMessageIndices
    static const int MessageSelectorIndex = 0;
    static const int MessageArgumentsIndex = 1;
    static const int MessageSize = 2;
    
    // initializeClassIndices
    // Class Class
    static const int SuperclassIndex = 0;
    static const int MessageDictionaryIndex = 1;
    static const int InstanceSpecificationIndex = 2;
    // Fields of a message dictionary
    static const int MethodArrayIndex = 1;
    static const int SelectorStart = 2;
    
    // initializeSmallIntegers
    // SmallIntegers"
    static const int MinusOnePointer = 65535;
    static const int ZeroPointer = 1;
    static const int OnePointer = 3;
    static const int TwoPointer = 5;
    
    // initializeContextIndices
    // Class MethodContext
    static const int SenderIndex = 0;
    static const int InstructionPointerIndex = 1;
    static const int StackPointerIndex = 2;
    static const int MethodIndex = 3;
    static const int ReceiverIndex = 5;
    static const int TempFrameStart = 6;
    // Class BlockContext
    static const int CallerIndex = 0;
    static const int BlockArgumentCountIndex = 3;
    static const int InitialIPIndex = 4;
    static const int HomeIndex = 5;
    
    // initializeAssociationIndex
    static const int ValueIndex = 1;
    
    // initializeCharacterIndex
    static const int CharacterValueIndex = 0;
    
    // initializeMethodIndices
    // Class CompiledMethod
    static const int HeaderIndex = 0;
    static const int LiteralStart = 1;
    
    // Forms
    static const int BitsInForm = 0;
    static const int WidthInForm = 1;
    static const int HeightInForm = 2;
    static const int OffsetInForm = 3;
    
    // Files
    static const int FileNameIndex = 1;    // fileName field of File
    
    // "Registers"
    int activeContext;
    int homeContext;
    int method;
    int receiver;
    int instructionPointer;
    int stackPointer;
    int currentBytecode;
    bool successFlag;
    
    // Class related registers
    int messageSelector;
    int argumentCount;
    int newMethod;
    int primitiveIndex;
    
    
    // Process-related Registers (pg 642)
    bool newProcessWaiting;  // The newProcessWaiting register will be true if a process switch is called for and false otherwise.
    int newProcess; // If newProcessWaiting is true then the newProcess register will point to the Process to be transferred to.
    /*
     The semaphoreList register points to an Array used by the interpreter to buffer Semaphores that should be signaled. This is an Array in Interpreter, not in the object memory. It will be a table in a machine-language interpreter.
     */
    
    int semaphoreList[4096];
    
    int semaphoreIndex; // The semaphoreIndex register hold the index of the last Semaphore in the semaphoreList buffer.
    
    // Using an array of int for method cache to remain faithful as possible to the bluebook
    // Any size change will require changes to hash function in findNewMethodInClass
    int methodCache[1024];
    
    ObjectMemory memory;
    
    // dbanay - primitiveSignalAtOopsLeftWordsLeft support
    bool checkLowMemory;
    bool memoryIsLow;
    int lowSpaceSemaphore;
    int oopsLeftLimit;
    std::uint32_t wordsLeftLimit;
    
    IHardwareAbstractionLayer *hal;
    IFileSystem *fileSystem;
    int currentDisplay;
    int currentDisplayWidth;
    int currentDisplayHeight;
    int currentCursor;
    
    // Return a std::string for a string or symbol oop
    std::string stringFromObject(int strOop);
    int stringObjectFor(const char *s);

#ifdef DEBUGGING_SUPPORT
    std::string selectorName(int selector);
    std::string classNameOfObject(int objectPointer);
    std::string className(int classPointer);
#endif
    
};

