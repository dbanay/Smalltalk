//
//  interpreter.cpp
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


#include <cstdint>
#include <cassert>
#include <cstdlib>
#include <cmath>
#include <limits>
#include "oops.h"
#include "interpreter.h"
#include "bitblt.h"


inline bool between_and(int value, int min, int max )
{
    return value >= min && value <= max;
}

Interpreter::Interpreter(IHardwareAbstractionLayer *halInterface, IFileSystem *fileSystemInterface) : hal(halInterface), fileSystem(fileSystemInterface),
#ifdef GC_MARK_SWEEP
   memory(halInterface, this)
#else
   memory(halInterface)
#endif
{
}

bool Interpreter::init()
{
    initializeMethodCache();
    semaphoreIndex = -1;
    if (!memory.loadSnapshot(fileSystem, hal->get_image_name()))
        return false;
    /*
     When Smalltalk is started up, the initial active context is found through the scheduler's active Process.
     (G&R pg. 644)
     */
    activeContext = firstContext();
    memory.increaseReferencesTo(activeContext);
    fetchContextRegisters();
    checkLowMemory = false;
    memoryIsLow = false;
    lowSpaceSemaphore = NilPointer;
    oopsLeftLimit = 0;
    wordsLeftLimit = 0;
    currentDisplay = 0;
    currentCursor = 0;
    currentDisplayWidth = 0;
    currentDisplayHeight = 0;
    return true;
}

void Interpreter::error(const char *message)
{
    hal->error(message);
}


#ifdef GC_MARK_SWEEP
void Interpreter::prepareForCollection()
{
    storeContextRegisters();
    memory.addRoot(SmalltalkPointer);
    memory.addRoot(activeContext);
    if (newProcess != NilPointer)
    {
        memory.addRoot(newProcess);
    }
    
}
void Interpreter::collectionCompleted()
{
    memory.increaseReferencesTo(activeContext);
    fetchContextRegisters();
    if (newProcessWaiting)
    {
        memory.increaseReferencesTo(newProcess);
    }
}
#endif

// primitiveAtEnd
void Interpreter::primitiveAtEnd()
{
#ifndef IMPLEMENT_PRIMITIVE_AT_END
    primitiveFail();
#else
   int stream;
    int array;
    int arrayClass;
    int length;
    int index;
    int limit;

   /* "source"
   	stream <- self popStack.
   	array <- memory fetchPointer: StreamArrayIndex
   			ofObject: stream.
   	arrayClass <- memory fetchClassOf: array.
   	length <- self lengthOf: array.
   	index <- self fetchInteger: StreamIndexIndex
   			ofObject: stream.
   	limit <- self fetchInteger: StreamReadLimitIndex
   			ofObject: stream. "ERROR dbanay : was Stream"
   	self success:
   		(arrayClass=ClassArrayPointer) | (arrayClass=ClassStringPointer).
   	self success
   		ifTrue: [(index >= limit) | (index >= length)
   				ifTrue: [self push: TruePointer]
   				ifFalse: [self push: FalsePointer]]
   		ifFalse: [self unPop: 1]
   */
    stream = popStack();
    array = memory.fetchPointer_ofObject(StreamArrayIndex, stream);
    arrayClass = memory.fetchClassOf(array);
    length = lengthOf(array);
    index = fetchInteger_ofObject(StreamIndexIndex, stream);
    limit = fetchInteger_ofObject(StreamReadLimitIndex, stream);
    success(arrayClass == ClassArrayPointer || arrayClass == ClassStringPointer);
    if (success())
    {
        if (index >= limit || index >= length)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
    {
        unPop(1);
    }
#endif
}


// checkIndexableBoundsOf:in:
void Interpreter::checkIndexableBoundsOf_in(int index, int array)
{

   /* "source"
    "dbanay - this is totally wrong"
   	class <- memory fetchClassOf: array.
   	self success: index >= 1.
   	self success: index + (self fixedFieldsOf: class) <= (self lengthOf: array)
   */
    /* "source corrected"
     self success: index >= 1.
     self success: index <= (self lengthOf: array)
     */
    success(index >= 1);
    success(index <= lengthOf(array));
}


// primitiveNextPut
void Interpreter::primitiveNextPut()
{
#ifndef IMPLEMENT_PRIMITIVE_NEXT_PUT
   primitiveFail();
    
#else
    int value;
    int stream;
    int index;
    int limit;
    int array;
    int arrayClass;
    int ascii;

   /* "source"
   	value <- self popStack.
   	stream <- self popStack.
   	array <- memory fetchPointer: StreamArrayIndex
   			ofObject: stream.
   	arrayClass <- memory fetchClassOf: array.
   	index <- self fetchInteger: StreamIndexIndex
   			ofObject: stream.
   	limit <- self fetchInteger: StreamWriteLimitIndex
   			ofObject: stream.
   	self success: index < limit.
   	self success:
   		(arrayClass=ClassArrayPointer) | (arrayClass=ClassStringPointer).
   	self checkIndexableBoundsOf: index + 1
   		in: array.
   	self success
   		ifTrue: [index <- index + 1.
   			arrayClass = ClassArrayPointer
   				ifTrue: [self subscript: array
   						with: index
   						storing: value]
   				ifFalse: [ascii <- memory fetchPointer: CharacterValueIndex
   							ofObject: value.
   					self subscript: array
   						with: index
   						storing: ascii]].
   	self success
   		ifTrue: [self storeInteger: StreamIndexIndex
   				ofObject: stream
   				withValue: index].
   	self success
   		ifTrue: [self push: value]
   		ifFalse: [self unPop: 2]
   */
   

    value = popStack();
    stream = popStack();
    array = memory.fetchPointer_ofObject(StreamArrayIndex, stream);
    arrayClass = memory.fetchClassOf(array);
    index = fetchInteger_ofObject(StreamIndexIndex, stream);
    limit = fetchInteger_ofObject(StreamWriteLimitIndex,stream);
    success(index < limit);
    success(arrayClass==ClassArrayPointer || arrayClass==ClassStringPointer);
    checkIndexableBoundsOf_in(index + 1, array);
    if (success())
    {
        index = index + 1;
        if (arrayClass == ClassArrayPointer)
        {
            subscript_with_storing(array, index, value);
        }
        else
        {
            ascii = memory.fetchPointer_ofObject(CharacterValueIndex, value);
            subscript_with_storing(array, index, ascii);
        }
     }
    if (success())
        storeInteger_ofObject_withValue(StreamIndexIndex, stream, index);
    if (success())
        push(value);
    else
        unPop(2);
#endif
}


// lengthOf:
int Interpreter::lengthOf(int array)
{
   /* "source"
   	(self isWords: (memory fetchClassOf: array))
   		ifTrue: [^memory fetchWordLengthOf: array]
   		ifFalse: [^memory fetchByteLengthOf: array]
   */
   
    return  isWords(memory.fetchClassOf(array))
        ?  memory.fetchWordLengthOf(array) :
           memory.fetchByteLengthOf(array);
}


// primitiveNext
void Interpreter::primitiveNext()
{
#ifndef IMPLEMENT_PRIMITIVE_NEXT
    primitiveFail();

#else
    int stream;
    int index;
    int limit;
    int array;
    int arrayClass;
    int result = 0;
    int ascii;

   /* "source"
   	stream <- self popStack.
   	array <- memory fetchPointer: StreamArrayIndex
   			ofObject: stream.
   	arrayClass <- memory fetchClassOf: array.
   	index <- self fetchInteger: StreamIndexIndex
   			ofObject: stream.
   	limit <- self fetchInteger: StreamReadLimitIndex
   			ofObject: stream.
   	self success: index < limit.
   	self success:
   		(arrayClass=ClassArrayPointer) | (arrayClass=ClassStringPointer).
   	self checkIndexableBoundsOf: index + 1
   		in: array.
   	self success
   		ifTrue: [index <- index + 1.
   			result <- self subscript: array
   					with: index].
   	self success
   		ifTrue: [self storeInteger: StreamIndexIndex
   				ofObject: stream
   				withValue: index].
   	self success
   		ifTrue: [arrayClass = ClassArrayPointer
   				ifTrue: [self push: result]
   				ifFalse: [ascii <- memory integerValueOf: result.
   					self push: (memory fetchPointer: ascii
   							ofObject: CharacterTablePointer)]]
   		ifFalse: [self unPop: 1]
   */
    stream = popStack();
    array = memory.fetchPointer_ofObject(StreamArrayIndex, stream);
    arrayClass = memory.fetchClassOf(array);
    index = fetchInteger_ofObject(StreamIndexIndex, stream);
    limit = fetchInteger_ofObject(StreamReadLimitIndex, stream);
    success(index < limit);
    success(arrayClass == ClassArrayPointer || arrayClass == ClassStringPointer);
    checkIndexableBoundsOf_in(index + 1, array);
    if (success())
    {
        index = index + 1;
        result = subscript_with(array, index);
    }
    if (success())
    {
        storeInteger_ofObject_withValue(StreamIndexIndex, stream, index);
    }
    if (success())
    {
        if (arrayClass == ClassArrayPointer)
        {
            push(result);
        }
        else
        {
            ascii = memory.integerValueOf(result);
            push(memory.fetchPointer_ofObject(ascii, CharacterTablePointer));
        }
    }
    else
        unPop(1);
#endif
}


// dispatchSubscriptAndStreamPrimitives
void Interpreter::dispatchSubscriptAndStreamPrimitives()
{
   /* "source"
   	primitiveIndex = 60 ifTrue: [^self primitiveAt].
   	primitiveIndex = 61 ifTrue: [^self primitiveAtPut].
   	primitiveIndex = 62 ifTrue: [^self primitiveSize].
   	primitiveIndex = 63 ifTrue: [^self primitiveStringAt].
   	primitiveIndex = 64 ifTrue: [^self primitiveStringAtPut].
   	primitiveIndex = 65 ifTrue: [^self primitiveNext].
   	primitiveIndex = 66 ifTrue: [^self primitiveNextPut].
   	primitiveIndex = 67 ifTrue: [^self primitiveAtEnd]
   */
    switch (primitiveIndex)
    {
        case 60: primitiveAt(); break;
        case 61: primitiveAtPut(); break;
        case 62: primitiveSize(); break;
        case 63: primitiveStringAt(); break;
        case 64: primitiveStringAtPut(); break;
        case 65: primitiveNext(); break;
        case 66: primitiveNextPut(); break;
        case 67: primitiveAtEnd(); break;
    }
}


// primitiveStringAt
void Interpreter::primitiveStringAt()
{
   int index;
   int array;
   int ascii;
   int character = 0;

   /* "source"
   	index <- self positive16BitValueOf: self popStack.
   	array <- self popStack.
   	self checkIndexableBoundsOf: index
   		in: array.
   	self success
   		ifTrue: [ascii <- memory integerValueOf: (self subscript: array
   								with: index).
   			character <- memory fetchPointer: ascii
   					ofObject: CharacterTablePointer].
   	self success
   		ifTrue: [self push: character]
   		ifFalse: [self unPop: 2]
   */
    
    index = positive16BitValueOf(popStack());
    array = popStack();
    checkIndexableBoundsOf_in(index, array);
    if (success())
    {
        ascii  = memory.integerValueOf(subscript_with(array, index));
        character = memory.fetchPointer_ofObject(ascii, CharacterTablePointer);
    }
    
    if (success())
    {
        push(character);
    }
    else
        unPop(2);
}


// primitiveAt
void Interpreter::primitiveAt()
{
   int index;
   int array;
   int arrayClass;
   int result = 0;

   /* "source"
   	index <- self positive16BitValueOf: self popStack.
   	array <- self popStack.
   	arrayClass <- memory fetchClassOf: array.
   	self checkIndexableBoundsOf: index
   		in: array.
   	self success
   		ifTrue: [index <- index + (self fixedFieldsOf: arrayClass).
   			result <- self subscript: array
   					with: index].
   	self success
   		ifTrue: [self push: result]
   		ifFalse: [self unPop: 2]
   */
    index = positive16BitValueOf(popStack());
    array = popStack();
    arrayClass = memory.fetchClassOf(array);
    checkIndexableBoundsOf_in(index, array);
    
    if (success())
    {
        index = index + fixedFieldsOf(arrayClass);
        result = subscript_with(array, index);
    }
    
    if (success())
    {
        push(result);
    }
    else
        unPop(2);
}


// primitiveSize
void Interpreter::primitiveSize()
{
   int array;
   int cls;
   int length = 0;

   /* "source"
   	array <- self popStack.
    "dbanay ERROR make sure it is a pointer!"
    self success: memory isIntegerObject: array.
    self success ifTrue: [ class <- memory fetchClassOf: array.
                           length <- self positive16BitIntegerFor:
                        (self lengthOf: array) - (self fixedFieldsOf: class)].
   	self success
   		ifTrue: [self push: length]
   		ifFalse: [self unPop: 1]
   */

    array = popStack();
    success(!memory.isIntegerObject(array));
    if (success())
    {
        cls = memory.fetchClassOf(array);
        length = positive16BitIntegerFor(lengthOf(array) - fixedFieldsOf(cls));
    }
    if (success())
        push(length);
    else
        unPop(1);
}


// primitiveStringAtPut
void Interpreter::primitiveStringAtPut()
{
   int index;
   int array;
   int ascii;
   int character;

   /* "source"
   	character <- self popStack.
   	index <- self positive16BitValueOf: self popStack.
   	array <- self popStack.
   	self checkIndexableBoundsOf: index
   		in: array.
   	self success: (memory fetchClassOf: character) = ClassCharacterPointer.
   	self success
   		ifTrue: [ascii <- memory fetchPointer: CharacterValueIndex
   					ofObject: character.
   			self subscript: array
   				with: index
   				storing: ascii].
   	self success
   		ifTrue: [self push: character]
   		ifFalse: [self unPop: 2]  "dbanay ERROR should be 3"
   */
    character = popStack();
    index = positive16BitValueOf(popStack());
    array = popStack();
    checkIndexableBoundsOf_in(index, array);
    success(memory.fetchClassOf(character) == ClassCharacterPointer);
    if (success())
    {
        ascii  = memory.fetchPointer_ofObject(CharacterValueIndex, character);
        subscript_with_storing(array, index, ascii);
    }
    if (success())
        push(character);
    else
        unPop(3);
}


// subscript:with:
int Interpreter::subscript_with(int array, int index)
{
   int cls;
   int value;

   /* "source"
   	class <- memory fetchClassOf: array.
   	(self isWords: class)
   		ifTrue: [(self isPointers: class)
   				ifTrue: [^memory fetchPointer: index - 1
   						ofObject: array]
   				ifFalse: [value <- memory fetchWord: index - 1
   						ofObject: array.
   					^self positive16BitIntegerFor: value]]
   		ifFalse: [value <- memory fetchByte: index - 1
   					ofObject: array.
   			^memory integerObjectOf: value]
   */
    cls = memory.fetchClassOf(array);
    if (isWords(cls))
    {
        if (isPointers(cls))
        {
            return memory.fetchPointer_ofObject(index - 1, array);
        }
        else
        {
            value = memory.fetchWord_ofObject(index - 1, array);
            return positive16BitIntegerFor(value);
        }
    }
    
    value = memory.fetchByte_ofObject(index - 1, array);
    return memory.integerObjectOf(value);
}


// primitiveAtPut
void Interpreter::primitiveAtPut()
{
   int array;
   int index;
   int arrayClass;
   int value;

   /* "source"
   	value <- self popStack.
   	index <- self positive16BitValueOf: self popStack.
   	array <- self popStack.
   	arrayClass <- memory fetchClassOf: array.
   	self checkIndexableBoundsOf: index.
   		in: array.
   	self success
   		ifTrue: [index <- index + (self fixedFieldsOf: arrayClass).
   			self subscript: array
   				with: index
   				storing: value].
   	self success
   		ifTrue: [self push: value]
   		ifFalse: [self unPop: 3]
   */
    value = popStack();
    index = positive16BitValueOf(popStack());
    array = popStack();
    arrayClass = memory.fetchClassOf(array);
    checkIndexableBoundsOf_in(index, array);
    if (success())
    {
        index = index + fixedFieldsOf(arrayClass);
        subscript_with_storing(array, index, value);
    }
    
    if (success())
        push(value);
    else
        unPop(3);
}


// subscript:with:storing:
void Interpreter::subscript_with_storing(int array, int index, int value)
{
   int cls;

   /* "source"
   	class <- memory fetchClassOf: array.
   	(self isWords: class)
   		ifTrue: [(self isPointers: class)
   				ifTrue: [^memory storePointer: index - 1
   						ofObject: array
   						withValue: value]
   				ifFalse: [self success: (memory isIntegerObject: value).
   					self success: ifTrue:
   						[^memory
   							storeWord: index-1
   							ofObject: array
   							withValue: (self positive16BitValueOf:
   								value)]]]
   		ifFalse: [self success: (memory isIntegerObject: value). "ERROR: dbanay check for large pos as well"
   			self success ifTrue:
   				[^memory storeByte: index - 1
   					ofObject: array
   					withValue: (self lowByteOf:
   							(memory integerValueOf:
   								value))]]
   */
   
    cls = memory.fetchClassOf(array);
    if (isWords(cls))
    {
        if (isPointers(cls))
        {
            memory.storePointer_ofObject_withValue(index - 1, array, value);
        }
        else
        {
            success(memory.isIntegerObject(value) || memory.fetchClassOf(value) == ClassLargePositiveIntegerPointer);
            if (success())
            {
                memory.storeWord_ofObject_withValue(index - 1, array, positive16BitValueOf(value));
            }
        }
    }
    else
    {
        success(memory.isIntegerObject(value));
        if (success())
        {
            memory.storeByte_ofObject_withValue(index - 1, array, lowByteOf(memory.integerValueOf(value)));
        }
    }
}


/*
 The instruction pointer stored in a context is a one-relative index to the method's fields because subscripting in Smalltalk (i.e., the at: message) takes one-relative indices. The memory, however, uses zero-relative indices; so the fetchContextRegisters routine subtracts one to convert it to a memory index and the storeContextRegisters routine adds the one back in.
*/
// storeContextRegisters
void Interpreter::storeContextRegisters()
{
   /* "source"
   	self storeInstructionPointerValue: instructionPointer + 1
   		inContext: activeContext.
   	self storeStackPointerValue: stackPointer - TempFrameStart + 1
   		inContext: activeContext
   */
   
    storeInstructionPointerValue_inContext(instructionPointer + 1, activeContext);
    storeStackPointerValue_inContext(stackPointer - TempFrameStart + 1, activeContext);
}

// isBlockContext:
bool Interpreter::isBlockContext(int contextPointer)
{
   int methodOrArguments;

   /* "source"
   	methodOrArguments <- memory fetchPointer: MethodIndex
   				ofObject: contextPointer.
   	^memory isIntegerObject: methodOrArguments
   */
    methodOrArguments = memory.fetchPointer_ofObject(MethodIndex,
                                    contextPointer);
    return memory.isIntegerObject(methodOrArguments);

}

// newActiveContext:
void Interpreter::newActiveContext(int aContext)
{
   /* "source"
   	self storeContextRegisters.
   	memory decreaseReferencesTo: activeContext.
   	activeContext <- aContext.
   	memory increaseReferencesTo: activeContext.
   	self fetchContextRegisters
   */
        
    storeContextRegisters();
    memory.decreaseReferencesTo(activeContext);
    activeContext = aContext;
    memory.increaseReferencesTo(activeContext);
    fetchContextRegisters();
}

/*
 The instruction pointer stored in a context is a one-relative index to the method's fields because subscripting in Smalltalk (i.e., the at: message)takes one-relative indices. The memory, however, uses zero-relative indices; so the fetchContextRegisters routine subtracts one to convert it to a memory index and the storeContextRegisters routine adds the one back in.
*/

// fetchContextRegisters
void Interpreter::fetchContextRegisters()
{
   /* "source"
   	(self isBlockContext: activeContext)
   		ifTrue: [homeContext <- memory fetchPointer: HomeIndex
   						ofObject: activeContext]
   		ifFalse: [homeContext <- activeContext].
   	receiver <- memory fetchPointer: ReceiverIndex
   			ofObject: homeContext.
   	method <- memory fetchPointer: MethodIndex
   			ofObject: homeContext.
   	instructionPointer <- (self instructionPointerOfContext: activeContext) - 1.
   	stackPointer <-
   		(self stackPointerOfContext: activeContext) + TempFrameStart - 1
   */
    
    if (isBlockContext(activeContext))
        homeContext = memory.fetchPointer_ofObject(HomeIndex, activeContext);
    else
        homeContext = activeContext;
    receiver = memory.fetchPointer_ofObject(ReceiverIndex, homeContext);
    method = memory.fetchPointer_ofObject(MethodIndex, homeContext);
    instructionPointer = instructionPointerOfContext(activeContext) - 1;
    stackPointer = stackPointerOfContext(activeContext) + TempFrameStart - 1;
}

// dispatchInputOutputPrimitives
void Interpreter::dispatchInputOutputPrimitives()
{
   /* "source"
   	primitiveIndex = 90 ifTrue: [^self primitiveMousePoint].
   	primitiveIndex = 91 ifTrue: [^self primitiveCursorLocPut].
   	primitiveIndex = 92 ifTrue: [^self primitiveCursorLink].
   	primitiveIndex = 93 ifTrue: [^self primitiveInputSemaphore].
   	primitiveIndex = 94 ifTrue: [^self primitiveSampleInterval].
   	primitiveIndex = 95 ifTrue: [^self primitiveInputWord].
   	primitiveIndex = 96 ifTrue: [^self primitiveCopyBits].
   	primitiveIndex = 97 ifTrue: [^self primitiveSnapshot].
   	primitiveIndex = 98 ifTrue: [^self primitiveTimeWordsInto].
   	primitiveIndex = 99 ifTrue: [^self primitiveTickWordsInto].
   	primitiveIndex = 100 ifTrue: [^self primitiveSignalAtTick].
   	primitiveIndex = 101 ifTrue: [^self primitiveBeCursor].
   	primitiveIndex = 102 ifTrue: [^self primitiveBeDisplay].
   	primitiveIndex = 103 ifTrue: [^self primitiveScanCharacters].
   	primitiveIndex = 104 ifTrue: [^self primitiveDrawLoop].
   	primitiveIndex = 105 ifTrue: [^self primitiveStringReplace]
   */
    
    switch (primitiveIndex)
    {
        case  90: primitiveMousePoint(); break;
        case  91: primitiveCursorLocPut(); break;
        case  92: primitiveCursorLink(); break;
        case  93: primitiveInputSemaphore(); break;
        case  94: primitiveSampleInterval(); break;
        case  95: primitiveInputWord(); break;
        case  96: primitiveCopyBits(); break;
        case  97: primitiveSnapshot(); break;
        case  98: primitiveTimeWordsInto(); break;
        case  99: primitiveTickWordsInto(); break;
        case  100: primitiveSignalAtTick(); break;
        case  101: primitiveBeCursor(); break;
        case  102: primitiveBeDisplay(); break;
        case  103: primitiveScanCharacters(); break;
        case  104: primitiveDrawLoop(); break;
        case  105: primitiveStringReplace(); break;
    }
}

void Interpreter::primitiveMousePoint()
{
    /*
     Poll the mouse to find out its position.  Return a Point.  Fail if event-driven
     tracking is used instead of polling.
     */
    int mouseX, mouseY;

    pop(1);  // remove receiver
    hal->get_cursor_location(&mouseX, &mouseY);
    int pointResult = memory.instantiateClass_withPointers(ClassPointPointer, ClassPointSize);
    storeInteger_ofObject_withValue(XIndex, pointResult, mouseX);
    storeInteger_ofObject_withValue(YIndex, pointResult, mouseY);
    push(pointResult);
}

void Interpreter::primitiveCursorLocPut()
{
    /*
     Move the cursor to the screen location specified by the argument. Fail if
     the argument is not a Point.
     */
    int point = popStack();
    success(memory.fetchClassOf(point) == ClassPointPointer);
    if (success())
    {
        int x = fetchInteger_ofObject(XIndex, point);
        int y = fetchInteger_ofObject(YIndex, point);
        hal->set_cursor_location(x, y);
    }
    else
        unPop(1);
}

void Interpreter::primitiveCursorLink()
{
    /*
     Cause the cursor to track the pointing device location if the argument is true.
     Decouple the cursor from the pointing device if the argument is false.
     */
    int flag = popStack();
    hal->set_link_cursor(flag == TruePointer);
}

void Interpreter::primitiveInputSemaphore()
{
    /*
     Install the argument (a Semaphore) as the object to be signalled whenever
     an input event occurs. The semaphore will be signaled once for every
     word placed in the input buffer by an I/O device. Fail if the argument is
     neither a Semaphore nor nil.
     */
    int semaphore = popStack();
    success(semaphore == NilPointer || memory.fetchClassOf(semaphore) == ClassSemaphorePointer);
    if (success())
        hal->set_input_semaphore(semaphore);
    else
        unPop(1);
}

void Interpreter::primitiveSampleInterval()
{
    /*
     Set the minimum time span between event driven mouse position
     samples. The argument is a number of milliseconds. Fail if the argument
     is not a SmallInteger.
     */
    //Not actually used!
    primitiveFail();
    
}

void Interpreter::primitiveInputWord()
{
    /*
     Return the next word from the input buffer and remove the word from the
     buffer. This message should be sent just after the input semaphore
     finished a wait (was sent a signal by an I/O device). Fail if the input
     buffer is empty.
     */
        
    std::uint16_t word;
    bool result = hal->next_input_word(&word);
    if (result)
    {
        pop(1); // remove receiver
        push(positive16BitIntegerFor(word));
    }
    else
    {
        primitiveFail();
    }
}


// Return a oop to the display bitmap
// Reference may become invalid on the next cycle of the interpreter...
int Interpreter::getDisplayBits(int width, int height)
{
    // Return oop of display bits object
    // sanity checking display width/height
    if (currentDisplay == 0) return 0; // No display yet
    int displayBits =  memory.fetchPointer_ofObject(BitsInForm, currentDisplay);
    int computedSize = currentDisplayHeight * ((currentDisplayWidth + 15)/16);
    int actualSize = memory.fetchWordLengthOf(displayBits);
    // The sizes do not match...
    // Smalltalk shrinks the display bitmap to a height of 100 behind our back
    // when saving for example.
    if (computedSize != actualSize)
        return 0;
    return displayBits; // ok then...
}

void Interpreter::updateDisplay(int destForm, int updatedX, int updatedY, int updatedWidth, int updatedHeight)
{
    int width  = fetchInteger_ofObject(WidthInForm, currentDisplay);
    int height = fetchInteger_ofObject(HeightInForm, currentDisplay);

    if (currentDisplayWidth != width || currentDisplayHeight != height)
    {
        // Display bits changed... possibly due to screen size change...
        if (height <= 100)
            return; // See note in primitiveBeDisplay...
        hal->set_display_size(width, height);
        currentDisplayWidth  = width;
        currentDisplayHeight = height;
    }
    
    if (updatedWidth > 0 && updatedHeight > 0)
        hal->display_changed(updatedX, updatedY, updatedWidth, updatedHeight);
}

void Interpreter::primitiveCopyBits()
{
    
    int bitBltPointer = stackTop();
    int destForm = memory.fetchPointer_ofObject(DestFormIndex, bitBltPointer);
    int sourceForm = memory.fetchPointer_ofObject(SourceFormIndex, bitBltPointer);
    int destX =  fetchInteger_ofObject(DestXIndex, bitBltPointer);
    int destY =  fetchInteger_ofObject(DestYIndex, bitBltPointer);
    int clipX = fetchInteger_ofObject(ClipXIndex, bitBltPointer);
    int clipY = fetchInteger_ofObject(ClipYIndex, bitBltPointer);
    int clipWidth = fetchInteger_ofObject(ClipWidthIndex, bitBltPointer);
    int clipHeight = fetchInteger_ofObject(ClipHeightIndex, bitBltPointer);
    int sourceX = fetchInteger_ofObject(SourceXIndex, bitBltPointer);
    int sourceY = fetchInteger_ofObject(SourceYIndex, bitBltPointer);
    int width = fetchInteger_ofObject(WidthIndex, bitBltPointer);
    int height = fetchInteger_ofObject(HeightIndex, bitBltPointer);
    int rule = fetchInteger_ofObject(CombinationRuleIndex, bitBltPointer);

    success(between_and(rule, 0, 15));
    if (success())
    {
        BitBlt bitBlt(memory,
                      destForm,
                      sourceForm,
                      memory.fetchPointer_ofObject(HalftoneFormIndex, bitBltPointer),
                      rule,
                      destX,
                      destY,
                      width,
                      height,
                      sourceX,
                      sourceY,
                      clipX,
                      clipY,
                      clipWidth,
                      clipHeight);

        int updatedX, updatedY, updatedWidth, updatedHeight;
        bitBlt.copyBits();
        if (destForm == currentDisplay)
        {
            bitBlt.getUpdatedBounds(&updatedX, &updatedY, &updatedWidth, &updatedHeight);
            if (updatedWidth > 0 && updatedHeight > 0)
                updateDisplay(destForm,  updatedX, updatedY, updatedWidth, updatedHeight);
                
        }
    }
}

void Interpreter::primitiveSnapshot()
{
    /*
     The primitiveSnapshot routine writes the current state of the
     object memory on a file of the same format as the Smalltalk-80 release file.
     This file can be resumed in exactly the same way that the release file was
     originally started. Note that the pointer of the active context at
     the time of the primitive call must be stored in the active Process on the file.
     (G&R) pg 651.
    */

    int activeProcess = memory.fetchPointer_ofObject(ActiveProcessIndex, schedulerPointer());
    memory.storePointer_ofObject_withValue(SuspendedContextIndex, activeProcess, activeContext);
    storeContextRegisters();

    memory.garbageCollect();
    memory.saveSnapshot(fileSystem, hal->get_image_name());
    
    /* This is poorly documented by the Bluebook. There is an actual return value that is important.
    see snapshotAs:thenQuit: in the Smalltalk sources. When the system resumes a snapshot the interpreter will be
    back at the point of the save, and 'self' will be at the top of the stack. But after saving we
    return nil to tell the caller that we just wrote a snapshot. This is how we can distinguish
    the case were we save a session (to be continued potentially followed by an exit) or we are
    resuming a saved session.
     */
    pop(1); // DO NOT POP BEFORE WRITING IMAGE.
    push(NilPointer); //  return of nil signals we just saved
}

void Interpreter::primitiveTimeWordsInto()
{
    /*
     The argument is a byte indexable object of length at least four.  Store
     into the first four bytes of the argument the number of seconds since
     00:00 in the morning of January 1, 1901 (a 32-bit unsigned number).
     The low-order 8-bits are stored in the byte indexed by 1 and the high-
     order 8-bits in the byte indexed 4.
     */
    std::uint32_t time = hal->get_smalltalk_epoch_time();
    int byteIndexObject = popStack();
    memory.storeByte_ofObject_withValue(0, byteIndexObject, time & 0xff);
    memory.storeByte_ofObject_withValue(1, byteIndexObject, (time >>  8) & 0xff);
    memory.storeByte_ofObject_withValue(2, byteIndexObject, (time >> 16) & 0xff);
    memory.storeByte_ofObject_withValue(3, byteIndexObject, (time >> 24) & 0xff);
}

void Interpreter::primitiveTickWordsInto()
{
    /*
     The argument is a byte indexable object of length at least four (a
     LargePositiveInteger).  Store into the first four bytes of the argument the
     number of milliseconds since the millisecond clock was last reset or rolled
     over (a 32-bit unsigned number).  The low-order 8-bits are stored in
     the byte indexed by 1 and the high-order 8-bits in the byte indexed 4.
     */
    std::uint32_t time = hal->get_msclock();
    int byteIndexObject = popStack();
    memory.storeByte_ofObject_withValue(0, byteIndexObject, time & 0xff);
    memory.storeByte_ofObject_withValue(1, byteIndexObject, (time >>  8) & 0xff);
    memory.storeByte_ofObject_withValue(2, byteIndexObject, (time >> 16) & 0xff);
    memory.storeByte_ofObject_withValue(3, byteIndexObject, (time >> 24) & 0xff);
}

void Interpreter::primitiveSignalAtTick()
{
    /*
     
                     
     The primitiveSignalAtTick routine is associated with the signal:atTick:
     messages in ProcessorScheduler. his message takes a Semaphore as the first
     argument and a byte indexable object of at least four bytes as the second
     argument. The first four bytes of the second argument are interpreted as an
     unsigned 32-bit integer of the type stored by the primitiveTickWordsInto
     routine. The interpreter should signal the Semaphore argument when the
     millisecond clock reaches the value specified by the second argument.
     If the specified time has passed,the Semaphore is signaled immediately.
     This primitive signals the last Semaphore to be passed to it. If a new
     call is made on it before the last timer value has been reached the last
     Semaphore will not be signaled. If the first argument is not a Semaphore
     any currently waiting Semaphore will be forgotten. (pg. 652 G&R)

     Signal the semaphore when the millisecond clock reaches the value of
     the second argument.  The second argument is a byte indexable object at
     least four bytes long (a 32-bit unsigned number with the low order
     8-bits stored in the byte with the lowest index).  Fail if the first
     argument is neither a Semaphore nor nil.
     
     
     */
    
    int timePointer = popStack();
    int semaphore = popStack();
    
    success(semaphore == NilPointer ||  memory.fetchClassOf(semaphore) == ClassSemaphorePointer);
    if (success())
    {
        if (semaphore == NilPointer)
            semaphore = 0; // Tells HAL to cancel outstanding request
        
        std::uint32_t time  =
        memory.fetchByte_ofObject(0,  timePointer) |
        (memory.fetchByte_ofObject(1, timePointer) << 8) |
        (memory.fetchByte_ofObject(2, timePointer) << 16) |
        (memory.fetchByte_ofObject(3, timePointer) << 24);
        
        hal->signal_at(semaphore, time);
    }
    else
        unPop(2);
}

void Interpreter::primitiveBeCursor()
{
    /*
     Tell the interpreter to use the receiver as the current cursor image.  Fail if the
     receiver does not match the size expected by the hardware.
     */
    if (currentCursor != stackTop())
    {
        currentCursor   = stackTop();
        int cursorBitmap = memory.fetchPointer_ofObject(BitsInForm, currentCursor);
        std::uint16_t bits[16];
        for(int i = 0; i < 16; i++)
        {
            bits[i] = memory.fetchWord_ofObject(i, cursorBitmap);
        }
        hal->set_cursor_image(bits);
    }

 }

void Interpreter::primitiveBeDisplay()
{
    /*
     Tell the interpreter to use the receiver as the current display image.  Fail if the
     form is too wide to fit on the physical display
     */
    int newDisplay = stackTop();
    if (currentDisplay != newDisplay)
    {
        int width  = fetchInteger_ofObject(WidthInForm, newDisplay);
        int height = fetchInteger_ofObject(HeightInForm, newDisplay);
        // Intersting fact: In order to save space when writing the object image
        // Smalltalk resizes the display to the current width x 100 just before saving.
        // When the system resumes, it will be resized to the last actual size.
        // Here we ignore the change to the height of 100.
        if (height > 100)
        {
            if (!hal->set_display_size(width, height))
            {
                primitiveFail();
            }
            currentDisplay       = newDisplay;
            currentDisplayWidth  = width;
            currentDisplayHeight = height;
        }
        else
            currentDisplay = 0;
    }
}

// scanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops displaying: display

void Interpreter::primitiveScanCharacters()
{
#ifdef IMPLEMENT_PRIMITIVE_SCANCHARS
    bool displaying = popStack() == TruePointer;
    int stops = popStack();
    int rightX = popInteger();
    int sourceString = popStack();
    int stopIndex = popInteger();
    int startIndex = popInteger();
    int scannerPointer = popStack();
    int destForm = memory.fetchPointer_ofObject(DestFormIndex, scannerPointer);
    int sourceForm = memory.fetchPointer_ofObject(SourceFormIndex, scannerPointer);
    int destX =  fetchInteger_ofObject(DestXIndex, scannerPointer);
    int destY;
    int clipX;
    int clipY;
    int clipWidth;
    int clipHeight;
    int sourceX = 0; // uninitialized... assigned during scanning
    int sourceY;
    int rule;
    int width =  0;
    int height;
    int xTable = memory.fetchPointer_ofObject(XTableIndexIndex, scannerPointer);
    int lastIndex = fetchInteger_ofObject(LastIndexIndex, scannerPointer);
    int stopConditions = memory.fetchPointer_ofObject(StopConditionsIndex, scannerPointer);
    
    // If not displaying, some of the fields are nil, so only
    // fetch integers that are actually used...
    if (displaying)
    {
        destY = fetchInteger_ofObject(DestYIndex, scannerPointer);
        clipX = fetchInteger_ofObject(ClipXIndex, scannerPointer);
        clipY = fetchInteger_ofObject(ClipYIndex, scannerPointer);
        clipWidth = fetchInteger_ofObject(ClipWidthIndex, scannerPointer);
        clipHeight = fetchInteger_ofObject(ClipHeightIndex, scannerPointer);
        sourceY = fetchInteger_ofObject(SourceYIndex, scannerPointer);
        rule = fetchInteger_ofObject(CombinationRuleIndex, scannerPointer);
        height = fetchInteger_ofObject(HeightIndex, scannerPointer);
    }
    else
    {
        destY = 0;
        clipX = 0;
        clipY = 0;
        clipWidth = 0;
        clipHeight = 0;
        sourceY = 0;
        rule = 0;
        height = 0;
    }
    
    if (success())
    {
        
        CharacterScanner scanner(memory,
                                 destForm,
                                 sourceForm,
                                 memory.fetchPointer_ofObject(HalftoneFormIndex, scannerPointer),
                                 rule,
                                 destX,
                                 destY,
                                 width,
                                 height,
                                 sourceX,
                                 sourceY,
                                 clipX,
                                 clipY,
                                 clipWidth,
                                 clipHeight,
                                 xTable,
                                 lastIndex,
                                 stopConditions
                                 );
        

        int result = scanner.scanCharactersFrom_to_in_rightX_stopConditions_displaying(startIndex, stopIndex, sourceString, rightX, stops,  displaying);
        /*
         Need to pull out the following modified values and store them back into the scanner:
         destX, width, sourceX in BitBlt fields
         lastIndex  in CharacterScanner
         */
        
        storeInteger_ofObject_withValue(DestXIndex, scannerPointer, scanner.updateDestX());
        storeInteger_ofObject_withValue(WidthIndex, scannerPointer, scanner.updatedWidth());
        storeInteger_ofObject_withValue(SourceXIndex, scannerPointer, scanner.updatedSourceX());
        storeInteger_ofObject_withValue(LastIndexIndex, scannerPointer, scanner.updatedLastIndex());
        
        if (destForm == currentDisplay && displaying)
        {
            updateDisplay(destForm, clipX, clipY, clipWidth, clipHeight);
        }
        push(result);
    }
    else
    {
        unPop(7);
    }
#else
    primitiveFail();
#endif

}

void Interpreter::primitiveDrawLoop()
{
    // optional
    primitiveFail();
}

void Interpreter::primitiveStringReplace()
{
    // optional
    primitiveFail();

}

// lookupMethodInDictionary:
bool Interpreter::lookupMethodInDictionary(int dictionary)
{
   int length;
   int index;
   int mask;
   bool wrapAround;
   int nextSelector;
   int methodArray;

   /* "source"
   	length <- memory fetchWordLengthOf: dictionary.
   	mask <- length - SelectorStart - 1.
   	index <- (mask bitAnd: (self hash: messageSelector)) + SelectorStart.
   	wrapAround <- false.
   	[true] whileTrue:
   		[nextSelector <- memory fetchPointer: index
   					ofObject: dictionary.
   		nextSelector=NilPointer ifTrue: [^false].
   		nextSelector=messageSelector
   			ifTrue: [methodArray <- memory fetchPointer: MethodArrayIndex
   							ofObject: dictionary.
   				newMethod <- memory fetchPointer:  index - SelectorStart
   							ofObject: methodArray.
   				primitiveIndex <- self primitiveIndexOf: newMethod.
   				^true].
   		index <- index + 1.
   		index = length
   			ifTrue: [wrapAround ifTrue: [^false].
   				wrapAround <- true.
   				index <- SelectorStart]]
   */
    length = memory.fetchWordLengthOf(dictionary);
    mask = length - SelectorStart - 1;
    index = (mask & hash(messageSelector)) + SelectorStart;
    wrapAround = false;
    for(;;)
    {
        nextSelector = memory.fetchPointer_ofObject(index, dictionary);
        if (nextSelector == NilPointer) return false;
        if (nextSelector == messageSelector)
        {
            methodArray = memory.fetchPointer_ofObject(MethodArrayIndex, dictionary);
            newMethod = memory.fetchPointer_ofObject(index - SelectorStart, methodArray);
            primitiveIndex = primitiveIndexOf(newMethod);
            return true;
        }
        index = index + 1;
        if (index == length)
        {
            if (wrapAround) return false;
            wrapAround = true;
            index = SelectorStart;
        }
    }
}

// createActualMessage
void Interpreter::createActualMessage()
{
   int argumentArray;
   int message;

   /* "source"
   	argumentArray <- memory instantiateClass: ClassArrayPointer
   				withPointers: argumentCount.
   	"ERROR: messageSize misspelled, self inserted"
   	message <- memory instantiateClass: ClassMessagePointer
   			withPointers: MessageSize.
   	memory storePointer: MessageSelectorIndex
   		ofObject: message
   		withValue: messageSelector.
   	memory storePointer: MessageArgumentsIndex
   		ofObject: message
   		withValue: argumentArray.
   	self transfer: argumentCount
   		"ERROR in selector in next line and next but one after"
   		fromIndex: stackPointer - (argumentCount - 1)
   		ofObject: activeContext
   		toIndex: 0
   		ofObject: argumentArray.
   	self pop: argumentCount.
   	self push: message.
   	argumentCount <- 1
   */
    
    argumentArray = memory.instantiateClass_withPointers(ClassArrayPointer, argumentCount);
    message = memory.instantiateClass_withPointers(ClassMessagePointer, MessageSize);
    memory.storePointer_ofObject_withValue(MessageSelectorIndex, message, messageSelector);
    memory.storePointer_ofObject_withValue(MessageArgumentsIndex, message, argumentArray);
    transfer_fromIndex_ofObject_toIndex_ofObject(argumentCount,
                                                 stackPointer - (argumentCount - 1),
                                                 activeContext,
                                                 0,
                                                 argumentArray);
   
    pop(argumentCount);
    push(message);
    argumentCount = 1;
}


// lookupMethodInClass:
bool Interpreter::lookupMethodInClass(int cls)
{
   int currentClass;
   int dictionary;

   /* "source"
   	currentClass <- class.
   	[currentClass~=NilPointer] whileTrue:
   		[dictionary <- memory fetchPointer: MessageDictionaryIndex
   					ofObject: currentClass.
   		(self lookupMethodInDictionary: dictionary)
   			ifTrue: [^true].
   		currentClass <- self superclassOf: currentClass].
   	messageSelector = DoesNotUnderstandSelector.
   		ifTrue: [self error: 'Recursive not understood error encountered'].
   	self createActualMessage.
   	messageSelector <- DoesNotUnderstandSelector.
   	^self lookupMethodInClass: class
   */
    
    currentClass = cls;
    while (currentClass != NilPointer)
    {
        dictionary = memory.fetchPointer_ofObject(MessageDictionaryIndex, currentClass);
        if (lookupMethodInDictionary(dictionary)) return true;
        currentClass = superclassOf(currentClass);
    }
    if (messageSelector == DoesNotUnderstandSelector)
        error("Recursive not understood error encountered'");
    createActualMessage();
    messageSelector = DoesNotUnderstandSelector;
    return lookupMethodInClass(cls);
}


// returnBytecode
void Interpreter::returnBytecode()
{
   /* "source"
   	currentBytecode = 120
   		ifTrue: [^self returnValue: receiver
   				to: self sender].
   	currentBytecode = 121
   		ifTrue: [^self returnValue: TruePointer
   				to: self sender].
   	currentBytecode = 122
   		ifTrue: [^self returnValue: FalsePointer
   				to: self sender].
   	currentBytecode = 123
   		ifTrue: [^self returnValue: NilPointer
   				to: self sender].
   	currentBytecode = 124
   		ifTrue: [^self returnValue: self popStack
   				to: self sender].
   	currentBytecode = 125
   		ifTrue: [^self returnValue: self popStack
   				to: self caller]
   */
    switch (currentBytecode)
    {
        case 120: returnValue_to(receiver, sender()); break;
        case 121: returnValue_to(TruePointer, sender()); break;
        case 122: returnValue_to(FalsePointer, sender()); break;
        case 123: returnValue_to(NilPointer, sender()); break;
        case 124: returnValue_to(popStack(), sender()); break;
        case 125: returnValue_to(popStack(), caller()); break;
    }
}


// nilContextFields
void Interpreter::nilContextFields()
{
   /* "source"
   	memory storePointer: SenderIndex
   		ofObject: activeContext
   		withValue: NilPointer.
   	memory storePointer: InstructionPointerIndex
   		ofObject: activeContext
   		withValue: NilPointer
   */
    
    memory.storePointer_ofObject_withValue(SenderIndex, activeContext, NilPointer);
    memory.storePointer_ofObject_withValue(InstructionPointerIndex, activeContext, NilPointer);
 
}

// returnToActiveContext:
void Interpreter::returnToActiveContext(int aContext)
{
   /* "source"
       memory increaseReferencesTo: aContext.
       self nilContextFields.
       memory decreaseReferencesTo: activeContext.
       activeContext <- aContext.
       self fetchContextRegisters
   */
    
    memory.increaseReferencesTo(aContext);
    nilContextFields();
    memory.decreaseReferencesTo(activeContext);
    activeContext = aContext;
    fetchContextRegisters();
}


// returnValue:to:
void Interpreter::returnValue_to(int resultPointer, int contextPointer)
{
   int sendersIP;

   /* "source"
   	contextPointer = NilPointer
   		ifTrue: [self push: activeContext.
   			self push: resultPointer.
   			^self sendSelector: CannotReturnSelector
   				argumentCount: 1].
   	sendersIP <- memory fetchPointer: InstructionPointerIndex
   				ofObject: contextPointer.
   	sendersIP = NilPointer
   		ifTrue: [self push: activeContext.
   			self push: resultPointer.
   			^self sendSelector: CannotReturnSelector
   				argumentCount: 1].
   	memory increaseReferencesTo: resultPointer.
   	self returnToActiveContext: contextPointer.
   	self push: resultPointer.
   	memory decreaseReferencesTo: resultPointer
   */
    if (contextPointer == NilPointer)
    {
        push(activeContext);
        push(resultPointer);
        sendSelector_argumentCount(CannotReturnSelector, 1);
        return;
    }
    sendersIP = memory.fetchPointer_ofObject(InstructionPointerIndex, contextPointer);
    if (sendersIP == NilPointer)
    {
        push(activeContext);
        push(resultPointer);
        sendSelector_argumentCount(CannotReturnSelector, 1);
        return;
    }
    memory.increaseReferencesTo(resultPointer);
    returnToActiveContext(contextPointer);
    push(resultPointer);
    memory.decreaseReferencesTo(resultPointer);
}


// synchronousSignal:
void Interpreter::synchronousSignal(int aSemaphore)
{
   int excessSignals;

   /* "source"
   	(self isEmptyList: aSemaphore)
   		ifTrue: [excessSignals <- self fetchInteger: ExcessSignalsIndex
   						ofObject: aSemaphore.
   			self storeInteger: ExcessSignalsIndex
   				ofObject: aSemaphore
   				withValue: excessSignals + 1]
   		ifFalse: [ "dbanay: ref counting issue: removeFirstLinkOfList"
                    aProcess <- self removeFirstLinkOfList: aSemaphore.
    self resume: aProcess.
                    memory decreaseReferencesTo: aProcess]
   */
    
    if (isEmptyList(aSemaphore))
    {
        excessSignals = fetchInteger_ofObject(ExcessSignalsIndex, aSemaphore);
        storeInteger_ofObject_withValue(ExcessSignalsIndex, aSemaphore, excessSignals + 1);
    }
    else
    {
        // removeFirstLinkOfList returns pointer that must be released
        int aProcess = removeFirstLinkOfList(aSemaphore);
        resume(aProcess);
        memory.decreaseReferencesTo(aProcess);
    }
}


// primitiveBlockCopy
void Interpreter::primitiveBlockCopy()
{
   int context;
   int methodContext;
   int blockArgumentCount;
   int newContext;
   int initialIP;
   int contextSize;


   /* "source"
   	blockArgumentCount <- self popStack.
   	context <- self popStack.
   	(self isBlockContext: context)
   		ifTrue: [methodContext <- memory fetchPointer: HomeIndex
   						ofObject: context]
   		ifFalse: [methodContext <- context].
   	contextSize <- memory fetchWordLengthOf: methodContext.
   	newContext <- memory instantiateClass: ClassBlockContextPointer
   				withPointers: contextSize.
   	initialIP <- memory integerObjectOf: instructionPointer + 3.
   	memory storePointer: InitialIPIndex
   		ofObject: newContext
   		withValue: initialIP.
   	memory storePointer: InstructionPointerIndex
   		ofObject: newContext
   		withValue: initialIP.
   	self storeStackPointerValue: 0
   		inContext: newContext.
   	memory storePointer: BlockArgumentCountIndex
   		ofObject: newContext
   		withValue: blockArgumentCount.
   	memory storePointer: HomeIndex
   		ofObject: newContext
   		withValue: methodContext.
   	self push: newContext
   */
   
    blockArgumentCount = popStack();
    context = popStack();
    if (isBlockContext(context))
        methodContext = memory.fetchPointer_ofObject(HomeIndex, context);
    else
        methodContext = context;
    contextSize = memory.fetchWordLengthOf(methodContext);
    newContext  = memory.instantiateClass_withPointers(ClassBlockContextPointer, contextSize);
    initialIP   = memory.integerObjectOf(instructionPointer + 3);
    memory.storePointer_ofObject_withValue(InitialIPIndex, newContext, initialIP);
    memory.storePointer_ofObject_withValue(InstructionPointerIndex, newContext, initialIP);
    storeStackPointerValue_inContext(0, newContext);
    memory.storePointer_ofObject_withValue(BlockArgumentCountIndex, newContext, blockArgumentCount);
    memory.storePointer_ofObject_withValue(HomeIndex, newContext, methodContext);
    push(newContext);
}


// primitiveResume
void Interpreter::primitiveResume()
{
   /* "source"
   	self resume: self stackTop
   */
    resume(stackTop());
}


// primitivePerformWithArgs
void Interpreter::primitivePerformWithArgs()
{
   int thisReceiver;
   int performSelector;
   int argumentArray;
   int arrayClass;
   int arraySize;
   int index;

   /* "source"
   	argumentArray <- self popStack.
   	arraySize <- memory fetchWordLengthOf: argumentArray.
   	arrayClass <- memory fetchClassOf: argumentArray.
   	self success: (stackPointer + arraySize)
   				< (memory fetchWordLengthOf: activeContext).
   	self success: (arrayClass = ClassArrayPointer).
   	self success
   		ifTrue: [performSelector <- messageSelector.
   			messageSelector <- self popStack.
   			thisReceiver <- self stackTop.
   			argumentCount <- arraySize.
   			index <- 1.
   			[index <= argumentCount]
   				whileTrue: [self push: (memory fetchPointer: index - 1
   								ofObject: argumentArray).
   					index <- index + 1].
   			self lookupMethodInClass:
   				(memory fetchClassOf: thisReceiver).
   			self success: (self argumentCountOf: newMethod)
   						= argumentCount.
   			self success
   				ifTrue: [self executeNewMethod]
   				ifFalse: [self unPop: argumentCount.
   					self push: messageSelector.
   					self push: argumentArray.
   					argumentCount <- 2.
   					messageSelector <- performSelector]]
   	ifFalse: [self unPop: 1]
   */

    argumentArray = popStack();
    arraySize = memory.fetchWordLengthOf(argumentArray);
    arrayClass = memory.fetchClassOf(argumentArray);
    success((stackPointer + arraySize)
            < memory.fetchWordLengthOf(activeContext));
    success(arrayClass == ClassArrayPointer);
    if (success())
    {
        performSelector = messageSelector;
        messageSelector = popStack();
        thisReceiver = stackTop();
        argumentCount = arraySize;
        index =  1;
        while (index <= argumentCount)
        {
            push(memory.fetchPointer_ofObject(index - 1, argumentArray));
            index = index + 1;
        }
        lookupMethodInClass(memory.fetchClassOf(thisReceiver));
        success(argumentCountOf(newMethod) == argumentCount);
        if (success())
            executeNewMethod();
        else
        {
            unPop(argumentCount);
            push(messageSelector);
            push(argumentArray);
            argumentCount = 2;
            messageSelector = performSelector;
        }
    }
    else
        unPop(1);

}


// wakeHighestPriority
//NOTE: Returns a referenced increased pointer that must be decreased by caller
int Interpreter::wakeHighestPriority()
{
   int priority;
   int processLists;
   int processList;

   /* "source"
   	processLists <- memory fetchPointer: ProcessListsIndex
   				ofObject: self schedulerPointer.
   	priority <- memory fetchWordLengthOf: processLists.
   	[processList <- memory fetchPointer: priority - 1
   				ofObject: processLists.
   	self isEmptyList: processList] whileTrue: [priority <- priority - 1].
   	^self removeFirstLinkOfList: processList
   */
    
    processLists = memory.fetchPointer_ofObject(ProcessListsIndex, schedulerPointer());
    priority = memory.fetchWordLengthOf(processLists);
    for(;;)
    {
        assert(priority > 0);
        processList = memory.fetchPointer_ofObject(priority - 1, processLists);
        if (!isEmptyList(processList)) break;
        priority = priority - 1;
    }
   
    return removeFirstLinkOfList(processList);
}


// primitivePerform
void Interpreter::primitivePerform()
{
   int performSelector;
   int newReceiver;
   int selectorIndex;
    

   /* "source"
   	performSelector <- messageSelector.
   	messageSelector <- self stackValue: argumentCount - 1.
   	newReceiver <- self stackValue: argumentCount.
   	self lookupMethodInClass: (memory fetchClassOf: newReceiver).
   	self success: (self argumentCountOf: newMethod) = (argumentCount - 1).
   	self success
   		ifTrue: [selectorIndex <- stackPointer - argumentCount + 1.
   			self transfer: argumentCount - 1
   				fromIndex: selectorIndex + 1
   				ofObject: activeContext
   				toIndex: selectorIndex
   				ofObject: activeContext.
   			self pop: 1.
   			argumentCount <- argumentCount - 1.
   			self executeNewMethod]
   		ifFalse: [messageSelector <- performSelector]
   */
   
    performSelector = messageSelector;
    messageSelector = stackValue(argumentCount - 1);
    newReceiver  = stackValue(argumentCount);
    lookupMethodInClass(memory.fetchClassOf(newReceiver));
    success(argumentCountOf(newMethod) == argumentCount - 1);
    if (success())
    {
        selectorIndex = stackPointer - argumentCount + 1;
        transfer_fromIndex_ofObject_toIndex_ofObject( argumentCount - 1,
                        selectorIndex + 1,
                        activeContext,
                        selectorIndex,
                        activeContext);
        pop(1);
        argumentCount = argumentCount - 1;
        executeNewMethod();
    }
    else
    {
        messageSelector = performSelector;
    }
}


// primitiveValueWithArgs
void Interpreter::primitiveValueWithArgs()
{
    int argumentArray;
    int blockContext;
    int blockArgumentCount;
    int arrayClass;
    int arrayArgumentCount = 0;
    int initialIP;
    
   /* "source"
   	argumentArray <- self popStack.
   	blockContext <- self popStack.
   	blockArgumentCount <- self argumentCountOfBlock: blockContext.
   	arrayClass <- memory fetchClassOf: argumentArray.
   	self success: (arrayClass = ClassArrayPointer).
   	self success
   		ifTrue: [arrayArgumentCount <- memory fetchWordLengthOf: argumentArray.
   			self success: arrayArgumentCount = blockArgumentCount].
   	self success
   		ifTrue: [self transfer: arrayArgumentCount
   				fromIndex: 0
   				ofObject: argumentArray
   				toIndex: TempFrameStart
   				ofObject: blockContext.
   			initialIP <- memory fetchPointer: InitialIPIndex
   						ofObject: blockContext.
   			memory storePointer: InstructionPointerIndex
   				ofObject: blockContext
   				withValue: initialIP.
   			self storeStackPointerValue: arrayArgumentCount
   				inContext: blockContext.
   			memory storePointer: CallerIndex
   				ofObject: blockContext
   				withValue: activeContext.
   			self newActiveContext: blockContext]
   		ifFalse: [self unPop: 2]
   */
    
    argumentArray = popStack();
    blockContext =  popStack();
    blockArgumentCount = argumentCountOfBlock(blockContext);
    arrayClass = memory.fetchClassOf(argumentArray);
    success(arrayClass == ClassArrayPointer);
    if (success())
    {
        arrayArgumentCount = memory.fetchWordLengthOf(argumentArray);
        success(arrayArgumentCount == blockArgumentCount);
    }
    if (success())
    {
        transfer_fromIndex_ofObject_toIndex_ofObject(arrayArgumentCount,
                            0,
                            argumentArray,
                            TempFrameStart,
                            blockContext);
        initialIP = memory.fetchPointer_ofObject(InitialIPIndex, blockContext);
        memory.storePointer_ofObject_withValue(InstructionPointerIndex, blockContext, initialIP);
        storeStackPointerValue_inContext(arrayArgumentCount, blockContext);
        memory.storePointer_ofObject_withValue(CallerIndex, blockContext, activeContext);
        newActiveContext(blockContext);
    }
    else
        unPop(2);
}


// removeFirstLinkOfList:
int Interpreter::removeFirstLinkOfList(int aLinkedList)
{
   int firstLink;
   int lastLink;
   int nextLink;

   /* "source"
   	firstLink <- memory fetchPointer: FirstLinkIndex
   				ofObject: aLinkedList.
    memory increaseReferencesTo: firstLink. "dbanay: protect it in case lastLink = firstLink"
   	lastLink <- memory fetchPointer: LastLinkIndex
   				ofObject: aLinkedList.
   	lastLink = firstLink
   		ifTrue: [memory storePointer: FirstLinkIndex
   				ofObject: aLinkedList
   				withValue: NilPointer.
   			memory storePointer: LastLinkIndex
   				ofObject: aLinkedList
   				withValue: NilPointer]
   		ifFalse: [nextLink <- memory fetchPointer: NextLinkIndex
   						ofObject: firstLink.
   			memory storePointer: FirstLinkIndex
   				ofObject: aLinkedList
   				withValue: nextLink].
   	memory storePointer: NextLinkIndex
   		ofObject: firstLink
   		withValue: NilPointer.
   	^firstLink
   */
    /*
     The routines listed here ignore the reference-counting problem in the interest of clarity. (pg. 644 G&R).
     Found and fixed. -dbanay
     */
    
    firstLink = memory.fetchPointer_ofObject(FirstLinkIndex, aLinkedList);
    
    // When the link list is updated the references to fistLink
    // will drop to zero. So, we need to return an increased pointer that
    // must be decreased by the caller.
    // Calls:
    //     suspendActive->wakeHighestPriority->removeFirstLinkOfList
    //     synchronousSignal->removeFirstLinkOfList
    memory.increaseReferencesTo(firstLink);
    
    lastLink  = memory.fetchPointer_ofObject(LastLinkIndex, aLinkedList);
    if (lastLink == firstLink)
    {
        memory.storePointer_ofObject_withValue(FirstLinkIndex, aLinkedList, NilPointer);
        memory.storePointer_ofObject_withValue(LastLinkIndex, aLinkedList, NilPointer);
    }
    else
    {
        nextLink = memory.fetchPointer_ofObject(NextLinkIndex, firstLink);
        memory.storePointer_ofObject_withValue(FirstLinkIndex, aLinkedList, nextLink);
    }
    memory.storePointer_ofObject_withValue(NextLinkIndex, firstLink, NilPointer);
    return firstLink;
}


// addLastLink:toList:
void Interpreter::addLastLink_toList(int aLink, int aLinkedList)
{
   int lastLink;

   /* "source"
       (self isEmptyList: aLinkedList)
           ifTrue: [memory storePointer: FirstLinkIndex
                   ofObject: aLinkedList
                   withValue: aLink]
           ifFalse: [lastLink <- memory fetchPointer: LastLinkIndex
                           ofObject: aLinkedList.
               memory storePointer: NextLinkIndex
                   ofObject: lastLink
                   withValue: aLink].
       memory storePointer: LastLinkIndex
           ofObject: aLinkedList
           withValue: aLink.
       memory storePointer: MyListIndex
           ofObject: aLink
           withValue: aLinkedList
   */
    
   
    if (isEmptyList(aLinkedList))
    {
        memory.storePointer_ofObject_withValue(FirstLinkIndex, aLinkedList, aLink);
    }
    else
    {
        lastLink = memory.fetchPointer_ofObject(LastLinkIndex, aLinkedList);
        memory.storePointer_ofObject_withValue(NextLinkIndex, lastLink, aLink);
    }
    memory.storePointer_ofObject_withValue(LastLinkIndex, aLinkedList, aLink);
    memory.storePointer_ofObject_withValue(MyListIndex, aLink, aLinkedList);
    
}

// primitiveWait
void Interpreter::primitiveWait()
{
   int thisReceiver;
   int excessSignals;

   /* "source"
   	thisReceiver <- self stackTop.
   	excessSignals <- self fetchInteger: ExcessSignalsIndex
   				ofObject: thisReceiver.
   	excessSignals > 0
   		ifTrue: [self storeInteger: ExcessSignalsIndex
   				ofObject: thisReceiver
   				withValue: excessSignals - 1]
   		ifFalse: [self addLastLink: self activeProcess
   				toList: thisReceiver.
   			self suspendActive]
   */
    
    thisReceiver = stackTop();
    excessSignals = fetchInteger_ofObject(ExcessSignalsIndex, thisReceiver);
    if (excessSignals > 0)
    {
        storeInteger_ofObject_withValue(ExcessSignalsIndex, thisReceiver, excessSignals - 1);
    }
    else
    {
        addLastLink_toList(activeProcess(), thisReceiver);
        suspendActive();
    }
}


// primitiveFlushCache
void Interpreter::primitiveFlushCache()
{
   /* "source"
   	self initializeMethodCache
   */
    initializeMethodCache();
}


// suspendActive
void Interpreter::suspendActive()
{
   /* "source"
    "dbanay: ref counting issue - wakeHighestPriority"
    aProcess = self wakeHighestPriority.
   	self transferTo: self wakeHighestPriority.
    memory decreaseReferencesTo: aProcess
   */

    //Note: wakeHighestPriority returns a pointer that must be decreased by caller
    int aProcess = wakeHighestPriority();
    transferTo(aProcess);
    memory.decreaseReferencesTo(aProcess);
}


// activeProcess
int Interpreter::activeProcess()
{
   /* "source"
   	newProcessWaiting
   		ifTrue: [^newProcess]
   		ifFalse: [^memory fetchPointer: ActiveProcessIndex
   				ofObject: self schedulerPointer]
   */
    
    if (newProcessWaiting)
    {
        return newProcess;
    }
    
    return memory.fetchPointer_ofObject(ActiveProcessIndex, schedulerPointer());
}


// dispatchControlPrimitives
void Interpreter::dispatchControlPrimitives()
{
   /* "source"
   	primitiveIndex = 80 ifTrue: [^self primitiveBlockCopy].
   	primitiveIndex = 81 ifTrue: [^self primitiveValue].
   	primitiveIndex = 82 ifTrue: [^self primitiveValueWithArgs].
   	primitiveIndex = 83 ifTrue: [^self primitivePerform].
   	primitiveIndex = 84 ifTrue: [^self primitivePerformWithArgs].
   	primitiveIndex = 85 ifTrue: [^self primitiveSignal].
   	primitiveIndex = 86 ifTrue: [^self primitiveWait].
   	primitiveIndex = 87 ifTrue: [^self primitiveResume].
   	primitiveIndex = 88 ifTrue: [^self primitiveSuspend].
   	primitiveIndex = 89 ifTrue: [^self primitiveFlushCache]
   */
    switch (primitiveIndex)
    {
        case  80: primitiveBlockCopy(); break;
        case  81: primitiveValue(); break;
        case  82: primitiveValueWithArgs(); break;
        case  83: primitivePerform(); break;
        case  84: primitivePerformWithArgs(); break;
        case  85: primitiveSignal(); break;
        case  86: primitiveWait(); break;
        case  87: primitiveResume(); break;
        case  88: primitiveSuspend(); break;
        case  89: primitiveFlushCache(); break;
    }

}


bool Interpreter::isInLowMemoryCondition()
{
    return oopsLeftLimit > 0 && wordsLeftLimit > 0 &&
    (memory.oopsLeft() < oopsLeftLimit ||
     memory.coreLeft() < wordsLeftLimit);
}


// checkProcessSwitch
void Interpreter::checkProcessSwitch()
{
   int theActiveProcess;

   /* "source"
   	[semaphoreIndex > 0]
   		whileTrue:
   			[self synchronousSignal: (semaphoreList at: semaphoreIndex).
   			semaphoreIndex <- semaphoreIndex - 1].
   	newProcessWaiting
   		ifTrue: [newProcessWaiting <- false.
   			activeProcess <- self activeProcess.
   			memory storePointer: SuspendedContextIndex
   				ofObject: activeProcess
   				withValue: activeContext.
   			memory storePointer: ActiveProcessIndex
   				ofObject: self schedulerPointer
   				withValue: newProcess.
   			self newActiveContext:
   				(memory fetchPointer: SuspendedContextIndex
   					ofObject: newProcess).
                "dbanay: Need to remove reference"
                    memory decreaseReferencesTo: newProcess.
                    newProcess <- nil]
   */
    
    
    //dbanay -- warn about low memory -- once
    if (checkLowMemory)
    {
        bool memoryWasLow = memoryIsLow;
        memoryIsLow = false;
        // the Smalltalk system treat oops or words limits being zero as don't check
        if (lowSpaceSemaphore != NilPointer && oopsLeftLimit > 0 && wordsLeftLimit > 0)
        {
            if (isInLowMemoryCondition())
            {
                memory.garbageCollect(); // Try to get some memory back...
                if (isInLowMemoryCondition())
                {
                    memoryIsLow = true;
                    if (!memoryWasLow)
                        asynchronousSignal(lowSpaceSemaphore);
                }
            }
        }
        checkLowMemory = false;
    }
    
    
    while (semaphoreIndex >= 0)
    {
        synchronousSignal(semaphoreList[semaphoreIndex]);
        semaphoreIndex = semaphoreIndex - 1;
    }
    if (newProcessWaiting)
    {
        newProcessWaiting = false;
        theActiveProcess = activeProcess();
        memory.storePointer_ofObject_withValue(SuspendedContextIndex, theActiveProcess, activeContext);
        memory.storePointer_ofObject_withValue(ActiveProcessIndex, schedulerPointer(), newProcess);
        newActiveContext(memory.fetchPointer_ofObject(SuspendedContextIndex, newProcess));
        memory.decreaseReferencesTo(newProcess);
        newProcess = NilPointer;
    }
}


// primitiveSignal
void Interpreter::primitiveSignal()
{
   /* "source"
   	self synchronousSignal: self stackTop
   */
    synchronousSignal(stackTop());
}


// isEmptyList:
int Interpreter::isEmptyList(int aLinkedList)
{
   /* "source"
   	^(memory fetchPointer: FirstLinkIndex
   		ofObject: aLinkedList)
   			= NilPointer
   */
    
    return memory.fetchPointer_ofObject(FirstLinkIndex, aLinkedList) == NilPointer;
}


// primitiveSuspend
void Interpreter::primitiveSuspend()
{
   /* "source"
   	self success: self stackTop = self activeProcess.
   	self success
   		ifTrue: [self popStack.
   			self push: NilPointer.
   			self suspendActive]
   */
    success(stackTop() == activeProcess());
    if (success())
    {
        popStack();
        push(NilPointer);
        suspendActive();
    }
    
}


// primitiveValue
void Interpreter::primitiveValue()
{
   int blockContext;
   int blockArgumentCount;
   int initialIP;

   /* "source"
   	blockContext <- self stackValue: argumentCount.
   	blockArgumentCount <- self argumentCountOfBlock: blockContext.
   	self success: argumentCount = blockArgumentCount.
   	self success
   		ifTrue: [self transfer: argumentCount
   				fromIndex: stackPointer - argumentCount + 1
   				ofObject: activeContext
   				toIndex: TempFrameStart
   				ofObject: blockContext.
   			self pop: argumentCount + 1.
   			initialIP <- memory fetchPointer: InitialIPIndex
   						ofObject: blockContext.
   			memory storePointer: InstructionPointerIndex
   				ofObject: blockContext
   				withValue: initialIP.
   			self storeStackPointerValue: argumentCount
   				inContext: blockContext.
   			memory storePointer: CallerIndex
   				ofObject: blockContext
   				withValue: activeContext.
   			self newActiveContext: blockContext]
   */
   
    blockContext = stackValue(argumentCount);
    blockArgumentCount = argumentCountOfBlock(blockContext);
    success(argumentCount == blockArgumentCount);
    if (success())
    {
        transfer_fromIndex_ofObject_toIndex_ofObject(argumentCount,
                                                     stackPointer - argumentCount + 1,
                                                     activeContext,
                                                     TempFrameStart,
                                                     blockContext);
        pop(argumentCount + 1);
        initialIP = memory.fetchPointer_ofObject(InitialIPIndex, blockContext);
        memory.storePointer_ofObject_withValue(InstructionPointerIndex, blockContext, initialIP);
        storeStackPointerValue_inContext(argumentCount, blockContext);
        memory.storePointer_ofObject_withValue(CallerIndex, blockContext, activeContext);
        newActiveContext(blockContext);
    }

}


// firstContext
int Interpreter::firstContext()
{
   /* "source"
   	newProcessWaiting <- false.
    newProcess <- nil. "dbanay: need to initialize"
   	^memory fetchPointer: SuspendedContextIndex
   		ofObject: self activeProcess
   */
    
    newProcessWaiting = false;
    newProcess = NilPointer;
    return memory.fetchPointer_ofObject(SuspendedContextIndex, activeProcess());
}


// asynchronousSignal:
void Interpreter::asynchronousSignal(int aSemaphore)
{
   /* "source"
   	semaphoreIndex <- semaphoreIndex + 1.
   	semaphoreList at: semaphoreIndex put: aSemaphore
   */
    semaphoreIndex = semaphoreIndex + 1;
    if (semaphoreIndex == sizeof(semaphoreList)/sizeof(semaphoreList[0]))
        error("overflow semaphore list");
    semaphoreList[semaphoreIndex] = aSemaphore;

}


// transferTo:
void Interpreter::transferTo(int aProcess)
{
   /* "source"
   	newProcessWaiting <- true.
    "dbanay needs ref count newProcess"
    newProcess ~~ NilPointer ifTrue: [ memory decreaseReferencesTo: newProcess ].
   	newProcess <- aProcess
   */
    newProcessWaiting = true;
    if (newProcess != NilPointer)
        memory.decreaseReferencesTo(newProcess);
    newProcess = aProcess;
    memory.increaseReferencesTo(newProcess);

}


// resume:
void Interpreter::resume(int aProcess)
{
   int theActiveProcess;
   int activePriority;
   int newPriority;

   /* "source"
   	activeProcess <- self activeProcess.
   	activePriority <- self fetchInteger: PriorityIndex
   				ofObject: activeProcess.
   	newPriority <- success fetchInteger: PriorityIndex
   				ofObject: aProcess.
   	newPriority > activePriority
   		ifTrue: [self sleep: activeProcess
   			self transferTo: aProcess]
   		ifFalse: [self sleep: aProcess]
   */
    theActiveProcess = activeProcess();
    activePriority = fetchInteger_ofObject(PriorityIndex, theActiveProcess);
    newPriority = fetchInteger_ofObject(PriorityIndex, aProcess);
    if (newPriority > activePriority)
    {
        sleep(theActiveProcess);
        transferTo(aProcess);
    }
    else
    {
        sleep(aProcess);
    }
}


// sleep:
void Interpreter::sleep(int aProcess)
{
   int priority;
   int processLists;
   int processList;

   /* "source"
   	priority <- self fetchInteger: PriorityIndex
   			ofObject: aProcess.
   	processLists <- memory fetchPointer: ProcessListsIndex
   				ofObject: self schedulerPointer.
   	processList <- memory fetchPointer: priority - 1
   				ofObject: processLists.
   	self addLastLink: aProcess
   		toList: processList
   */
    priority = fetchInteger_ofObject(PriorityIndex, aProcess);
    processLists = memory.fetchPointer_ofObject(ProcessListsIndex, schedulerPointer());
    processList = memory.fetchPointer_ofObject(priority - 1, processLists);
    addLastLink_toList(aProcess, processList);
}


// primitiveClass
void Interpreter::primitiveClass()
{
   int instance;

   /* "source"
   	instance <- self popStack.
   	self push: (memory fetchClassOf: instance)
   */
    instance = popStack();
    push(memory.fetchClassOf(instance));
}


void Interpreter::primitiveCoreLeft()
{
    pop(1); // remove receiver
    push(positive32BitIntegerFor(memory.coreLeft()));
 }

void Interpreter::primitiveQuit()
{
    hal->signal_quit();

}

void Interpreter::primitiveExitToDebugger()
{
    hal->exit_to_debugger();
}

void Interpreter::primitiveOopsLeft()
{
    pop(1); // remove receiver
    push(positive16BitIntegerFor(memory.oopsLeft()));
}

void Interpreter::primitiveSignalAtOopsLeftWordsLeft()
{
    /*
     (from the sources)
     Tell the object memory to signal the Semaphore when either the number
      of object pointers remaining drops below numOops, or the number of
     words in the object space remaining drops below numWords.  Fail if the
     first argument is neither a Semaphore nor nil.  Fail if numOops is not a
     16-bit Integer, or if numWords is not a 32-bit LargePositiveInteger.
     */
    
    std::uint32_t numWords = positive32BitValueOf(popStack());
    int numOops =  positive16BitValueOf(popStack());
    int semaphore =  popStack();
    // sempahore must either be nil or an instance of Semaphore
    success(semaphore == NilPointer || memory.fetchClassOf(semaphore) == ClassSemaphorePointer);
    if (success())
    {
        // Don't need to take reference since Smalltalk is waiting on it someplace else
        lowSpaceSemaphore = semaphore;
        oopsLeftLimit = numOops;
        wordsLeftLimit = numWords;
    }
    else
        unPop(3);
 
}


// dispatchSystemPrimitives
void Interpreter::dispatchSystemPrimitives()
{
   /* "source"
   	primitiveIndex = 110 ifTrue: [^self primitiveEquivalent].
   	primitiveIndex = 111 ifTrue: [^self primitiveClass].
   	primitiveIndex = 112 ifTrue: [^self primitiveCoreLeft].
   	primitiveIndex = 113 ifTrue: [^self primitiveQuit].
   	primitiveIndex = 114 ifTrue: [^self primitiveExitToDebugger].
   	primitiveIndex = 115 ifTrue: [^self primitiveOopsLeft].
   	primitiveIndex = 116 ifTrue: [^self primitiveSignalAtOopsLeftWordsLeft]
   */

    switch (primitiveIndex)
    {
        case 110: primitiveEquivalent(); break;
        case 111: primitiveClass(); break;
        case 112: primitiveCoreLeft(); break;
        case 113: primitiveQuit(); break;
        case 114: primitiveExitToDebugger(); break;
        case 115: primitiveOopsLeft(); break;
        case 116: primitiveSignalAtOopsLeftWordsLeft(); break;
    }
}


// primitiveEquivalent
void Interpreter::primitiveEquivalent()
{
   int thisObject;
   int otherObject;

   /* "source"
   	otherObject <- self popStack.
   	thisObject <- self popStack.
   	thisObject = otherObject
   		ifTrue: [self push: TruePointer]
   		ifFalse: [self push: FalsePointer]
   */
   
    otherObject = popStack();
    thisObject = popStack();
    if (thisObject == otherObject)
        push(TruePointer);
    else
        push(FalsePointer);
}


// dispatchPrimitives
void Interpreter::dispatchPrimitives()
{
   /* "source"
   	primitiveIndex < 60
   		ifTrue: [^self dispatchArithmeticPrimitives].
   	primitiveIndex < 68
   		ifTrue: [^self dispatchSubscriptAndStreamPrimitives].
   	primitiveIndex < 80
   		ifTrue: [^self dispatchStorageManagementPrimitives].
   	primitiveIndex < 90
   		ifTrue: [^self dispatchControlPrimitives].
   	primitiveIndex < 110
   		ifTrue: [^self dispatchInputOutputPrimitives].
   	primitiveIndex < 128
   		ifTrue: [^self dispatchSystemPrimitives].
   	primitiveIndex < 256
   		ifTrue: [^self dispatchPrivatePrimitives]
   */
    if (primitiveIndex < 60)
        dispatchArithmeticPrimitives();
    else if (primitiveIndex < 68)
        dispatchSubscriptAndStreamPrimitives();
    else if (primitiveIndex < 80)
        dispatchStorageManagementPrimitives();
    else if (primitiveIndex < 90)
        dispatchControlPrimitives();
    else if (primitiveIndex < 110)
        dispatchInputOutputPrimitives();
    else if (primitiveIndex < 128)
        dispatchSystemPrimitives();
    else if (primitiveIndex < 256)
    {
        dispatchPrivatePrimitives();
    }
}

// dispatchPrivatePrimitives
void Interpreter::dispatchPrivatePrimitives()
{
    switch (primitiveIndex)
    {
        case 128: // beSnapshot
            primitiveBeSnapshotFile();
            break;
        case 130: // Posix File primitives
            primitivePosixFileOperation();
            break;
        case 131: // Posix Directory primitives
            primitivePosixDirectoryOperation();
            break;
        case 132: // Posix error
            primitivePosixLastErrorOperation();
            break;
        case 133: // Posix error string
            primitivePosixErrorStringOperation();
            break;
        default:
            primitiveFail();
            break;
    }
}

void Interpreter::primitiveBeSnapshotFile()
{

    int fileObjectPointer = stackTop();
    int fileNamePointer = memory.fetchPointer_ofObject(FileNameIndex, fileObjectPointer); // fileName field
    std::string fileName = stringFromObject(fileNamePointer);
    
    hal->set_image_name(fileName.c_str());
}

void Interpreter::primitivePosixLastErrorOperation()
{
    pop(1);
    pushInteger(fileSystem->last_error());
}

void Interpreter::primitivePosixErrorStringOperation()
{
    int code = popInteger();
    pop(1); // pop receiver
    if (success())
        push(stringObjectFor(fileSystem->error_text(code)));
    else
        unPop(2);
}

void Interpreter::primitivePosixFileOperation()
{
    // command id, name, page, file
    /*
     can get file descriptor from file ('fd' field)
     ID    Return                 Remarks
     ----  -------                ----------
     0     true/false             read page (stores size in bytesInPage)
     1     true/false             write page (uses bytesInPage)
     2     true/false             truncate after page or entire file if page is nil
     3     size of file/nil       file size
     4     true/false             open file -- sets fd in file
     5     true/false             close file -- sets fd to -1
     */
    
    int page = popStack();
    int name = popStack();
    int code = popInteger();
    int file = popStack();

    const int DescriptorIndex  = 8; // fd field of PosixFile
    const int PageNumberIndex  = 3; // pageNumber  field in PosixFilePage
    
    const int PageInPageIndex  = 1; // ByteArray contents in PosixFilePage
    const int BytesInPageIndex = 4; // bytesInPage field in PosixFilePage
    
    const int PageSize = 512;   // MUST match page size of PosixFilePage
    
    static std::uint8_t pageBuffer[PageSize];
    
    // Code must be legit
    success(code >= 0 && code <= 6);
    success(file != NilPointer);
    
    if (success())
    {
        switch (code)
        {
            case 0:  // Read page
            {
                success(memory.fetchPointer_ofObject(DescriptorIndex, file) != NilPointer);
                success(page != NilPointer);
                if (success())
                {
                    int fd = (int) positive32BitValueOf(memory.fetchPointer_ofObject(DescriptorIndex, file));
                    int pageNumber = fetchInteger_ofObject(PageNumberIndex, page);
                    assert(pageNumber >= 1);
                    
                    int byteArray = memory.fetchPointer_ofObject(PageInPageIndex, page);
                    
                    int position = (pageNumber - 1)*PageSize;
                    
                    if (fileSystem->seek_to(fd, position) != position)
                    {
                        push(FalsePointer);
                        return;
                    }
                    
                    // No direct pointer access! Read full page and store byte by byte
                    int bytesInPage = fileSystem->read(fd, (char *) &pageBuffer, PageSize);
                    
                    for(int i = 0; i < bytesInPage; i++)
                        memory.storeByte_ofObject_withValue(i, byteArray, pageBuffer[i]);
                    
                    storeInteger_ofObject_withValue(BytesInPageIndex, page, bytesInPage);
                    push(TruePointer);
                }
            }
                break;
                
            case 1: // Write page
            {	
                success(memory.fetchPointer_ofObject(DescriptorIndex, file) != NilPointer);
                success(page != NilPointer);
                if (success())
                {
                    int fd = (int) positive32BitValueOf(memory.fetchPointer_ofObject(DescriptorIndex, file));
                    int pageNumber = fetchInteger_ofObject(PageNumberIndex, page);
                    assert(pageNumber >= 1);
                    
                    int byteArray = memory.fetchPointer_ofObject(PageInPageIndex, page);
                    
                    int position = (pageNumber - 1)*PageSize;
                    
                    if (fileSystem->seek_to(fd, position) != position)
                    {
                        push(FalsePointer);
                        return;
                    }
                    
                    // No direct pointer access! Read bytes by byte into staging buffer
                    int bytesInPage = fetchInteger_ofObject(BytesInPageIndex, page);
                    // Fill buffer with page data
                    for(int i  = 0; i < bytesInPage; i++)
                        pageBuffer[i] = memory.fetchByte_ofObject(i, byteArray);
                    
                    // blast it out
                    if (fileSystem->write(fd, (char *) pageBuffer, bytesInPage) == bytesInPage)
                        push(TruePointer);
                    else
                        push(FalsePointer);
                }
            }
                break;
                
            case 2: // truncate page (make it the last page). A nil page means truncate to zero bytes
            {
                success(memory.fetchPointer_ofObject(DescriptorIndex, file) != NilPointer);
                success(page != NilPointer);
                if (success())
                {
                    
                    int result;
                    int fd = (int) positive32BitValueOf(memory.fetchPointer_ofObject(DescriptorIndex, file));
                    if (page != NilPointer)
                    {
                        int pageNumber = fetchInteger_ofObject(PageNumberIndex, page);
                        assert(pageNumber >= 1);
                        
                        int bytesInPage = fetchInteger_ofObject(BytesInPageIndex, page);
                        int newSize = (pageNumber-1)*PageSize + bytesInPage;
                        result = fileSystem->truncate_to(fd, newSize);
                    }
                    else
                        result = fileSystem->truncate_to(fd, 0);
                    push(result != -1 ? TruePointer : FalsePointer);
                }
                
            }
                break;
                
            case 3: // File size
            {
                success(memory.fetchPointer_ofObject(DescriptorIndex, file) != NilPointer);
                if (success())
                {
                    int fd = (int) positive32BitValueOf(memory.fetchPointer_ofObject(DescriptorIndex, file));
                    int size = fileSystem->file_size(fd);
                    if (size >= 0)
                        push(positive32BitIntegerFor(size));
                    else
                        push(NilPointer);
                }
            }
                break;
                
            case 4: // Open File
            {
                success(memory.fetchClassOf(name) == ClassStringPointer);
                if (success())
                {
                    std::string fileName = stringFromObject(name);
                    int fd = fileSystem->open_file(fileName.c_str());
                    if (fd != -1)
                    {
                        memory.storePointer_ofObject_withValue(DescriptorIndex, file, positive32BitIntegerFor(fd));
                        push(TruePointer);
                    }
                    else
                        push(FalsePointer);
                }
            }
                
                break;
                
            case 5: // Close file
            {
                int fd = (int) positive32BitValueOf(memory.fetchPointer_ofObject(DescriptorIndex, file));
                int result = fileSystem->close_file(fd);
                memory.storePointer_ofObject_withValue(DescriptorIndex, file, NilPointer);
                push(result != -1 ? TruePointer : FalsePointer);
            }
                break;
        }
    }
    
    if (!success())
        unPop(4);
    
}

void Interpreter::primitivePosixDirectoryOperation()
{
    /*
     If you allocate an array and then allocate more objects to put into that array, the array pointer has no reference and can go away if GC happens. Be sure to push it on the stack after alloc to protect it.

     */
    /*
     ID    ARG1      ARG2       Return               Remarks
     ----  --------  -------    ------               -----------
     0     name                 fd/nil               create file (replacing existing one)
     1     name                 true/false           delete file
     2     new name  PosixFile  true/false           rename file
     3     nil                  Array of strings     Array of file names in directory
     */
    
    const int DescriptorIndex  = 8; // fd field of PosixFile

 
    int arg2 = popStack();
    int arg1 = popStack();
    int code = popInteger();
    pop(1); // remove receiver
    
    success(code >= 0 && code <= 3);
    success(arg1 == NilPointer || memory.fetchClassOf(arg1) == ClassStringPointer);

    if (success())
    {
        switch (code)
        {
            case 0: // Create file
            {
                std::string s = stringFromObject(arg1);
                int fd = fileSystem->create_file(s.c_str());
                if (fd != -1)
                {
                    push(positive32BitIntegerFor(fd));
                }
                else
                    push(NilPointer);
            }
            break;
                
            case 1: // Delete file
            {
                std::string s = stringFromObject(arg1);
                push(fileSystem->delete_file(s.c_str()) ? TruePointer : FalsePointer);
            }
            break;
                
            case 2: // Rename file
            {
                int oldPos = 0; // silence uninitialized variable warning
                int file = arg2;
                int newNameObjectPointer = arg1;
                int descriptorObjectPointer = memory.fetchPointer_ofObject(DescriptorIndex, file);
                bool wasOpen = (descriptorObjectPointer != NilPointer);
                if (wasOpen)
                {
                    int fd = (int) positive32BitValueOf(descriptorObjectPointer);
                    oldPos = fileSystem->tell(fd); // Save position for restore after we open new file...
                    // Some operating systems don't let you rename an open file...
                    fileSystem->close_file(fd);
                }
                std::string newName = stringFromObject(newNameObjectPointer);
                std::string oldName = stringFromObject(memory.fetchPointer_ofObject(FileNameIndex, file));
                if (fileSystem->rename_file(oldName.c_str(), newName.c_str()))
                {
                    // Re-open and update descriptor of File object
                    if (wasOpen)
                    {
                        int fd = fileSystem->open_file(newName.c_str());
                        if (fd != -1)
                        {
                            memory.storePointer_ofObject_withValue(DescriptorIndex,
                                                                   file, positive32BitIntegerFor(fd));
                            fileSystem->seek_to(fd, oldPos); // Restore position....
                        }
                        else
                            memory.storePointer_ofObject_withValue(DescriptorIndex, file, NilPointer);
                    }
                    // Update with new name
                    memory.storePointer_ofObject_withValue(FileNameIndex, file, newNameObjectPointer);
                    push(TruePointer);
                }
                else
                {
                    if (wasOpen)
                    {
                        // Something failed. Open the original file again...if possible
                        int fd = fileSystem->open_file(oldName.c_str());
                        if (fd != -1)
                        {
                            memory.storePointer_ofObject_withValue(DescriptorIndex,
                                                                   file, positive32BitIntegerFor(fd));
                            fileSystem->seek_to(fd, oldPos); // Restore position....
                        }
                        else
                            memory.storePointer_ofObject_withValue(DescriptorIndex, file, NilPointer);
                    }
                    push(FalsePointer);
                }
            }
                break;
            case 3: // Enumerate files
            {
                // One pass to get length...
                int length = 0;
                fileSystem->enumerate_files([&](const char *) {
                    length++;
                });
                
                int array = memory.instantiateClass_withPointers(ClassArrayPointer, length);
                // Push the array first so there is a refernce on it just in case GC takes place when we add the
                // entries
                push(array);
                
                // A second pass to fetch and store values
                int i = 0;
                fileSystem->enumerate_files([&](const char *name) {
                    memory.storePointer_ofObject_withValue(i, array, stringObjectFor(name));
                    i++;
                });
            }
            break;
        }
    }
    else
        unPop(4);

}



// positive16BitValueOf:
int Interpreter::positive16BitValueOf(int integerPointer)
{
   int value;

   /* "source"
   	(memory isIntegerObject: integerPointer)
   		ifTrue: [^memory integerValueOf: integerPointer].
   	(memory fetchClassOf: integerPointer) =
   			ClassLargePositiveIntegerPointer
   		ifFalse: [^self primitiveFail].
   	(memory fetchByteLengthOf: integerPointer) = 2
   		ifFalse: [^self primitiveFail].
   	value <- memory fetchByte: 1
   			ofObject: integerPointer.
   	value <- value * 256 + (memory fetchByte: 0
   					ofObject: integerPointer).
   	^value
   */
    if (memory.isIntegerObject(integerPointer))
        return memory.integerValueOf(integerPointer);
    if (memory.fetchClassOf(integerPointer) != ClassLargePositiveIntegerPointer)
        return primitiveFail();
    if (memory.fetchByteLengthOf(integerPointer) != 2)
        return primitiveFail();
    
    value = memory.fetchByte_ofObject(1, integerPointer);
    value = value * 256 + memory.fetchByte_ofObject(0, integerPointer);
    return value;
}


std::uint32_t Interpreter::positive32BitValueOf(int integerPointer)
{
    std::uint32_t value;
    if (memory.isIntegerObject(integerPointer))
        return memory.integerValueOf(integerPointer);
    if (memory.fetchClassOf(integerPointer) != ClassLargePositiveIntegerPointer)
        return primitiveFail();
    
    int bytes = memory.fetchByteLengthOf(integerPointer);
    value = memory.fetchByte_ofObject(0, integerPointer);
    if (bytes > 1)
        value |= memory.fetchByte_ofObject(1, integerPointer) << 8;
    if (bytes > 2)
        value |= memory.fetchByte_ofObject(2, integerPointer) << 16;
    if (bytes > 3)
        value |= ((std::uint32_t) memory.fetchByte_ofObject(3, integerPointer)) << 24;

    return value;
}

// primitiveResponse
bool Interpreter::primitiveResponse()
{
   int flagValue;

   /* "source"
   	primitiveIndex = 0
   		ifTrue: [flagValue <- self flagValueOf: newMethod.
   			flagValue = 5
   				ifTrue: [self quickReturnSelf.
   					^true].
   			flagValue = 6
   				ifTrue: [self quickInstanceLoad.
   					^true].
   			^false]
   		ifFalse: [self initPrimitive.
   			self dispatchPrimitives.
   			^self success]
   */
   
    if (primitiveIndex == 0)
    {
        flagValue = flagValueOf(newMethod);
        if (flagValue == 5)
        {
            quickReturnSelf();
            return true;
        }
        if (flagValue == 6)
        {
            quickInstanceLoad();
            return true;
        }
        return false;
    }
    initPrimitive();
    dispatchPrimitives();
    return success();
}


// quickInstanceLoad
void Interpreter::quickInstanceLoad()
{
   int thisReceiver;
   int fieldIndex;

   /* "source"
   	thisReceiver <- self popStack.
   	fieldIndex <- self fieldIndexOf: newMethod.
   	self push: (memory fetchPointer: fieldIndex
   			ofObject: thisReceiver)
   */
    thisReceiver = popStack();
    fieldIndex = fieldIndexOf(newMethod);
    push(memory.fetchPointer_ofObject(fieldIndex, thisReceiver));
}


// arithmeticSelectorPrimitive
void Interpreter::arithmeticSelectorPrimitive()
{
   /* "source"
   	"ERROR: the next line is redundant"
   	"self success: (memory isIntegerObject: (self stackValue: 1))."
   	self success
   		ifTrue:[currentBytecode = 176 ifTrue: [^self primitiveAdd].
   			currentBytecode = 177 ifTrue: [^self primitiveSubtract].
   			currentBytecode = 178 ifTrue: [^self primitiveLessThan].
   			currentBytecode = 179 ifTrue: [^self primitiveGreaterThan].
   			currentBytecode = 180 ifTrue: [^self primitiveLessOrEqual].
   			currentBytecode = 181 ifTrue: [^self primitiveGreaterOrEqual].
   			currentBytecode = 182 ifTrue: [^self primitiveEqual].
   			currentBytecode = 183 ifTrue: [^self primitiveNotEqual].
   			currentBytecode = 184 ifTrue: [^self primitiveMultiply].
   			currentBytecode = 185 ifTrue: [^self primitiveDivide].
   			currentBytecode = 186 ifTrue: [^self primitiveMod].
   			currentBytecode = 187 ifTrue: [^self primitiveMakePoint].
   			currentBytecode = 188 ifTrue: [^self primitiveBitShift].
   			currentBytecode = 189 ifTrue: [^self primitiveDiv].
   			currentBytecode = 190 ifTrue: [^self primitiveBitAnd].
   			currentBytecode = 191 ifTrue: [^self primitiveBitOr]]
   */

    //success(memory.isIntegerObject(stackValue(1)));
    if (success())
    {
        switch (currentBytecode)
        {
            case 176: primitiveAdd(); break;
            case 177: primitiveSubtract(); break;
            case 178: primitiveLessThan(); break;
            case 179: primitiveGreaterThan(); break;
            case 180: primitiveLessOrEqual(); break;
            case 181: primitiveGreaterOrEqual(); break;
            case 182: primitiveEqual(); break;
            case 183: primitiveNotEqual(); break;
            case 184: primitiveMultiply(); break;
            case 185: primitiveDivide(); break;
            case 186: primitiveMod(); break;
            case 187: primitiveMakePoint(); break;
            case 188: primitiveBitShift(); break;
            case 189: primitiveDiv(); break;
            case 190: primitiveBitAnd(); break;
            case 191: primitiveBitOr(); break;
        }
    }
}

// positive16BitIntegerFor:
int Interpreter::positive16BitIntegerFor(int integerValue)
{
   int newLargeInteger;

   /* "source"
   	integerValue < 0
   		ifTrue: [^self primitiveFail].
   	(memory isIntegerValue: integerValue)
   		ifTrue: [^memory integerObjectOf: integerValue].
   	newLargeInteger <- memory instantiateClass:
   					ClassLargePositiveIntegerPointer
   				withBytes: 2.
   	memory storeByte: 0
   		ofObject: newLargeInteger
   		withValue: (self lowByteOf: integerValue).
   	memory storeByte: 1
   		ofObject: newLargeInteger
   		withValue: (self highByteOf: integerValue).
   	^newLargeInteger
   */
    
    if (integerValue < 0)
    {
        return primitiveFail();
    }
    if (memory.isIntegerValue(integerValue))
        return memory.integerObjectOf(integerValue);
    newLargeInteger = memory.instantiateClass_withBytes(ClassLargePositiveIntegerPointer, 2);
    memory.storeByte_ofObject_withValue(0, newLargeInteger, lowByteOf(integerValue));
    memory.storeByte_ofObject_withValue(1, newLargeInteger, highByteOf(integerValue));
    return newLargeInteger;
}

int Interpreter::positive32BitIntegerFor(int integerValue)
{
    
    if (memory.isIntegerValue(integerValue))
        return memory.integerObjectOf(integerValue);
    int newLargeInteger;
   
    newLargeInteger = memory.instantiateClass_withBytes(ClassLargePositiveIntegerPointer, 4);
    memory.storeByte_ofObject_withValue(0, newLargeInteger, lowByteOf(integerValue));
    memory.storeByte_ofObject_withValue(1, newLargeInteger, highByteOf(integerValue));
    memory.storeByte_ofObject_withValue(2, newLargeInteger, (integerValue >> 16) & 0xff);
    memory.storeByte_ofObject_withValue(3, newLargeInteger, (integerValue >> 24) & 0xff);
    return newLargeInteger;

}


// popInteger
int Interpreter::popInteger()
{
   int integerPointer;

   /* "source"
   	integerPointer <- self popStack.
   	self success: (memory isIntegerObject: integerPointer).
   	self success
   		ifTrue: [^memory integerValueOf: integerPointer]
   */
    integerPointer = popStack();
    success(memory.isIntegerObject(integerPointer));
    if (success())
        return memory.integerValueOf(integerPointer);
   
    return std::numeric_limits<int>::min();
}


// specialSelectorPrimitiveResponse
int Interpreter::specialSelectorPrimitiveResponse()
{
   /* "source"
   	self initPrimitive.
   	(currentBytecode between: 176 and: 191)
   		ifTrue: [self arithmeticSelectorPrimitive].
   	(currentBytecode between: 192 and: 207)
   		ifTrue: [self commonSelectorPrimitive].
   	^self success
   */
    
    initPrimitive();
    if (between_and(currentBytecode, 176, 191))
        arithmeticSelectorPrimitive();
    else if (between_and(currentBytecode, 192, 207))
        commonSelectorPrimitive();
    return success();
}


// commonSelectorPrimitive
void Interpreter::commonSelectorPrimitive()
{
   int receiverClass;

   /* "source"
   	argumentCount <- self fetchInteger: (currentBytecode - 176)*2 + 1
   				ofObject: SpecialSelectorsPointer.
   	receiverClass <-
   		memory fetchClassOf: (self stackValue: argumentCount).
   	currentBytecode = 198 ifTrue: [^self primitiveEquivalent].
   	currentBytecode = 199 ifTrue: [^self primitiveClass].
   	currentBytecode = 200
   		ifTrue: [self success:
   				(receiverClass = ClassMethodContextPointer)
   				| (receiverClass = ClassBlockContextPointer).
   			^self success ifTrue: [self primitiveBlockCopy]].
   	(currentBytecode = 201) | (currentBytecode = 202)
   		ifTrue: [self success: receiverClass = ClassBlockContextPointer.
   			^self success ifTrue: [self primitiveValue]].
   	self primitiveFail
   */
    
    argumentCount = fetchInteger_ofObject((currentBytecode - 176)*2 + 1, SpecialSelectorsPointer);
    receiverClass = memory.fetchClassOf(stackValue(argumentCount));
    if (currentBytecode == 198 )
    {
        primitiveEquivalent();
    }
    else if (currentBytecode == 199 )
    {
        primitiveClass();
    }
    else if (currentBytecode == 200)
    {
        success(receiverClass == ClassMethodContextPointer || receiverClass == ClassBlockContextPointer);
        if (success())
            primitiveBlockCopy();
    }
    else if (currentBytecode == 201 || currentBytecode == 202)
    {
        success(receiverClass == ClassBlockContextPointer);
        if (success())
            primitiveValue();
    }
    else
        primitiveFail();
}



// initializeMethodCache
void Interpreter::initializeMethodCache()
{
   /* "source"
   	methodCacheSize <- 1024.
   	methodCache <- Array new: methodCacheSize
   */
    for(int i = 0; i < sizeof(methodCache)/sizeof(methodCache[0]); i++)
        methodCache[i] = NilPointer;
}


// primitiveMod
void Interpreter::primitiveMod()
{
   int integerReceiver;
   int integerArgument;
   int integerResult = 0; //silence warning

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success: integerArgument ~= 0.
   	self success
   		ifTrue: [integerResult <- integerReceiver \\ integerArgument.
   			self success: (memory isIntegerValue: integerResult)].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */
    
    integerArgument = popInteger();
    integerReceiver = popInteger();
    success(integerArgument != 0);
    if (success())
    {
        integerResult = integerReceiver % integerArgument;
        if (integerArgument < 0)
        {
            if (integerResult > 0)
            {
                integerResult += integerArgument;
            }
        } else
        {
            if (integerResult < 0)
            {
                integerResult += integerArgument;
            }
        }
       success(memory.isIntegerValue(integerResult));
    }
    if (success())
        pushInteger(integerResult);
    else
        unPop(2);
}


// dispatchArithmeticPrimitives
void Interpreter::dispatchArithmeticPrimitives()
{
    /* "source"
     primitiveIndex < 20
     ifTrue: [^self dispatchIntegerPrimitives].
     primitiveIndex < 40
     ifTrue: [^self dispatchLargeIntegerPrimitives].
     primitiveIndex < 60
     ifTrue: [^self dispatchFloatPrimitives]
     */
    if (primitiveIndex < 20)
    {
        dispatchIntegerPrimitives();
    }
    else if (primitiveIndex < 40)
    {
        dispatchLargeIntegerPrimitives();
    }
    else if (primitiveIndex < 60)
    {
        dispatchFloatPrimitives();
    }
}


// primitiveEqual
void Interpreter::primitiveEqual()
{
   int integerReceiver;
   int integerArgument;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerReceiver = integerArgument
   				ifTrue: [self push: TruePointer]
   				ifFalse: [self push: FalsePointer]]
   		ifFalse: [self unPop: 2]
   */
    
    integerArgument = popInteger();
    integerReceiver = popInteger();

    if (success())
    {
        if (integerReceiver == integerArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
    {
        unPop(2);
    }
}


// primitiveBitOr
void Interpreter::primitiveBitOr()
{
   int integerReceiver;
   int integerArgument;
   int integerResult;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerResult <- integerReceiver bitOr: integerArgument].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */

    integerArgument = popInteger();
    integerReceiver = popInteger();
    
    if (success())
    {
        integerResult = integerReceiver | integerArgument;
        pushInteger(integerResult);
    }
    else
        unPop(2);
}


// primitiveDivide
// The primitive routine for division (associated with the selector/) is different than
// the other three arithmetic primitives since it only produces a result if the division
// is exact, otherwise the primitive fails.
void Interpreter::primitiveDivide()
{
   int integerReceiver;
   int integerArgument;
   int integerResult = 0; // eliminate warning

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success: integerArgument ~= 0.
   	self success: integerReceiver \\ integerArgument = 0.
   	self success
   		ifTrue: [integerResult <- integerReceiver // integerArgument.
   			self success: (memory isIntegerValue: integerResult)].
   	self success
   		ifTrue: [ self push: (memory integerObjectOf: integerResult)]
   		ifFalse: [self unPop: 2]
   */
 
    integerArgument = popInteger();
    integerReceiver = popInteger();
    success(integerArgument != 0);
    success(integerReceiver % integerArgument == 0);
    if (success())
    {
        integerResult = integerReceiver / integerArgument;
        success(memory.isIntegerValue(integerResult));
    }
    if (success())
        push(memory.integerObjectOf(integerResult));
    else
        unPop(2);
}


// primitiveMultiply
void Interpreter::primitiveMultiply()
{
   int integerReceiver;
   int integerArgument;
   int integerResult = 0;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerResult <- integerReceiver * integerArgument.
   			self success: (memory isIntegerValue: integerResult)].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */

    integerArgument = popInteger();
    integerReceiver = popInteger();
    
    if (success())
    {
        integerResult = integerReceiver * integerArgument;
        success(memory.isIntegerValue(integerResult));
    }
    if (success())
        pushInteger(integerResult);
    else
        unPop(2);
}

// primitiveBitAnd
void Interpreter::primitiveBitAnd()
{
   int integerReceiver;
   int integerArgument;
   int integerResult;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerResult <- integerReceiver bitAnd: integerArgument].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */
    
    
    integerArgument = popInteger();
    integerReceiver = popInteger();
    
    if (success())
    {
        integerResult = integerReceiver & integerArgument;
        pushInteger(integerResult);
    }
    else
        unPop(2);
}


// primitiveSubtract
void Interpreter::primitiveSubtract()
{
   int integerReceiver;
   int integerArgument;
   int integerResult = 0;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerResult <- integerReceiver - integerArgument.
   			self success: (memory isIntegerValue: integerResult)].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */
    
   integerArgument = popInteger();
   integerReceiver = popInteger();

    if (success())
    {
        integerResult = integerReceiver - integerArgument;
        success(memory.isIntegerValue(integerResult));
    }
    if (success())
        pushInteger(integerResult);
    else
        unPop(2);
}


// dispatchIntegerPrimitives
void Interpreter::dispatchIntegerPrimitives()
{
   /* "source"
   	primitiveIndex = 1 ifTrue: [^self primitiveAdd].
   	primitiveIndex = 2 ifTrue: [^self primitiveSubtract].
   	primitiveIndex = 3 ifTrue: [^self primitiveLessThan].
   	primitiveIndex = 4 ifTrue: [^self primitiveGreaterThan].
   	primitiveIndex = 5 ifTrue: [^self primitiveLessOrEqual].
   	primitiveIndex = 6 ifTrue: [^self primitiveGreaterOrEqual].
   	primitiveIndex = 7 ifTrue: [^self primitiveEqual].
   	primitiveIndex = 8 ifTrue: [^self primitiveNotEqual].
   	primitiveIndex = 9 ifTrue: [^self primitiveMultiply].
   	primitiveIndex = 10 ifTrue: [^self primitiveDivide].
   	primitiveIndex = 11 ifTrue: [^self primitiveMod].
   	primitiveIndex = 12 ifTrue: [^self primitiveDiv].
   	primitiveIndex = 13 ifTrue: [^self primitiveQuo].
   	primitiveIndex = 14 ifTrue: [^self primitiveBitAnd].
   	primitiveIndex = 15 ifTrue: [^self primitiveBitOr].
   	primitiveIndex = 16 ifTrue: [^self primitiveBitXor].
   	primitiveIndex = 17 ifTrue: [^self primitiveBitShift].
   	primitiveIndex = 18 ifTrue: [^self primitiveMakePoint]
   */
    
    switch (primitiveIndex)
    {
        case 1: primitiveAdd(); break;
        case 2: primitiveSubtract(); break;
        case 3: primitiveLessThan(); break;
        case 4: primitiveGreaterThan(); break;
        case 5: primitiveLessOrEqual(); break;
        case 6: primitiveGreaterOrEqual(); break;
        case 7: primitiveEqual(); break;
        case 8: primitiveNotEqual(); break;
        case 9: primitiveMultiply(); break;
        case 10: primitiveDivide(); break;
        case 11: primitiveMod(); break;
        case 12: primitiveDiv(); break;
        case 13: primitiveQuo(); break;
        case 14: primitiveBitAnd(); break;
        case 15: primitiveBitOr(); break;
        case 16: primitiveBitXor(); break;
        case 17: primitiveBitShift(); break;
        case 18: primitiveMakePoint(); break;
    }
}


// primitiveGreaterOrEqual
void Interpreter::primitiveGreaterOrEqual()
{
   int integerReceiver;
   int integerArgument;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerReceiver >= integerArgument
   				ifTrue: [self push: TruePointer]
   				ifFalse: [self push: FalsePointer]]
   		ifFalse: [self unPop: 2]
   */
   integerArgument = popInteger();
   integerReceiver = popInteger();
    
    if (success())
    {
        if (integerReceiver >= integerArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);
}


// primitiveAdd
void Interpreter::primitiveAdd()
{
   int integerReceiver;
   int integerArgument;
   int integerResult = 0;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerResult <- integerReceiver + integerArgument.
   			self success: (memory isIntegerValue: integerResult)].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */
    integerArgument = popInteger();
    integerReceiver = popInteger();

     if (success())
     {
         integerResult = integerReceiver + integerArgument;
         success(memory.isIntegerValue(integerResult));
     }
     if (success())
         pushInteger(integerResult);
     else
         unPop(2);
}


// primitiveNotEqual
void Interpreter::primitiveNotEqual()
{
   int integerReceiver;
   int integerArgument;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerReceiver ~= integerArgument
   				ifTrue: [self push: TruePointer]
   				ifFalse: [self push: FalsePointer]]
   		ifFalse: [self unPop: 2]
   */
    integerArgument = popInteger();
    integerReceiver = popInteger();
     
     if (success())
     {
         if (integerReceiver != integerArgument)
             push(TruePointer);
         else
             push(FalsePointer);
     }
     else
         unPop(2);
}


// primitiveQuo
void Interpreter::primitiveQuo()
{
   int integerReceiver;
   int integerArgument;
   int integerResult = 0;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success: integerArgument ~= 0.
   	self success
   		ifTrue: [integerResult <- integerReceiver quo: integerArgument.
   			self success: (memory isIntegerValue: integerResult)].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */
    integerArgument = popInteger();
    integerReceiver = popInteger();
    success(integerArgument != 0);
    
    if (success())
    {
        integerResult = integerReceiver / integerArgument;
        success(memory.isIntegerValue(integerResult));
    }
    if (success())
        push(memory.integerObjectOf(integerResult));
    else
        unPop(2);
}


// dispatchFloatPrimitives
void Interpreter::dispatchFloatPrimitives()
{
   /* "source"
   	primitiveIndex = 40 ifTrue: [^self primitiveAsFloat].
   	primitiveIndex = 41 ifTrue: [^self primitiveFloatAdd].
   	primitiveIndex = 42 ifTrue: [^self primitiveFloatSubtract].
   	primitiveIndex = 43 ifTrue: [^self primitiveFloatLessThan].
   	primitiveIndex = 44 ifTrue: [^self primitiveFloatGreaterThan].
   	primitiveIndex = 45 ifTrue: [^self primitiveFloatLessOrEqual].
   	primitiveIndex = 46 ifTrue: [^self primitiveFloatGreaterOrEqual].
   	primitiveIndex = 47 ifTrue: [^self primitiveFloatEqual].
   	primitiveIndex = 48 ifTrue: [^self primitiveFloatNotEqual].
   	primitiveIndex = 49 ifTrue: [^self primitiveFloatMultiply].
   	primitiveIndex = 50 ifTrue: [^self primitiveFloatDivide].
   	primitiveIndex = 51 ifTrue: [^self primitiveTruncated].
   	primitiveIndex = 52 ifTrue: [^self primitiveFractionalPart].
   	primitiveIndex = 53 ifTrue: [^self primitiveExponent].
   	primitiveIndex = 54 ifTrue: [^self primitiveTimesTwoPower]
   */

    switch (primitiveIndex)
    {
        case 40: primitiveAsFloat(); break;
        case 41: primitiveFloatAdd(); break;
        case 42: primitiveFloatSubtract(); break;
        case 43: primitiveFloatLessThan(); break;
        case 44: primitiveFloatGreaterThan(); break;
        case 45: primitiveFloatLessOrEqual(); break;
        case 46: primitiveFloatGreaterOrEqual(); break;
        case 47: primitiveFloatEqual(); break;
        case 48: primitiveFloatNotEqual(); break;
        case 49: primitiveFloatMultiply(); break;
        case 50: primitiveFloatDivide(); break;
        case 51: primitiveTruncated(); break;
        case 52: primitiveFractionalPart(); break;
        case 53: primitiveExponent(); break;
        case 54: primitiveTimesTwoPower(); break;
    }

}

// The primitiveAsFloat routine converts its SmallInteger receiver into a Float.
void Interpreter::primitiveAsFloat()
{
    int integerArgument = popInteger();
    if (success())
    {
        pushFloat((float) integerArgument);
    }
    else
        unPop(1);
}

void Interpreter::primitiveFloatAdd()
{
    float floatReceiver;
    float floatArgument;
    float floatResult;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    if (success())
    {
        floatResult = floatReceiver + floatArgument;
        pushFloat(floatResult);
    }
    else
        unPop(2);
}

void Interpreter::primitiveFloatSubtract()
{
    float floatReceiver;
    float floatArgument;
    float floatResult;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    if (success())
    {
        floatResult = floatReceiver - floatArgument;
        pushFloat(floatResult);
    }
    else
        unPop(2);
}

void Interpreter::primitiveFloatLessThan()
{
    float floatReceiver;
    float floatArgument;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    
    if (success())
    {
        if (floatReceiver < floatArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);
}

void Interpreter::primitiveFloatGreaterThan()
{
    float floatReceiver;
    float floatArgument;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    
    if (success())
    {
        if (floatReceiver > floatArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);
}

void Interpreter::primitiveFloatLessOrEqual()
{
    float floatReceiver;
    float floatArgument;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    
    if (success())
    {
        if (floatReceiver <= floatArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);
}

void Interpreter::primitiveFloatGreaterOrEqual()
{
    float floatReceiver;
    float floatArgument;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    
    if (success())
    {
        if (floatReceiver >= floatArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);
}

void Interpreter::primitiveFloatEqual()
{
    float floatReceiver;
    float floatArgument;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    
    if (success())
    {
        if (floatReceiver == floatArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);
}

void Interpreter::primitiveFloatNotEqual()
{
    float floatReceiver;
    float floatArgument;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    
    if (success())
    {
        if (floatReceiver != floatArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);
}

void Interpreter::primitiveFloatMultiply()
{
    float floatReceiver;
    float floatArgument;
    float floatResult;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    if (success())
    {
        floatResult = floatReceiver * floatArgument;
        pushFloat(floatResult);
    }
    else
        unPop(2);
}

void Interpreter::primitiveFloatDivide()
{
    float floatReceiver;
    float floatArgument;
    float floatResult;
    
    floatArgument = popFloat();
    floatReceiver = popFloat();
    success(floatArgument != 0);
    if (success())
    {
        floatResult = floatReceiver / floatArgument;
        pushFloat(floatResult);
    }
    else
        unPop(2);
}

void Interpreter::primitiveTruncated()
{
    float floatReceiver;
    int integerResult = 0;
    floatReceiver = popFloat();
    if (success())
    {
        integerResult = (int) floatReceiver;
        success(memory.isIntegerValue(integerResult));
    }
    
    if (success())
    {
        pushInteger(integerResult);
    }
    else
        unPop(1);
}

void Interpreter::primitiveFractionalPart()
{
    float floatReceiver;
    float floatResult;
    floatReceiver = popFloat();
    if (success())
    {
        floatResult = floatReceiver - (int) floatReceiver;
        pushFloat(floatResult);
    }
    else
        unPop(1);
}

// primitiveLessOrEqual
void Interpreter::primitiveLessOrEqual()
{
   int integerReceiver;
   int integerArgument;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerReceiver <= integerArgument
   				ifTrue: [self push: TruePointer]
   				ifFalse: [self push: FalsePointer]]
   		ifFalse: [self unPop: 2]
   */
   integerArgument = popInteger();
   integerReceiver = popInteger();
    
    if (success())
    {
        if (integerReceiver <= integerArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);
}


// primitiveMakePoint
void Interpreter::primitiveMakePoint()
{
   int integerReceiver;
   int integerArgument;
   int pointResult;

   /* "source"

   	integerArgument <- self popStack.
   	integerReceiver <- self popStack.
   	self success: (memory isIntegerValue: integerReceiver).
   	self success: (memory isIntegerValue: integerArgument)."
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [pointResult <- memory instantiateClass: ClassPointPointer
   						withPointers: ClassPointSize.
        "ERROR: dbanay next two changed to be storeInteger:ofObject:withValue"
   			self storeInteger: XIndex
   				ofObject: pointResult
   				withValue: integerReceiver.
   			self storeInteger: YIndex
   				ofObject: pointResult
   				withValue: integerArgument.
   			self push: pointResult]
   		ifFalse: [self unPop: 2]
   */
    integerArgument = popInteger();
    integerReceiver = popInteger();
    success(memory.isIntegerValue(integerReceiver));
    success(memory.isIntegerValue(integerArgument));
    if (success())
    {
        pointResult = memory.instantiateClass_withPointers(ClassPointPointer, ClassPointSize);
        storeInteger_ofObject_withValue(XIndex, pointResult, integerReceiver);
        storeInteger_ofObject_withValue(YIndex, pointResult, integerArgument);
        push(pointResult);
    }
    else
        unPop(2);
}


// primitiveBitXor
void Interpreter::primitiveBitXor()
{
   int integerReceiver;
   int integerArgument;
   int integerResult;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerResult <- integerReceiver bitXor: integerArgument].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */
   integerArgument = popInteger();
   integerReceiver = popInteger();
   
   if (success())
   {
       integerResult = integerReceiver ^ integerArgument;
       pushInteger(integerResult);
   }
   else
       unPop(2);
}


// primitiveLessThan
void Interpreter::primitiveLessThan()
{
   int integerReceiver;
   int integerArgument;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerReceiver < integerArgument
   				ifTrue: [self push: TruePointer]
   				ifFalse: [self push: FalsePointer]]
   		ifFalse: [self unPop: 2]
   */
   integerArgument = popInteger();
   integerReceiver = popInteger();
    
    if (success())
    {
        if (integerReceiver < integerArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);


}


// primitiveBitShift
void Interpreter::primitiveBitShift()
{
   int integerReceiver;
   int integerArgument;
   int integerResult = 0;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerResult <- integerReceiver bitShift: integerArgument.
   			self success: (memory isIntegerValue: integerResult)].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */
    integerArgument = popInteger();
    integerReceiver = popInteger();
    if (success())
    {
        integerResult = integerArgument >= 0 ? integerReceiver << integerArgument : integerReceiver >> -integerArgument;
        success(memory.isIntegerValue(integerResult));
    }
    if (success())
        pushInteger(integerResult);
    else
        unPop(2);
}


// primitiveGreaterThan
void Interpreter::primitiveGreaterThan()
{
   int integerReceiver;
   int integerArgument;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success
   		ifTrue: [integerReceiver > integerArgument
   				ifTrue: [self push: TruePointer]
   				ifFalse: [self push: FalsePointer]]
   		ifFalse: [self unPop: 2]
   */
   integerArgument = popInteger();
   integerReceiver = popInteger();
    
    if (success())
    {
        if (integerReceiver > integerArgument)
            push(TruePointer);
        else
            push(FalsePointer);
    }
    else
        unPop(2);
}


// primitiveDiv
void Interpreter::primitiveDiv()
{
   int integerReceiver;
   int integerArgument;
   int integerResult = 0;

   /* "source"
   	integerArgument <- self popInteger.
   	integerReceiver <- self popInteger.
   	self success: integerArgument ~= 0.
   	self success
   		ifTrue: [integerResult <- integerReceiver // integerArgument.
   			self success: (memory isIntegerValue: integerResult)].
   	self success
   		ifTrue: [self pushInteger: integerResult]
   		ifFalse: [self unPop: 2]
   */
   integerArgument = popInteger();
   integerReceiver = popInteger();
   success(integerArgument != 0);
   if (success())
   {
       integerResult = integerReceiver / integerArgument;
       if (integerReceiver % integerArgument != 0)
       {
           // Smalltalk truncates to negative infinity ( -7 // 2 == -4 not -3)
           if (integerResult < 0)
               integerResult--;
       }
       success(memory.isIntegerValue(integerResult));
   }
   if (success())
       push(memory.integerObjectOf(integerResult));
   else
       unPop(2);

}

std::string Interpreter::stringFromObject(int stringOrSymbolPointer)
{
  
    int len = memory.fetchByteLengthOf(stringOrSymbolPointer);
    std::string s;
    s.reserve(len);

    for(int i = 0; i < len; i++)
    {
        std::uint8_t ch = memory.fetchByte_ofObject(i, stringOrSymbolPointer);
        s.append(1, ch);
    }
    return s;
 }

int Interpreter::stringObjectFor(const char *s)
{
    if (!s)
        return NilPointer;
    
    const char *p = s;
    while (*p)
        p++;
    int length = (int) (p - s);
    int objectPointer = memory.instantiateClass_withBytes(ClassStringPointer, length);
    p = s;
    for(int i = 0; i < length; i++)
    {
        memory.storeByte_ofObject_withValue(i, objectPointer, *p++);
    }
    return objectPointer;
}


#ifdef DEBUGGING_SUPPORT

std::string Interpreter::selectorName(int selector)
{
    return stringFromObject(selector);
}

std::string Interpreter::classNameOfObject(int objectPointer)
{
    return className(memory.fetchClassOf(objectPointer));
}

std::string Interpreter::className(int classPointer)
{
    if (classPointer == ClassSmallInteger)
        return "SmallInteger";
    
    if (classPointer == NilPointer)
        return "UndefinedObject";
    
    int symbol = memory.fetchPointer_ofObject(6, classPointer);
    if (memory.fetchClassOf(symbol) != ClassSymbolPointer)
        return "<unknown>";
    
    return stringFromObject(symbol);
}
#endif

// sendSelector:argumentCount:
void Interpreter::sendSelector_argumentCount(int selector, int count)
{
   int newReceiver;

   /* "source"
   	messageSelector <- selector.
   	argumentCount <- count.
   	newReceiver <- self stackValue: argumentCount.
   	self sendSelectorToClass: (memory fetchClassOf: newReceiver)
   */
    messageSelector = selector;
    argumentCount = count;
    newReceiver = stackValue(argumentCount);
    
#if 0
#ifdef DEBUGGING_SUPPORT
    // Debugging aid that helped me figure out why stuff
    // wasn't working - intercept show: and error: messages and
    // display them
    const int showSelector = 6640; // #show:
    const int errorSelector = 282; // #error:
#if 0
    bool showSend = true;

    if (selector == showSelector)
    {
        int s = stackValue(argumentCount-1);
        assert(memory.fetchClassOf(s) == ClassStringPointer);
        std::string text = stringFromObject(s);
        printf("Transcript: %s\n", text.c_str());
        showSend = false;
    }
    else if (selector == errorSelector)
    {
        int s = stackValue(argumentCount-1);
        assert(memory.fetchClassOf(s) == ClassStringPointer);
        std::string text = stringFromObject(s);
        printf("ERROR: %s\n", text.c_str());
        showSend = false;
    }

    if (showSend)
    {
        int cls = memory.fetchClassOf(newReceiver);
        std::string className = classNameOfObject(newReceiver);
        std::string sel = selectorName(selector);
        printf("sendSelector %s [%d] (args = %d) to %d (%s) 0x%d\n",
               sel.c_str(), selector, count, newReceiver, className.c_str(), cls);
    }
#endif
#endif
#endif
    sendSelectorToClass(memory.fetchClassOf(newReceiver));
}


// findNewMethodInClass:
void Interpreter::findNewMethodInClass(int cls)
{
   int hash;

   /* "source"
   	hash <- (((messageSelector bitAnd: class) bitAnd: 16rFF) bitShift: 2) + 1.
   	((methodCache at: hash) = messageSelector
   			and: [(methodCache at: hash + 1) = class])
   		ifTrue: [newMethod <- methodCache at: hash + 2.
   			primitiveIndex <- methodCache at: hash + 3]
   		ifFalse: [self lookupMethodInClass: class.
   			methodCache at: hash put: messageSelector.
   			methodCache at: hash + 1 put: class.
   			methodCache at: hash + 2 put: newMethod.
   			methodCache at: hash + 3 put: primitiveIndex]
   */
   
    // This is not a great hash function
    // Bits of History, pg.244 has a discussion and a better one
    // hash = ((messageSelector ^ cls) & 0xff) << 2;
    
    hash = (messageSelector & cls & 0xff) << 2; // removed +1 since C arrays are zero based
    if (methodCache[hash] == messageSelector && methodCache[hash+1] == cls)
    {
        newMethod = methodCache[hash+2];
        primitiveIndex = methodCache[hash+3];
    }
    else
    {

        lookupMethodInClass(cls);
        methodCache[hash] = messageSelector;
        methodCache[hash+1] = cls;
        methodCache[hash+2] = newMethod;
        methodCache[hash+3] = primitiveIndex;
    }
    
}


// activateNewMethod
void Interpreter::activateNewMethod()
{
   int contextSize;
   int newContext;
 
   /* "source"
   	(self largeContextFlagOf: newMethod) = 1
   		ifTrue: [contextSize <- 32 + TempFrameStart]
   		ifFalse: [contextSize <- 12 + TempFrameStart].
   	newContext <- memory instantiateClass: ClassMethodContextPointer
   				withPointers: contextSize.
   	memory storePointer: SenderIndex
   		ofObject: newContext
   		withValue: activeContext.
   	self storeInstructionPointerValue:
   		(self initialInstructionPointerOfMethod: newMethod)
   		inContext: newContext.
   	self storeStackPointerValue: (self temporaryCountOf: newMethod)
   		inContext: newContext.
   	memory storePointer: MethodIndex
   		ofObject: newContext
   		withValue: newMethod.
   	self transfer: argumentCount + 1
   		fromIndex: stackPointer - argumentCount
   		ofObject: activeContext
   		toIndex: ReceiverIndex
   		ofObject: newContext.
   	self pop: argumentCount + 1.
   	self newActiveContext: newContext
   */
    
    if (largeContextFlagOf(newMethod) == 1)
        contextSize = 32 + TempFrameStart;
    else
        contextSize = 12 + TempFrameStart;
    newContext = memory.instantiateClass_withPointers(
                    ClassMethodContextPointer, contextSize);
    memory.storePointer_ofObject_withValue(SenderIndex,
                    newContext, activeContext);
    storeInstructionPointerValue_inContext(
                    initialInstructionPointerOfMethod(newMethod), newContext);
    storeStackPointerValue_inContext(temporaryCountOf(newMethod), newContext);
    memory.storePointer_ofObject_withValue(MethodIndex, newContext, newMethod);
    transfer_fromIndex_ofObject_toIndex_ofObject(argumentCount + 1,
                                                 stackPointer - argumentCount,
                                                 activeContext,
                                                 ReceiverIndex,
                                                 newContext);
    pop(argumentCount + 1);
    newActiveContext(newContext);
}


// sendSpecialSelectorBytecode
void Interpreter::sendSpecialSelectorBytecode()
{
   int selectorIndex;
   int selector;
   int count;

   /* "source"
   	self specialSelectorPrimitiveResponse
   		ifFalse: [selectorIndex <- (currentBytecode - 176) * 2.
   			selector <- memory fetchPointer: selectorIndex
   					ofObject: SpecialSelectorsPointer.
   			count <- self fetchInteger: selectorIndex + 1
   					ofObject: SpecialSelectorsPointer.
   			self sendSelector: selector
   				argumentCount: count]
   */
    
    if (!specialSelectorPrimitiveResponse())
    {
        selectorIndex = (currentBytecode - 176) * 2;
        selector = memory.fetchPointer_ofObject(selectorIndex, SpecialSelectorsPointer);
        count = fetchInteger_ofObject(selectorIndex + 1, SpecialSelectorsPointer);
        sendSelector_argumentCount(selector, count);
    }
}


// doubleExtendedSuperBytecode
void Interpreter::doubleExtendedSuperBytecode()
{
   int methodClass;

   /* "source"
   	argumentCount <- self fetchByte.
   	messageSelector <- self literal: self fetchByte.
   	methodClass <- self methodClassOf: method.
   	self sendSelectorToClass: (self superclassOf: methodClass)
   */
    argumentCount = fetchByte();
    messageSelector = literal(fetchByte());
    methodClass = methodClassOf(method);
    sendSelectorToClass(superclassOf(methodClass));
}


// sendBytecode
void Interpreter::sendBytecode()
{
   /* "source"
   	(currentBytecode between: 131 and: 134)
   		ifTrue: [^self extendedSendBytecode].
   	(currentBytecode between: 176 and: 207)
   		ifTrue: [^self sendSpecialSelectorBytecode].
   	(currentBytecode between: 208 and: 255)
   		ifTrue: [^self sendLiteralSelectorBytecode]
   */
    if (between_and(currentBytecode, 131, 134))
        extendedSendBytecode();
    else if (between_and(currentBytecode, 176, 207))
        sendSpecialSelectorBytecode();
    else if (between_and(currentBytecode, 208, 255))
        sendLiteralSelectorBytecode();
    
}


// doubleExtendedSendBytecode
void Interpreter::doubleExtendedSendBytecode()
{
   int count;
   int selector;

   /* "source"
   	count <- self fetchByte.
   	selector <- self literal: self fetchByte.
   	self sendSelector: selector
   		argumentCount: count
   */
    
    count = fetchByte();
    selector = literal(fetchByte());
    sendSelector_argumentCount(selector, count);
}


// sendSelectorToClass:
void Interpreter::sendSelectorToClass(int classPointer)
{
   /* "source"
   	self findNewMethodInClass: classPointer.
   	self executeNewMethod
   */
    findNewMethodInClass(classPointer);
    executeNewMethod();
}


// sendLiteralSelectorBytecode
void Interpreter::sendLiteralSelectorBytecode()
{
   int selector;

   /* "source"
   	selector <- self literal: (self extractBits: 12 to: 15
   					of: currentBytecode).
   	self sendSelector: selector
   		argumentCount: (self extractBits: 10 to: 11
   					of: currentBytecode) - 1
   */
    selector = literal(extractBits_to_of(12, 15, currentBytecode));
    sendSelector_argumentCount(selector, extractBits_to_of(10, 11, currentBytecode) - 1);
}


// singleExtendedSuperBytecode
void Interpreter::singleExtendedSuperBytecode()
{
   int descriptor;
   int selectorIndex;
   int methodClass;

   /* "source"
   	descriptor <- self fetchByte.
   	argumentCount <- self extractBits: 8 to: 10
   				of: descriptor.
   	selectorIndex <- self extractBits: 11 to: 15
   				of: descriptor.
   	messageSelector <- self literal: selectorIndex.
   	methodClass <- self methodClassOf: method.
   	self sendSelectorToClass: (self superclassOf: methodClass)
   */
    
    descriptor = fetchByte();
    argumentCount = extractBits_to_of(8, 10, descriptor);
    selectorIndex = extractBits_to_of(11, 15, descriptor);
    messageSelector = literal(selectorIndex);
    methodClass = methodClassOf(method);
    sendSelectorToClass(superclassOf(methodClass));
}


// singleExtendedSendBytecode
void Interpreter::singleExtendedSendBytecode()
{
   int descriptor;
   int selectorIndex;

   /* "source"
   	descriptor <- self fetchByte.
   	selectorIndex <- self extractBits: 11 to: 15
   				of: descriptor.
   	self sendSelector: (self literal: selectorIndex)
   		argumentCount: (self extractBits: 8 to: 10
   					of: descriptor)
   */
    descriptor = fetchByte();
    selectorIndex = extractBits_to_of(11, 15, descriptor);
    sendSelector_argumentCount(literal(selectorIndex), extractBits_to_of(8, 10, descriptor));
}


// extendedSendBytecode
void Interpreter::extendedSendBytecode()
{
   /* "source"
   	currentBytecode = 131 ifTrue: [^self singleExtendedSendBytecode].
   	currentBytecode = 132 ifTrue: [^self doubleExtendedSendBytecode].
   	currentBytecode = 133 ifTrue: [^self singleExtendedSuperBytecode].
   	currentBytecode = 134 ifTrue: [^self doubleExtendedSuperBytecode]
   */
    
    switch (currentBytecode)
    {
        case 131: singleExtendedSendBytecode(); break;
        case 132: doubleExtendedSendBytecode(); break;
        case 133: singleExtendedSuperBytecode(); break;
        case 134: doubleExtendedSuperBytecode(); break;
    }
}


// executeNewMethod
void Interpreter::executeNewMethod()
{
   /* "source"
   	self primitiveResponse
   		ifFalse: [self activateNewMethod]
   */
    if (!primitiveResponse())
        activateNewMethod();
}


// dispatchOnThisBytecode
void Interpreter::dispatchOnThisBytecode()
{
   /* "source"
   	(currentBytecode between: 0 and: 119) ifTrue: [^self stackBytecode].
   	(currentBytecode between: 120 and: 127) ifTrue: [^self returnBytecode].
   	(currentBytecode between: 128 and: 130) ifTrue: [^self stackBytecode].
   	(currentBytecode between: 131 and: 134 ifTrue: [^self sendBytecode].
   	(currentBytecode between: 135 and: 137) ifTrue: [^self stackBytecode].
   	(currentBytecode between: 144 and: 175) ifTrue: [^self jumpBytecode].
   	(currentBytecode between: 176 and: 255) ifTrue: [^self sendBytecode]
   */
   if (between_and(currentBytecode, 0, 119)) {  stackBytecode();  }
   else if (between_and(currentBytecode, 120, 127)) {  returnBytecode();  }
   else if (between_and(currentBytecode, 128, 130)) {  stackBytecode();  }
   else if (between_and(currentBytecode, 131, 134)) {  sendBytecode();  }
   else if (between_and(currentBytecode, 135, 137)) {  stackBytecode();  }
   else if (between_and(currentBytecode, 144, 175)) {  jumpBytecode();  }
   else if (between_and(currentBytecode, 176, 255)) {  sendBytecode(); }
}


// fetchByte
int Interpreter::fetchByte()
{
   int byte;

   /* "source"
   	byte <- memory fetchByte: instructionPointer
   			ofObject: method.
   	instructionPointer <- instructionPointer + 1.
   	^byte
   */
   
    byte = memory.fetchByte_ofObject(instructionPointer, method);
    instructionPointer = instructionPointer + 1;
    return byte;
}


// cycle
void Interpreter::cycle()
{
   /* "source"
   	self checkProcessSwitch.
   	currentBytecode <- self fetchByte.
   	self dispatchOnThisBytecode
   */
    
    checkProcessSwitch();
    currentBytecode = fetchByte();
    dispatchOnThisBytecode();
}



// interpret
void Interpreter::interpret()
{
   /* "source"
   	[true] whileTrue: [self cycle]
   */
    for(;;)
    {
        cycle();
    }
}


// primitiveIndexOf:
int Interpreter::primitiveIndexOf(int methodPointer)
{
   int flagValue;

   /* "source"
   	flagValue <- self flagValueOf: methodPointer.
   	flagValue=7
   		ifTrue: [^self extractBits: 7 to: 14
   				of: (self headerExtensionOf: methodPointer)]
   		ifFalse: [^0]
   */
    flagValue = flagValueOf(methodPointer);
    if (flagValue==7)
    {
        return extractBits_to_of(7, 14, headerExtensionOf(methodPointer));
    }
    return 0;
}


// argumentCountOf:
int Interpreter::argumentCountOf(int methodPointer)
{
   int flagValue;

   /* "source"
   	flagValue <- self flagValueOf: methodPointer.
   	flagValue < 5
   		ifTrue: [^flagValue].
   	flagValue < 7
   		ifTrue: [^0]
   		ifFalse: [^self extractBits: 2 to: 6
   				of: (self headerExtensionOf: methodPointer)]
   */
    flagValue = flagValueOf(methodPointer);
    if (flagValue < 5)
        return flagValue;
    if (flagValue < 7)
        return 0;
    else
        return extractBits_to_of(2, 6, headerExtensionOf(methodPointer));
}

// methodClassOf:
int Interpreter::methodClassOf(int methodPointer)
{
   int literalCount;
   int association;

   /* "source"
   	literalCount <- self literalCountOf: methodPointer.
   	association <- self literal: literalCount - 1
   				ofMethod: methodPointer.
   	^memory fetchPointer: ValueIndex
   		ofObject: association
   */
    literalCount = literalCountOf(methodPointer);
    association = literal_ofMethod(literalCount - 1, methodPointer);
    return memory.fetchPointer_ofObject(ValueIndex, association);
}


// storeAndPopReceiverVariableBytecode
void Interpreter::storeAndPopReceiverVariableBytecode()
{
   int variableIndex;

   /* "source"
   	variableIndex <- self extractBits: 13 to: 15
   				of: currentBytecode.
   	memory storePointer: variableIndex
   		ofObject: receiver
   		withValue: self popStack
   */
    
    variableIndex = extractBits_to_of(13, 15, currentBytecode);
    memory.storePointer_ofObject_withValue(variableIndex, receiver, popStack());
}


// extendedStoreBytecode
void Interpreter::extendedStoreBytecode()
{
   int descriptor;
   int variableType;
   int variableIndex;
   int association;

   /* "source"
   	descriptor <- self fetchByte.
   	variableType <- self extractBits: 8 to: 9
   				of: descriptor.
   	variableIndex <- self extractBits: 10 to: 15
   				of: descriptor.
   	variableType = 0 ifTrue:
   		[^memory storePointer: variableIndex
   			ofObject: receiver
   			withValue: self stackTop].
   	variableType = 1 ifTrue:
   		[^memory storePointer: variableIndex + TempFrameStart
   			ofObject: homeContext
   			withValue: self stackTop].
   	variableType = 2 ifTrue:
   		[^self error: 'illegal store'].
   	variableType = 3 ifTrue:
   		[association <- self literal: variableIndex.
   		^memory storePointer: ValueIndex
   			ofObject: association
   			withValue: self stackTop]
   */
   
    descriptor    = fetchByte();
    variableType  = extractBits_to_of(8, 9, descriptor);
    variableIndex = extractBits_to_of(10, 15, descriptor);
    switch (variableType)
    {
        case 0: memory.storePointer_ofObject_withValue(variableIndex, receiver, stackTop()); break;
        case 1: memory.storePointer_ofObject_withValue(variableIndex + TempFrameStart, homeContext, stackTop()); break;
        case 2: error("illegal store");   break;
        case 3:
            association = literal(variableIndex);
            memory.storePointer_ofObject_withValue(ValueIndex, association, stackTop());
            break;
    }
}


// pushLiteralConstantBytecode
void Interpreter::pushLiteralConstantBytecode()
{
   int fieldIndex;

   /* "source"
   	fieldIndex <- self extractBits: 11 to: 15
   			of: currentBytecode.
   	self pushLiteralConstant: fieldIndex
   */
    
    fieldIndex = extractBits_to_of(11, 15, currentBytecode);
    pushLiteralConstant(fieldIndex);
}


// storeAndPopTemporaryVariableBytecode
void Interpreter::storeAndPopTemporaryVariableBytecode()
{
   int variableIndex;

   /* "source"
   	variableIndex <- self extractBits: 13 to: 15
   				of: currentBytecode.
   	memory storePointer: variableIndex + TempFrameStart
   		ofObject: homeContext
   		withValue: self popStack
   */
   variableIndex = extractBits_to_of(13, 15, currentBytecode);
   memory.storePointer_ofObject_withValue(variableIndex + TempFrameStart,
                    homeContext,
                    popStack());
}


// extendedStoreAndPopBytecode
void Interpreter::extendedStoreAndPopBytecode()
{
   /* "source"
   	self extendedStoreBytecode.
   	self popStackBytecode
   */
    
    extendedStoreBytecode();
    popStackBytecode();
}


// stackBytecode
void Interpreter::stackBytecode()
{
   /* "source"
   	(currentBytecode between: 0 and: 15)
   		ifTrue: [^self pushReceiverVariableBytecode].
   	(currentBytecode between: 16 and: 31)
   		ifTrue: [^self pushTemporaryVariableBytecode].
   	(currentBytecode between: 32 and: 63)
   		ifTrue: [^self pushLiteralConstantBytecode].
   	(currentBytecode between: 64 and: 95)
   		ifTrue: [^self pushLiteralVariableBytecode].
   	(currentBytecode between: 96 and: 103)
   		ifTrue: [^self storeAndPopReceiverVariableBytecode].
   	(currentBytecode between: 104 and: 111)
   		ifTrue: [^self storeAndPopTemporaryVariableBytecode].
   	currentBytecode = 112
   		ifTrue: [^self pushReceiverBytecode].
   	currentBytecode between: 113 and: 119)
   		ifTrue: [^self pushConstantBytecode].
   	currentBytecode = 128
   		ifTrue: [^self extendedPushBytecode].
   	currentBytecode = 129
   		ifTrue: [^self extendedStoreBytecode].
   	currentBytecode = 130
   		ifTrue: [^self extendedStoreAndPopBytecode].
   	currentBytecode = 135
   		ifTrue: [^self popStackBytecode].
   	currentBytecode = 136
   		ifTrue: [^self duplicateTopBytecode].
   	currentBytecode = 137
   		ifTrue: [^self pushActiveContextBytecode]
   */
   
    if      (between_and(currentBytecode, 0, 15))    pushReceiverVariableBytecode();
    else if (between_and(currentBytecode, 16, 31))   pushTemporaryVariableBytecode();
    else if (between_and(currentBytecode, 32, 63))   pushLiteralConstantBytecode();
    else if (between_and(currentBytecode, 64, 95))   pushLiteralVariableBytecode();
    else if (between_and(currentBytecode, 96, 103))  storeAndPopReceiverVariableBytecode();
    else if (between_and(currentBytecode, 104 ,111)) storeAndPopTemporaryVariableBytecode();
    else if (currentBytecode == 112)                 pushReceiverBytecode();
    else if (between_and(currentBytecode, 113, 119)) pushConstantBytecode();
    else if (currentBytecode == 128)                 extendedPushBytecode();
    else if (currentBytecode == 129)                 extendedStoreBytecode();
    else if (currentBytecode == 130)                 extendedStoreAndPopBytecode();
    else if (currentBytecode == 135)                 popStackBytecode();
    else if (currentBytecode == 136)                 duplicateTopBytecode();
    else if (currentBytecode == 137)                 pushActiveContextBytecode();
}


// extendedPushBytecode
void Interpreter::extendedPushBytecode()
{
   int descriptor;
   int variableType;
   int variableIndex;

   /* "source"
   	descriptor <- self fetchByte.
   	variableType <- self extractBits: 8 to: 9
   				of: descriptor.
   	variableIndex <- self extractBits: 10 to: 15
   				of: descriptor.
   	variableType=0 ifTrue: [^self pushReceiverVariable: variableIndex].
   	variableType=1 ifTrue: [^self pushTemporaryVariable: variableIndex].
   	variableType=2 ifTrue: [^self pushLiteralConstant: variableIndex].
   	variableType=3 ifTrue: [^self pushLiteralVariable: variableIndex]
   */
    descriptor = fetchByte();
    variableType = extractBits_to_of(8, 9, descriptor);
    variableIndex = extractBits_to_of(10, 15, descriptor);
    switch (variableType)
    {
        case 0: pushReceiverVariable(variableIndex); break;
        case 1: pushTemporaryVariable(variableIndex); break;
        case 2: pushLiteralConstant(variableIndex); break;
        case 3: pushLiteralVariable(variableIndex); break;
    }
}

// pushConstantBytecode
void Interpreter::pushConstantBytecode()
{
   /* "source"
   	currentBytecode = 113 ifTrue: [^self push: TruePointer].
   	currentBytecode = 114 ifTrue: [^self push: FalsePointer].
   	currentBytecode = 115 ifTrue: [^self push: NilPointer].
   	currentBytecode = 116 ifTrue: [^self push: MinusOnePointer].
   	currentBytecode = 117 ifTrue: [^self push: ZeroPointer].
   	currentBytecode = 118 ifTrue: [^self push: OnePointer].
   	currentBytecode = 119 ifTrue: [^self push: TwoPointer]
   */
    switch (currentBytecode)
    {
        case 113: push(TruePointer); break;
        case 114: push(FalsePointer); break;
        case 115: push(NilPointer); break;
        case 116: push(MinusOnePointer); break;
        case 117: push(ZeroPointer); break;
        case 118: push(OnePointer); break;
        case 119: push(TwoPointer); break;
    }
}


// jumpIf:by:
void Interpreter::jumpIf_by(int condition, int offset)
{
   int boolean;

   /* "source"
   	boolean <- self popStack.
   	boolean = condition
   		ifTrue: [self jump: offset]
   		ifFalse: [(boolean=TruePointer) | (boolean=FalsePointer)
   				ifFalse: [self unPop: 1.
   					self sendMustBeBoolean]]
   */
    
    boolean = popStack();
    if (boolean == condition)
    {
        jump(offset);
    }
    else
    {
        if (!(boolean == TruePointer || boolean == FalsePointer))
        {
            unPop(1);
            sendMustBeBoolean();
        }
    }
}


// longConditionalJump
void Interpreter::longConditionalJump()
{
   int offset;

   /* "source"
   	offset <- self extractBits: 14 to: 15
   			of: currentBytecode.
   	offset <- offset * 256 + self fetchByte.
   	(currentBytecode between: 168 and: 171)
   		ifTrue: [^self jumpIf: TruePointer
   				by: offset].
   	(currentBytecode between: 172 and: 175)
   		ifTrue: (^self jumpIf: FalsePointer
   				by: offset]
   */
    offset = extractBits_to_of(14, 15, currentBytecode);
    offset = offset * 256 + fetchByte();
    if (between_and(currentBytecode, 168, 171))
    {
        jumpIf_by(TruePointer, offset);
    }
    else if (between_and(currentBytecode, 172, 175))
    {
        jumpIf_by(FalsePointer, offset);
    }
}

// jumpBytecode
void Interpreter::jumpBytecode()
{
   /* "source"
   	(currentBytecode between: 144 and: 151)
   		ifTrue: [^self shortUnconditionalJump].
   	(currentBytecode between: 152 and: 159)
   		ifTrue: [^self shortConditionalJump].
   	(currentBytecode between: 160 and: 167)
   		ifTrue: [^self longUnconditionalJump].
   	(currentBytecode between: 168 and: 175)
   		ifTrue: [^self longConditionalJump]
   */
   
   if (between_and( currentBytecode, 144, 151)) shortUnconditionalJump();
   else if (between_and( currentBytecode, 152, 159)) shortConditionalJump();
   else if (between_and( currentBytecode, 160, 167)) longUnconditionalJump();
   else if (between_and( currentBytecode, 168, 175)) longConditionalJump();
}

// storeInteger:ofObject:withValue:
void Interpreter::storeInteger_ofObject_withValue(int fieldIndex, int objectPointer, int integerValue)
{
   int integerPointer;

   /* "source"
   	(memory isIntegerValue: integerValue)
   		ifTrue: [integerPointer <- memory integerObjectOf: integerValue.
   			memory storePointer: fieldIndex
   				ofObject: objectPointer
   				withValue: integerPointer]
   		ifFalse: [^self primitiveFail]
   */
    if (memory.isIntegerValue(integerValue))
    {
        integerPointer = memory.integerObjectOf(integerValue);
        memory.storePointer_ofObject_withValue(fieldIndex, objectPointer, integerPointer);
    }
    else
        primitiveFail();
}


// transfer:fromIndex:ofObject:toIndex:ofObject:
void Interpreter::transfer_fromIndex_ofObject_toIndex_ofObject(
      int count, 
      int firstFrom, 
      int fromOop, 
      int firstTo, 
      int toOop
   )
{
   int fromIndex;
   int toIndex;
   int lastFrom;
   int oop;

   /* "source"
   	fromIndex <- firstFrom.
   	lastFrom <- firstFrom + count.
   	toIndex <- firstTo.
   	[fromIndex < lastFrom] whileTrue:
   		[oop <- memory fetchPointer: fromIndex
   				ofObject: fromOop.
   		memory storePointer: toIndex
   			ofObject: toOop
   			withValue: oop.
   		memory storePointer: fromIndex
   			ofObject: fromOop
   			withValue: NilPointer.
   		fromIndex <- fromIndex + 1.
   		toIndex <- toIndex + 1]
   */
    
    fromIndex = firstFrom;
    lastFrom = firstFrom + count;
    toIndex = firstTo;
    while (fromIndex < lastFrom)
    {
        oop = memory.fetchPointer_ofObject(fromIndex, fromOop);
        memory.storePointer_ofObject_withValue(toIndex, toOop, oop);
        memory.storePointer_ofObject_withValue(fromIndex, fromOop, NilPointer);
        fromIndex = fromIndex + 1;
        toIndex = toIndex + 1;
    }
}

// fetchInteger:ofObject:
int Interpreter::fetchInteger_ofObject(int fieldIndex, int objectPointer)
{
   int integerPointer;

   /* "source"
   	integerPointer <- memory fetchPointer: fieldIndex
   				 ofObject: objectPointer.
   	(memory isIntegerObject: integerPointer)
   		ifTrue: [^memory integerValueOf: integerPointer]
   		ifFalse: [^self primitiveFail]
   */
    integerPointer = memory.fetchPointer_ofObject(fieldIndex, objectPointer);
    if (memory.isIntegerObject(integerPointer))
        return memory.integerValueOf(integerPointer);
    else
       return  primitiveFail();
}

// primitiveNewMethod
void Interpreter::primitiveNewMethod()
{
   int header;
   int bytecodeCount;
   int cls;
   int size;

   /* "source"
   	header <- self popStack.
   	bytecodeCount <- self popInteger.
   	class <- self popStack.
   	size <- (self literalCountOfHeader: header) + 1 * 2 + bytecodeCount.
    "ERROR: dbanay - need tp initialize literal frame"
   	self push: (memory instantiateClass: class
   				withBytes: size)
   */
    
    header = popStack();
    bytecodeCount = popInteger();
    cls = popStack();
    int literalCount = literalCountOfHeader(header);
    size = (literalCount + 1) * 2 + bytecodeCount;
    int newMethod = memory.instantiateClass_withBytes(cls, size);
    for(int i = 0; i < literalCount; i++)
    {
        memory.storeWord_ofObject_withValue(LiteralStart + i, newMethod, NilPointer);
    }
    // Note: using storeWord vs storePointer because it memory with
    // initialized with zeros and this break ref counting
    memory.storeWord_ofObject_withValue(HeaderIndex, newMethod, header);
    push(newMethod);
}


// primitiveAsOop
void Interpreter::primitiveAsOop()
{
   int thisReceiver;

   /* "source"
   	thisReceiver <- self popStack.
   	self success: (memory isIntegerObject: thisReceiver) == false.
   	self success
   		ifTrue: [self push: (thisReceiver bitOr: 1)]
   		ifFalse: [self unPop: 1]
   */
    thisReceiver = popStack();
    success(memory.isIntegerObject(thisReceiver) == false);
    if (success())
        push(thisReceiver | 1);
    else
        unPop(1);
}


// primitiveSomeInstance
void Interpreter::primitiveSomeInstance()
{
   int cls;
   int instance;


   /* "source"
   	class <- self popStack.
   	"ERROR: instancesOf not defined in G&R, body of
   	method has been changed completely from:
   	(memory instancesOf: class)
   		ifTrue: [self push: (memory initialInstanceOf: class)]
   		ifFalse: [self primitiveFail]
   	to:"
   
   	instance <- memory initialInstanceOf: class.
   	instance = NilPointer
   		ifTrue: [self primitiveFail]
   		ifFalse: [self push: instance]
   */
    
    cls = popStack();
    instance = memory.initialInstanceOf(cls);
    if (instance == NilPointer)
        primitiveFail();
    else
        push(instance);
}


// primitiveObjectAt
void Interpreter::primitiveObjectAt()
{
   int thisReceiver;
   int index;

   /* "source"
   	index  <- self popInteger.
   	thisReceiver <- self popStack.
   	self success: index > 0.
   	self success: index <= (self objectPointerCountOf: thisReceiver).
   	self success
   		ifTrue: [self push: (memory fetchPointer: index - 1
   					ofObject: thisReceiver)]
   		ifFalse: [self unPop: 2]
   */
    index = popInteger();
    thisReceiver = popStack();
    success(index > 0);
    success(index <= objectPointerCountOf(thisReceiver));
    if (success())
    {
        push(memory.fetchPointer_ofObject(index - 1, thisReceiver));
    }
    else
        unPop(2);

}


// primitiveNextInstance
void Interpreter::primitiveNextInstance()
{
   int object;
   int instance;

   /* "source"
   	object <- self popStack.
   	"ERROR: isLastInstance: not defined in G&R, body of
   	method has been changed completely from:
   	(memory isLastInstance: object)
   		ifTrue: [self primitiveFail]
   		ifFalse: [self push: (memory instanceAfter: object)]
   	to:"
   
   	instance <- memory instanceAfter: object.
   	instance = NilPointer
   		ifTrue: [self primitiveFail]
   		ifFalse: [self push: instance]
   */
    object = popStack();
    instance = memory.instanceAfter(object);
    if (instance == NilPointer)
        primitiveFail();
    else
        push(instance);
}


// primitiveNew
void Interpreter::primitiveNew()
{
   int cls;
   int size;

   /* "source"
   	class <- self popStack.
   	size <- self fixedFieldsOf: class.
   	self success: (self isIndexable: class) == false.
   	self success
   		ifTrue: [(self isPointers: class)
   				ifTrue: [self push: (memory instantiateClass: class
   							withPointers: size)]
   				ifFalse: [self push: (memory instantiateClass: class
   							withWords: size)]]
   		ifFalse: [self unPop: 1]
   */
   
    cls = popStack();
    size = fixedFieldsOf(cls);
    success(isIndexable(cls) == false);
    if (success())
    {
        if (isPointers(cls))
        {
            push(memory.instantiateClass_withPointers(cls, size));
        }
        else
        {
            push(memory.instantiateClass_withWords(cls, size));
        }
    }
    else
        unPop(1);
}


// primitiveAsObject
void Interpreter::primitiveAsObject()
{
   int thisReceiver;
   int newOop;

   /* "source"
   	thisReceiver <- self popStack.
   	newOop <- thisReceiver bitAnd: 16rFFFE.
   	"ERROR: hasObject not defined"
   	self success: (memory hasObject: newOop).
   	self success
   		ifTrue: [self push: newOop]
   		ifFalse: [self unPop: 1]
   */
    
    thisReceiver = popStack();
    newOop = thisReceiver & 0xFFFE;
    success(memory.hasObject(newOop));
    if (success())
        push(newOop);
    else
        unPop(1);
   
}


// primitiveNewWithArg
void Interpreter::primitiveNewWithArg()
{
   int size;
   int cls;

   /* "source"
   	size <- self positive16BitValueOf: self popStack.
    self success: size <= 65533. "dbanay: ERROR check max size"
   	class <- self popStack.
   	self success: (self isIndexable: class).
   	self success
   		ifTrue: [size <- size + (self fixedFieldsOf: class).
   			(self isPointers: class)
   				ifTrue: [self push: (memory instantiateClass: class
   							withPointers: size)]
   				ifFalse: [(self isWords: class)
   						ifTrue: [self push: (memory
   									instantiateClass: class
   									withWords: size)]
   						ifFalse: [self push: (memory
   									instantiateClass: class
   									withBytes: size)]]]
   		ifFalse: [self unPop: 2]
   */
    
    size = positive16BitValueOf(popStack());
    success(size <= 65533);
    
    cls  = popStack();
    success(isIndexable(cls));
    if (success())
    {
        size = size + fixedFieldsOf(cls);
        if (isPointers(cls))
        {
            push(memory.instantiateClass_withPointers(cls, size));
        }
        else
        {
            if (isWords(cls))
            {
                push(memory.instantiateClass_withWords(cls, size));
            }
            else
            {
                push(memory.instantiateClass_withBytes(cls, size));
            }
        }
    }
    else
        unPop(2);
}


// primitiveInstVarAtPut
void Interpreter::primitiveInstVarAtPut()
{
   int thisReceiver;
   int index;
   int newValue;

   /* "source"
   	"ERROR: realValue is not used in the method"
   	newValue <- self popStack.
   	index <- self popInteger.
   	thisReceiver <- self popStack.
   	self checkInstanceVariableBoundsOf: index
   		in: thisReceiver.
   	self success
   		ifTrue: [self subscript: thisReceiver
   				with: index
   				storing: newValue].
   	self success
   		ifTrue: [self push: newValue]
   		ifFalse: [self unPop: 3]
   */
    
    newValue = popStack();
    index = popInteger();
    thisReceiver = popStack();
    checkIndexableBoundsOf_in(index, thisReceiver);
    if (success())
    {
        subscript_with_storing(thisReceiver, index, newValue);
    }
    if (success())
        push(newValue);
    else
        unPop(3);
}


// primitiveObjectAtPut
void Interpreter::primitiveObjectAtPut()
{
   int thisReceiver;
   int index;
   int newValue;

   /* "source"
   	newValue <- self popStack.
   	index <- self popInteger.
   	thisReceiver <- self popStack.
   	self success: index > 0.
   	self success: index <= (self objectPointerCountOf: thisReceiver).
   	self success
   		ifTrue: [memory storePointer: index - 1
   				ofObject: thisReceiver
   				withValue: newValue.
   			self push: newValue]
   		ifFalse: [self unPop: 3]
   */
   
    newValue = popStack();
    index = popInteger();
    thisReceiver = popStack();
    success(index > 0);
    success(index <= objectPointerCountOf(thisReceiver));
    if (success())
    {
        memory.storePointer_ofObject_withValue(index - 1, thisReceiver, newValue);
        push(newValue);
    }
    else
        unPop(3);

}


// primitiveInstVarAt
void Interpreter::primitiveInstVarAt()
{
   int thisReceiver;
   int index;
   int value = 0;

   /* "source"
   	index <- self popInteger.
   	thisReceiver <- self popStack.
   	self checkInstanceVariableBoundsOf: index
   		in: thisReceiver.
   	self success
   		ifTrue: [value <- self subscript: thisReceiver
   					with: index].
   	self success
   		ifTrue: [self push: value]
   		ifFalse: [self unPop: 2]
   */
   
    index = popInteger();
    thisReceiver = popStack();
    checkInstanceVariableBoundsOf_in(index, thisReceiver);
    if (success())
        value  = subscript_with(thisReceiver, index);
    if (success())
        push(value);
    else
        unPop(2);
    
}


// primitiveBecome
void Interpreter::primitiveBecome()
{
   int thisReceiver;
   int otherPointer;

   /* "source"
   	otherPointer <- self popStack.
   	thisReceiver <- self popStack.
   	self success: (memory isIntegerObject: otherPointer) not.
   	self success: (memory isIntegerObject: thisReceiver) not.
   	self success
   		ifTrue: [memory swapPointersOf: thisReceiver and: otherPointer.
   			self push: thisReceiver]
   		ifFalse: [self unPop: 2]
   */
    otherPointer = popStack();
    thisReceiver = popStack();
    success(!memory.isIntegerObject(otherPointer));
    success(!memory.isIntegerObject(thisReceiver));
    if (success())
    {
        memory.swapPointersOf_and(thisReceiver, otherPointer);
        push(thisReceiver);
    }
    else
        unPop(2);
}


// dispatchStorageManagementPrimitives
void Interpreter::dispatchStorageManagementPrimitives()
{
   /* "source"
   	primitiveIndex = 68 ifTrue: [^self primitiveObjectAt].
   	primitiveIndex = 69 ifTrue: [^self primitiveObjectAtPut].
   	primitiveIndex = 70 ifTrue: [^self primitiveNew].
   	primitiveIndex = 71 ifTrue: [^self primitiveNewWithArg].
   	primitiveIndex = 72 ifTrue: [^self primitiveBecome].
   	primitiveIndex = 73 ifTrue: [^self primitiveInstVarAt].
   	primitiveIndex = 74 ifTrue: [^self primitiveInstVarAtPut].
   	primitiveIndex = 75 ifTrue: [^self primitiveAsOop].
   	primitiveIndex = 76 ifTrue: [^self primitiveAsObject].
   	primitiveIndex = 77 ifTrue: [^self primitiveSomeInstance].
   	primitiveIndex = 78 ifTrue: [^self primitiveNextInstance].
   	primitiveIndex = 79 ifTrue: [^self primitiveNewMethod]
   */
    switch (primitiveIndex)
    {
        case 68: primitiveObjectAt(); break;
        case 69: primitiveObjectAtPut(); break;
        case 70: primitiveNew(); break;
        case 71: primitiveNewWithArg(); break;
        case 72: primitiveBecome(); break;
        case 73: primitiveInstVarAt(); break;
        case 74: primitiveInstVarAtPut(); break;
        case 75: primitiveAsOop(); break;
        case 76: primitiveAsObject(); break;
        case 77: primitiveSomeInstance(); break;
        case 78: primitiveNextInstance(); break;
        case 79: primitiveNewMethod(); break;
    }
}

void Interpreter::pushFloat(float f)
{
    std::uint32_t uint32 = *(std::uint32_t *) &f;
    int objectPointer = memory.instantiateClass_withWords(ClassFloatPointer, 2);
    memory.storeWord_ofObject_withValue(0, objectPointer, uint32 & 0xffff);
    memory.storeWord_ofObject_withValue(1, objectPointer, uint32 >> 16);

    push(objectPointer);
}

float Interpreter::popFloat()
{
    int objectPointer = popStack();
    success(memory.fetchClassOf(objectPointer) == ClassFloatPointer);
    if (success())
    {
        return extractFloat(objectPointer);
    }
    
    return std::nanf("");
}


