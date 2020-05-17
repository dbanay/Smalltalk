//
//  objmemory.h
//  Smalltalk-80
//
//  Created by Dan Banay on 2/20/20.
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

#include <cstdint>
#include <cassert>
#include <functional>
#include "hal.h"
#include "filesystem.h"
#include "realwordmemory.h"
#include "oops.h"

// The Smalltalk-80 VM generates a tremendous amount of circular references as it runs
//  -- primarily a MethodContext that references a BlockContext (from a temp field) that
// has a back reference to that MethodContext (the sender field). If a reference counting only
// scheme is used, then free object table entries will eventually be consumed. If, on the other hand,
// a GC only approach is used then memory will fill up with contexts and GC will happen fairly
// frequently. Therefore, the hybrid reference counting approach with full garbage collection
// when too much circular garbage accumulates is recommended.

// GM_MARK_SWEEP and GC_REF_COUNT are not mutually exclusive!
// You can define *BOTH* for a hybrid collector which ref counts until
// memory is exhausted (cyclical data) and then does a full GC

// Mark and sweep collection when memory full
#define GC_MARK_SWEEP

// Ref counting
#define GC_REF_COUNT


// Define to use recursive marking for ref counting/GC
// If undefined the stack space efficient pointer reversal approach described
// on page 678 of G&R is used. Not recommended, and only included for completeness.
//#define RECURSIVE_MARKING

// Perform range checks etc. at runtime
#define RUNTIME_CHECKING

#ifdef RUNTIME_CHECKING
#define RUNTIME_CHECK2(c,f,l) runtime_check(c, "RUNTIME ERROR: (" #c ") at: " f "(" #l ")")
#define RUNTIME_CHECK1(c,f,l) RUNTIME_CHECK2(c,f,l)
#define RUNTIME_CHECK(cond) RUNTIME_CHECK1(cond,  __FILE__, __LINE__)
#else
#define RUNTIME_CHECK(cond) ((void)0)
#endif

#ifdef GC_MARK_SWEEP
class IGCNotification
{
public:
    // About to garbage collect. Client should call addRoot to specify roots of the world
    virtual void prepareForCollection() = 0;
    // Garbage collection has been completed
    virtual void collectionCompleted() = 0;
};

#endif

class ObjectMemory
{
public:
#ifdef GC_MARK_SWEEP
    ObjectMemory(IHardwareAbstractionLayer *halInterface, IGCNotification *notification = 0);
#else
    ObjectMemory(IHardwareAbstractionLayer *halInterface);
#endif
    
    bool loadSnapshot(IFileSystem *fileSystem, const char *imageFileName);
    bool saveSnapshot(IFileSystem *fileSystem, const char *imageFileName);
        
    
    // --- BCIInterface ---

    inline int oopsLeft()
    {
        return freeOops;
    }

    inline std::uint32_t coreLeft()
    {
        return freeWords;
    }


    void garbageCollect();
    
    // storePointer:ofObject:withValue:
    int storePointer_ofObject_withValue(int fieldIndex, int objectPointer, int valuePointer);
    
    // storeWord:ofObject:withValue:
    int storeWord_ofObject_withValue(int wordIndex, int objectPointer, int valueWord);
    
    // increaseReferencesTo:
    inline void increaseReferencesTo(int objectPointer)
    {
        /* "source"
         self countUp: objectPointer
        */
#ifdef GC_REF_COUNT
         countUp(objectPointer);
#endif
    }
    
    // initialInstanceOf:
    int initialInstanceOf(int classPointer);
    
    // decreaseReferencesTo:
    inline void decreaseReferencesTo(int objectPointer)
    {
       /* "source"
        self countDown: objectPointer
       */
#ifdef GC_REF_COUNT
        countDown(objectPointer);
#endif
    }

    
    // isIntegerValue:
    inline bool isIntegerValue(int valueWord)
    {
       /* "source"
        "ERROR: G&R really cock this up"
        "dbanay - still broken in July 1985 ed!"
        ^valueWord >= -16384 and: [valueWord <= 16383]
       */
       
        return valueWord >= -16384 && valueWord <= 16383;
    }

    
    // fetchWord:ofObject:
    inline int fetchWord_ofObject(int wordIndex, int objectPointer)
    {
       /* "source"
        ^self heapChunkOf: objectPointer word: HeaderSize + wordIndex
       */
       
        RUNTIME_CHECK(wordIndex >= 0 && wordIndex < fetchWordLengthOf(objectPointer));
        return heapChunkOf_word(objectPointer, HeaderSize + wordIndex);
    }

    
    // integerValueOf:
    inline int integerValueOf(int objectPointer)
    {
       /* "source"
           ^objectPointer/2
       */
     
        
        return (std::int16_t)(objectPointer & 0xfffe)/2;
        // Right shifting a negative number is undefined according to the standard.
        // return ((std::int16_t) objectPointer) >> 1;
    }
    
    // swapPointersOf:and:
    void swapPointersOf_and(int firstPointer, int secondPointer);
    
    // fetchWordLengthOf:
    inline int fetchWordLengthOf(int objectPointer)
    {
       /* "source"
        ^(self sizeBitsOf: objectPointer) - HeaderSize
       */

        return sizeBitsOf(objectPointer) - HeaderSize;
    }
    
    // instantiateClass:withWords:
    int instantiateClass_withWords(int classPointer, int length);
    
    // isIntegerObject:
    inline bool isIntegerObject(int objectPointer)
    {
       /* "source"
        ^(objectPointer bitAnd: 1) = 1
       */
       
        return (objectPointer & 1) == 1;
    }

    
    // instantiateClass:withBytes:
    int instantiateClass_withBytes(int classPointer, int length);
    
    // hasObject:
    bool hasObject(int objectPointer);
    
    // instantiateClass:withPointers:
    int instantiateClass_withPointers(int classPointer, int length);
    
    // fetchByte:ofObject:
    inline int fetchByte_ofObject(int byteIndex, int objectPointer)
    {
        /* "source"
         ^self heapChunkOf: objectPointer byte: (HeaderSize*2 + byteIndex)
         */
        return heapChunkOf_byte(objectPointer, (HeaderSize*2 + byteIndex));
    }
    
    // fetchPointer:ofObject:
    inline int fetchPointer_ofObject(int fieldIndex, int objectPointer)
    {
        /* "source"
         ^self heapChunkOf: objectPointer word: HeaderSize + fieldIndex
         */
        RUNTIME_CHECK(fieldIndex >= 0 && fieldIndex < fetchWordLengthOf(objectPointer));
        return heapChunkOf_word(objectPointer, HeaderSize + fieldIndex);
    }
    
    // fetchClassOf:
    inline int fetchClassOf(int objectPointer)
    {
        /* Note that fetchClassOf:objectPointer returns IntegerClass (the object table index of SmallInteger)
            if its argument is an immediate integer. G&R pg 686 */
       /* "source"
        (self isIntegerObject: objectPointer)
            ifTrue: [^IntegerClass] "ERROR IntegerClass not defined"
            ifFalse: [^self classBitsOf: objectPointer]
       */
        
        if (isIntegerObject(objectPointer))
            return ClassSmallInteger;
        
        return classBitsOf(objectPointer);

    }
    
    // integerObjectOf:
    inline int integerObjectOf(int value)
    {
       /* "source"
        ^(value bitShift: 1) + 1
       */
        return (std::uint16_t) ((value << 1) | 1);
    }
    
    // fetchByteLengthOf:
    inline int fetchByteLengthOf(int objectPointer)
    {
       /* "source"
        "ERROR in selector of next line"
        ^(self fetchWordLengthOf: objectPointer)*2 - (self oddBitOf: objectPointer)
       */
        return fetchWordLengthOf(objectPointer)*2 - oddBitOf(objectPointer);
    }

    
    // instanceAfter:
    int instanceAfter(int objectPointer);
    
    // storeByte:ofObject:withValue:
    inline int storeByte_ofObject_withValue(int byteIndex, int objectPointer, int valueByte)
    {
       /* "source"
        ^self heapChunkOf: objectPointer
            byte: (HeaderSize*2 + byteIndex)
            put: valueByte
       */
       
        return heapChunkOf_byte_put(objectPointer,
                HeaderSize*2 + byteIndex,
                valueByte);
    }
    
    
    // --- ObjectPointers ---
    
    // cantBeIntegerObject:
    void cantBeIntegerObject(int objectPointer);
        
    #ifdef GC_MARK_SWEEP
        void addRoot(int rootObjectPointer) //dbanay
        {
            markObjectsAccessibleFrom(rootObjectPointer);
        }
    #endif
    
private:
    
    // --- Compaction ---
    
    // sweepCurrentSegmentFrom:
    int sweepCurrentSegmentFrom(int lowWaterMark);
    
    // compactCurrentSegment
    void compactCurrentSegment();
    
    // releasePointer:
    void releasePointer(int objectPointer);
    
    // reverseHeapPointersAbove:
    void reverseHeapPointersAbove(int lowWaterMark);
    
    // abandonFreeChunksInSegment:
    int abandonFreeChunksInSegment(int segment);
    
    // allocateChunk:
    int allocateChunk(int size);
    
#ifdef GC_MARK_SWEEP
    // --- MarkingGarbage ---
    
    // reclaimInaccessibleObjects
    void reclaimInaccessibleObjects();
    
    // markObjectsAccessibleFrom:
    int markObjectsAccessibleFrom(int rootObjectPointer);
    
    // markAccessibleObjects
    void markAccessibleObjects();
    
    // rectifyCountsAndDeallocateGarbage
    void rectifyCountsAndDeallocateGarbage();
    
    // zeroReferenceCounts
    void zeroReferenceCounts();
    
#endif
    
    // --- NonpointerObjs ---
    
    // lastPointerOf:
    int lastPointerOf(int objectPointer);
    
    // spaceOccupiedBy:
    int spaceOccupiedBy(int objectPointer);
    
    // allocate:odd:pointer:extra:class:
    int allocate_odd_pointer_extra_class(
                                         int size,
                                         int oddBit,
                                         int pointerBit,
                                         int extraWord,
                                         int classPointer
                                         );
    
    
    // --- UnallocatedSpc ---
    
    // headOfFreePointerList
    int headOfFreePointerList();
    
    // toFreeChunkList:add:
    void toFreeChunkList_add(int size, int objectPointer);
    
    // headOfFreeChunkList:inSegment:put:
    int headOfFreeChunkList_inSegment_put(int size, int segment, int objectPointer);
    
    // removeFromFreePointerList
    int removeFromFreePointerList();
    
    // toFreePointerListAdd:
    void toFreePointerListAdd(int objectPointer);
    
    // removeFromFreeChunkList:
    int removeFromFreeChunkList(int size);
    
    // resetFreeChunkList:inSegment:
    void resetFreeChunkList_inSegment(int size, int segment);
    
    // headOfFreeChunkList:inSegment:
    int headOfFreeChunkList_inSegment(int size, int segment);
    
    // headOfFreePointerListPut:
    int headOfFreePointerListPut(int objectPointer);
    
    
    // --- RefCntGarbage ---
    
    // countDown:
    int countDown(int rootObjectPointer);
    
    // countUp:
    int countUp(int objectPointer);
    
    // deallocate:
    void deallocate(int objectPointer);
    
    // forAllOtherObjectsAccessibleFrom:suchThat:do:
    int forAllOtherObjectsAccessibleFrom_suchThat_do(
                                                     int objectPointer,
                                                     const std::function <bool (int)>& predicate,
                                                     const std::function <void (int)>& action
                                                     );
    
    // forAllObjectsAccessibleFrom:suchThat:do:
    int forAllObjectsAccessibleFrom_suchThat_do(int objectPointer,
                                                const std::function <bool (int)>& predicate,
                                                const std::function <void (int)>& action);

    
    // --- ObjectTableEnt ---
    
    // segmentBitsOf:
    inline int segmentBitsOf(int objectPointer)
    {
       /* "source"
        ^self ot: objectPointer bits: 12 to: 15
       */
       
        return ot_bits_to(objectPointer, 12, 15);
    }
    
    // heapChunkOf:byte:put:
    inline int heapChunkOf_byte_put(int objectPointer, int offset, int value)
    {
       /* "source"
        ^wordMemory segment: (self segmentBitsOf: objectPointer)
            word: ((self locationBitsOf: objectPointer) + (offset//2))
            byte: (offset\\2) put: value
       */
       
        return wordMemory.segment_word_byte_put(segmentBitsOf(objectPointer),
                locationBitsOf(objectPointer) + (offset/2),
                offset % 2, value);
    }

    
    // pointerBitOf:put:
    inline int pointerBitOf_put(int objectPointer, int value)
    {
       /* "source"
        ^self ot: objectPointer bits: 9 to: 9 put: value
       */
       
        return ot_bits_to_put(objectPointer, 9, 9, value);
    }

    
    // heapChunkOf:word:
    inline int heapChunkOf_word(int objectPointer, int offset)
    {
       /* "source"
        ^wordMemory segment: (self segmentBitsOf: objectPointer)
            word: ((self locationBitsOf: objectPointer) + offset)
       */
        return wordMemory.segment_word(segmentBitsOf(objectPointer),
                locationBitsOf(objectPointer) + offset);
    }
    
    // segmentBitsOf:put:
    inline int segmentBitsOf_put(int objectPointer, int value)
    {
        /* "source"
         ^self ot: objectPointer bits: 12 to: 15 put: value
         */
        
        return ot_bits_to_put(objectPointer, 12, 15, value);
    }
    
    // heapChunkOf:word:put:
    inline int heapChunkOf_word_put(int objectPointer, int offset, int value)
    {
       /* "source"
        ^wordMemory segment: (self segmentBitsOf: objectPointer)
            word: ((self locationBitsOf: objectPointer) + offset)
            put: value
       */
        return wordMemory.segment_word_put(segmentBitsOf(objectPointer),
                locationBitsOf(objectPointer) + offset,
                value);
    }
    
    // oddBitOf:
    inline int oddBitOf(int objectPointer)
    {
        /* "source"
         ^self ot: objectPointer bits: 8 to: 8
         */
        
        return ot_bits_to(objectPointer, 8, 8);
    }

    
    // freeBitOf:
    inline int freeBitOf(int objectPointer)
    {
       /* "source"
        ^self ot: objectPointer bits: 10 to: 10
       */
       
        return ot_bits_to(objectPointer, 10, 10);
    }
    
    // locationBitsOf:
    inline int locationBitsOf(int objectPointer)
    {
       /* "source"
        self cantBeIntegerObject: objectPointer.
        ^wordMemory segment: ObjectTableSegment
            word: ObjectTableStart + objectPointer + 1
       */
        cantBeIntegerObject(objectPointer);
        return wordMemory.segment_word(ObjectTableSegment,
            ObjectTableStart + objectPointer + 1);
    }
    
    // ot:
    inline int ot(int objectPointer)
    {

       /* "source"
        self cantBeIntegerObject: objectPointer.
        ^wordMemory segment: ObjectTableSegment
            word: ObjectTableStart + objectPointer
       */
       
        cantBeIntegerObject(objectPointer);
        return wordMemory.segment_word(ObjectTableSegment,
            ObjectTableStart + objectPointer);
    }
    
    // freeBitOf:put:
    inline int freeBitOf_put(int objectPointer, int value)
    {
       /* "source"
        ^self ot: objectPointer bits: 10 to: 10 put: value
       */
       
        return ot_bits_to_put(objectPointer, 10, 10, value);
    }
    
    // classBitsOf:put:
    inline int classBitsOf_put(int objectPointer, int value)
    {
       /* "source"
        ^self heapChunkOf: objectPointer word: 1 put: value
       */
       
        return heapChunkOf_word_put(objectPointer, 1, value);
    }
    
    // heapChunkOf:byte:
    inline int heapChunkOf_byte(int objectPointer, int offset)
    {
       /* "source"
        ^wordMemory segment: (self segmentBitsOf: objectPointer)
            word: ((self locationBitsOf: objectPointer) + (offset//2))
            byte: (offset\\2)
       */

        return wordMemory.segment_word_byte(segmentBitsOf(objectPointer),
                        locationBitsOf(objectPointer) + offset/2,
                        offset % 2);
    }

    
    // locationBitsOf:put:
    inline int locationBitsOf_put(int objectPointer, int value)
    {
       /* "source"
        self cantBeIntegerObject: objectPointer.
        ^wordMemory segment: ObjectTableSegment
            word: ObjectTableStart + objectPointer + 1
            put: value
       */
        cantBeIntegerObject(objectPointer);
        return wordMemory.segment_word_put(ObjectTableSegment,
            ObjectTableStart + objectPointer + 1,
            value);
    }

    
    // sizeBitsOf:
    inline int sizeBitsOf(int objectPointer)
    {
       /* "source"
        ^self heapChunkOf: objectPointer word: 0
       */
        
        return heapChunkOf_word(objectPointer, 0);
    }

    
    // oddBitOf:put:
    inline int oddBitOf_put(int objectPointer, int value)
    {
       /* "source"
        ^self ot: objectPointer bits: 8 to: 8 put: value
       */
       return ot_bits_to_put(objectPointer, 8, 8, value);
    }
    
    // ot:put:
    inline int ot_put(int objectPointer, int value)
    {
       /* "source"
        self cantBeIntegerObject: objectPointer.
        ^wordMemory segment: ObjectTableSegment
            word: ObjectTableStart + objectPointer
            put: value
       */
       
        cantBeIntegerObject(objectPointer);
        return wordMemory.segment_word_put(ObjectTableSegment,
                ObjectTableStart + objectPointer,
                value);
    }

    // countBitsOf:put:
    inline int countBitsOf_put(int objectPointer, int value)
    {
       /* "source"
        ^self ot: objectPointer bits: 0 to: 7 put: value
       */
       
      return ot_bits_to_put(objectPointer, 0, 7, value);
    }
    
    // classBitsOf:
    inline int classBitsOf(int objectPointer)
    {
        /* "source"
         ^self heapChunkOf: objectPointer word: 1
         */
        return heapChunkOf_word(objectPointer, 1);
        
    }
    
    // countBitsOf:
    inline int countBitsOf(int objectPointer)
    {
       /* "source"
        ^self ot: objectPointer bits: 0 to: 7
       */
       
        return ot_bits_to(objectPointer, 0, 7);
    }
    
    // ot:bits:to:put:
    inline int ot_bits_to_put(
                       int objectPointer,
                       int firstBitIndex,
                       int lastBitIndex,
                       int value
                       )
    {
       /* "source"
        self cantBeIntegerObject: objectPointer.
        ^wordMemory segment: ObjectTableSegment
            word: ObjectTableStart + objectPointer
            bits: firstBitIndex
            to: lastBitIndex
            put: value
       */
       
        cantBeIntegerObject(objectPointer);
        return wordMemory.segment_word_bits_to_put(ObjectTableSegment,
                    ObjectTableStart + objectPointer,
                    firstBitIndex,
                    lastBitIndex,
                    value);
    }
    
    // sizeBitsOf:put:
    inline int sizeBitsOf_put(int objectPointer, int value)
    {
        /* "source"
         ^self heapChunkOf: objectPointer word: 0 put: value
         */
        
        return heapChunkOf_word_put(objectPointer, 0, value);
    }
    
    // ot:bits:to:
    inline int ot_bits_to(int objectPointer, int firstBitIndex, int lastBitIndex)
    {
       /* "source"
        self cantBeIntegerObject: objectPointer.
        ^wordMemory segment: ObjectTableSegment
            word: ObjectTableStart + objectPointer
            bits: firstBitIndex
            to: lastBitIndex
       */
       
        cantBeIntegerObject(objectPointer);
        return wordMemory.segment_word_bits_to(ObjectTableSegment,
                 ObjectTableStart + objectPointer,
                 firstBitIndex, lastBitIndex);

    }
    
    // pointerBitOf:
    inline int pointerBitOf(int objectPointer)
    {
       /* "source"
        ^self ot: objectPointer bits: 9 to: 9
       */
       
        return ot_bits_to(objectPointer, 9, 9);
    }

    
    // --- Allocation ---
    
    // obtainPointer:location:
    int obtainPointer_location(int size, int location);
    
    // attemptToAllocateChunk:
    int attemptToAllocateChunk(int size);
    
    // attemptToAllocateChunkInCurrentSegment:
    int attemptToAllocateChunkInCurrentSegment(int size);
    
    void outOfMemoryError();
    
    int auditFreeOops();
    
#ifdef RUNTIME_CHECKING
    inline void runtime_check(bool condition, const char *errorMessage)
    {
        if (!condition)
        {
            assert(0);
            hal->error(errorMessage);
        }
    }
#endif
    
private:
    RealWordMemory wordMemory;
    // Special Register G&R pg. 667
    int currentSegment; // The index of the heap segment currently being used for allocation

    int freeWords; // free words remaining (make primitiveFreeCore "fast")
    
    // An a table entry with a free bit set OR that contains a reference to a free chunk
    // (free bit clear but count field zero) of memory is counted as a free oop
    int freeOops;  // free OT entries (make primitiveFreeOops "fast")
    
    
    // G&R pg. 664 - Object Table Related Constants
    // Object Table Segment (last segment) contains the Object Table followed by the
    // head of the OT free pointer list
    // +-------------------------+
    // |                         | <--- ObjectTableStart
    // |                         |
    // |                         |
    // |      Object Table       |
    // |                         |
    // |                         |
    // +-------------------------+
    // |     FreePointerList     |
    // +-------------------------+
    // |////// UNUSED WORD //////|
    // +-------------------------+
    //
    
    static const int ObjectTableSegment = RealWordMemory::SegmentCount-1;
    static const int ObjectTableStart = 0;
    static const int ObjectTableSize = RealWordMemory::SegmentSize - 2;
    // The smallest number that is too large to represent in an eight-bit count field; that is, 256.
    static const int HugeSize = 256; // G&R pg 661
    
    // The location of the head of the linked list of free object table entries
    static const int FreePointerList = ObjectTableStart + ObjectTableSize; // G&R pg. 664

    // G&R pg. 664 - Object Table Related Constants
    // The smallest size of chunk that is not stored on a list whose chunk share the same size.
    // (Theindex of the last free chunk list).
    static const int BigSize = 20;
    static const int FirstFreeChunkListSize = BigSize+1;

    
    // Heap Constants G&R pg. 658
    
    // The number of heaps segments used in the implementation.
    // We reserve the last segment for the Object Table and use the remaining for the heap
    static const int HeapSegmentCount = RealWordMemory::SegmentCount - 1;
    
    // Each heap segment is organized as follows:
    //
    // +-------------------------+
    // |                         |
    // |                         |
    // |     Object Storage      |
    // |                         |
    // |                         |<--- HeapSpaceStop (last word)
    // +-------------------------+
    // |   Array of BigSize+1    |<--- FirstFreeChunkList
    // |   Free Chunks Linked    |
    // |   List Heads            |
    // |                         |<--- LastFreeChunkList
    // +-------------------------+
    //
    
    // The index of the first memory segmentused to store the heap
    static const int FirstHeapSegment = 0;
    static const int LastHeapSegment = FirstHeapSegment + HeapSegmentCount - 1;
    
    // The address of the last location used in each heap segment.
    static const int HeapSpaceStop = RealWordMemory::SegmentSize - FirstFreeChunkListSize - 1;
    static const int HeaderSize = 2; // The number of words in an object header(2).
    // If HeaderSize changes, revisit forAllOtherObjectsAccessibleFrom_suchThat_do
    // where we test if the offset passes the class field...
    
    // The location of the head of the linked list of free chunks of size zero. Comes right
    // after the last word for object storage.
    static const int FirstFreeChunkList = HeapSpaceStop + 1;
    
    // The bluebook incorrectly uses LastFreeChunkList in all places it is used! The
    // headOfFreeChunkList:inSegment: and headOfFreeChunkList:inSegment:put methods take
    // a SIZE as the first parameter not a location.
    // The location of the head of the linked list of free chunks of size BigSize or larger.
    // static const int LastFreeChunkList =  FirstFreeChunkList + BigSize;
    
    // Any sixteen-bit value that cannot be an object table index, e.g.,2**16~1.
    static const int NonPointer = 65535;
    // Last special oop
    // (See SystemTracer in Smalltalk.sources)
    static const int LastSpecialOop = 52;

    
    // Snapshots
    // Object space starts at offset 512 in the image
    static const int ObjectSpaceBaseInImage = 512;

    bool loadObjectTable( IFileSystem *fileSystem, int fd);
    static bool padToPage(IFileSystem *fileSystem, int fd);
    bool loadObjects(IFileSystem *fileSystem, int fd);
    bool saveObjects(IFileSystem *fileSystem, int fd);

#ifdef GC_MARK_SWEEP
    IGCNotification *gcNotification;
#endif
    
    // Interface to the host operating system
    IHardwareAbstractionLayer *hal;
};

