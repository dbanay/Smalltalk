//
//  objmemory.cpp
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


#include <cstdint>
#include <algorithm>
#include "objmemory.h"
#include "oops.h"

#ifndef GC_REF_COUNT
#ifndef GC_MARK_SWEEP
#error "must define GC_REF_COUNT and/or GC_MARK_SWEEP"
#endif
#endif


ObjectMemory::ObjectMemory(
                           IHardwareAbstractionLayer *halInterface
#ifdef GC_MARK_SWEEP
    , IGCNotification *notification
#endif
) : currentSegment(-1), freeWords(0), freeOops(0)
{
#ifdef GC_MARK_SWEEP
    gcNotification = notification;
#endif
    hal = halInterface;
}



bool ObjectMemory::loadObjectTable(IFileSystem *fileSystem, int fd)
{
    // First two 32-bit values have the object space length and object table lengths in words
    std::int32_t objectTableLength;
    
    if (fileSystem->seek_to(fd, 4) == -1) // Skip over object space length
        return false;
    if (fileSystem->read(fd, (char *)&objectTableLength, sizeof(objectTableLength)) != sizeof(objectTableLength))
        return false;
    int fileSize = fileSystem->file_size(fd);
    
    if (fileSystem->seek_to(fd, fileSize - objectTableLength*2) == -1) // Reposition to start of object table
        return false;

    for(int objectPointer = 0; objectPointer < objectTableLength; objectPointer+=2)
    {
        std::uint16_t words[2];
        if (fileSystem->read(fd, (char *)&words, sizeof(words)) != sizeof(words))
            return false;
        ot_put(objectPointer, words[0]);
        locationBitsOf_put(objectPointer, words[1]);
    }
     
    headOfFreePointerListPut(NonPointer);
    
    // Initialize the remaining entries as free
    for(int objectPointer = objectTableLength; objectPointer < ObjectTableSize; objectPointer += 2)
    {
        ot_put(objectPointer, 0);
        freeBitOf_put(objectPointer, 1);
        locationBitsOf_put(objectPointer, 0);
    }
    
    // Build the OT entry free list, Go backwards so we have lower entries first on free list.
    // Why? Makes the OT easier to see in debugger.
    // Note we skip oop 0, which is considered reserved and invalid. (page 2, Xerox Virtual Image booklet)
    for(int objectPointer = ObjectTableSize-2; objectPointer >= 2; objectPointer -= 2)
        if (freeBitOf(objectPointer))
            toFreePointerListAdd(objectPointer);
    
    freeOops = auditFreeOops();
    
    return true;
}

bool ObjectMemory::loadObjects(IFileSystem *fileSystem, int fd)
{
    static const int SegmentHeapSpaceSize = HeapSpaceStop + 1;
    
    // Track amount of free space available for objects in each segment
    int heapSpaceRemaining[HeapSegmentCount];
    
    for(int segment = FirstHeapSegment; segment <= LastHeapSegment; segment++)
        heapSpaceRemaining[segment - FirstHeapSegment] = SegmentHeapSpaceSize;
    
    // Load objects from the virtual image into the heap segments
    // being careful to not split an object across a segment boundary
    int destinationSegment = FirstHeapSegment, destinationWord = 0;
    
    for(int objectPointer = 2; objectPointer < ObjectTableSize; objectPointer += 2)
    {
        if (freeBitOf(objectPointer)) continue;
        // A free chunk has it's COUNT field set to zero but the free bit is clear
        assert (countBitsOf(objectPointer) != 0); // SANITY Make sure a freeChunk wasn't saved!
        
        // On disk objects are stored contiguously as if a large 20-bit WORD addressed space
        // In this scheme, the OT segment and locations combine to form a WORD address
        const int objectImageWordAddress = (segmentBitsOf(objectPointer) << 16)
        + locationBitsOf(objectPointer);
        
        fileSystem->seek_to(fd, ObjectSpaceBaseInImage + objectImageWordAddress * sizeof(std::uint16_t));
        
        std::uint16_t objectSize;
        fileSystem->read(fd, (char *) &objectSize, sizeof(objectSize));
        
        
        // Account for the extra word used by HugeSize objects
        int extraSpace = objectSize < HugeSize || pointerBitOf(objectPointer) == 0 ? 0 : 1;
        int space = objectSize + extraSpace; // space in memory
        
        if (space > heapSpaceRemaining[destinationSegment - FirstHeapSegment])
        {
            // No room left in the current segment, move to next
            destinationSegment++;
            if (destinationSegment == HeapSegmentCount) return false; // Full
            destinationWord = 0;
        }
        
        // Update OT entry so that it references the object location in ObjectMemory vs the disk image
        segmentBitsOf_put(objectPointer, destinationSegment);
        locationBitsOf_put(objectPointer, destinationWord);
        
        // Store the object in the image into word memory
        // First comes the size...
        
        sizeBitsOf_put(objectPointer, objectSize );
        
        // Next is the class...
        std::uint16_t classBits;
        fileSystem->read(fd,(char *) &classBits, sizeof(classBits));
        
        classBitsOf_put(objectPointer, classBits);
        
        // Followed by the fields...
        for(int wordIndex = 0; wordIndex < objectSize-HeaderSize; wordIndex++)
        {
            std::uint16_t word;
            fileSystem->read(fd,(char *) &word, sizeof(word));
            // use heap chunk
            storeWord_ofObject_withValue(wordIndex, objectPointer, word);
        }
        
        destinationWord += space;
        
        heapSpaceRemaining[destinationSegment - FirstHeapSegment] -= space;
    }
    
    // Initialize the free chunk lists for each heap segment with the sentinel
    for(int segment = FirstHeapSegment; segment <= LastHeapSegment; segment++)
    {
        for(int size = HeaderSize; size <= BigSize; size++)
            resetFreeChunkList_inSegment(size, segment);
    }
    
    freeWords = 0;
    // Place any remaining space in each segment onto it's free chunk list, which is
    // is a linked list of object pointers.
    // The chunks are linked using the class field of an object. The size field of
    // the object contains the actual size of the free chunk.
    for(int segment = FirstHeapSegment; segment <= LastHeapSegment; segment++)
    {
        int freeChunkSize = heapSpaceRemaining[segment - FirstHeapSegment];
        freeWords += freeChunkSize;
        if (freeChunkSize >= HeaderSize)
        {
            int freeChunkLocation = SegmentHeapSpaceSize - freeChunkSize;
            // G&R pg 665 - each free chunk has an OT entry
            currentSegment = segment; // Set special segment register
            int objectPointer = obtainPointer_location(freeChunkSize, freeChunkLocation);
            toFreeChunkList_add(std::min(freeChunkSize, (int) BigSize), objectPointer);
        }
    }
    
    currentSegment = FirstHeapSegment;
    
    return true;
    
}

bool ObjectMemory::loadSnapshot(IFileSystem *fileSystem, const char *fileName)
{
    int fd = fileSystem->open_file(fileName);
    if (fd == -1)
        return false;
     
    bool succeeded = loadObjectTable(fileSystem, fd) && loadObjects(fileSystem, fd);
    
    fileSystem->close_file(fd);
    
    return succeeded;
}


bool ObjectMemory::padToPage(IFileSystem *fileSystem, int fd)
{
    int pos = fileSystem->tell(fd);
    int desired = ((pos + 512 - 1) / 512) * 512;
    std::uint16_t word = 0;
    int pad = (desired - pos) / sizeof(word);
    while (pad-- > 0)
    {
        if (fileSystem->write(fd, (char *) &word, sizeof(word)) != sizeof(word))
            return false;
    }
    
    return true;
 }

bool ObjectMemory::saveSnapshot(IFileSystem *fileSystem, const char *imageFileName)
{
    
    int fd = fileSystem->create_file(imageFileName);
    if (fd == -1)
        return false;
    bool success = saveObjects(fileSystem, fd);
    
    fileSystem->close_file(fd);
    return success;
    
}

bool ObjectMemory::saveObjects(IFileSystem *fileSystem, int fd)
{
    // Avoid dumping out the entire object table -- we only need to write entries up until
    // the last OT entry that references an object
    int lastUsedObjectPointer = NonPointer;
    for(int objectPointer = 2; objectPointer < ObjectTableSize; objectPointer += 2)
    {
         if (hasObject(objectPointer))
            lastUsedObjectPointer = objectPointer;
    }
    
    int storedObjectTableLength = lastUsedObjectPointer + 2;
    
    std::int32_t placeHolder[2] = {0};
    
    // Write place holder value for object space length and object table length
    if (fileSystem->write(fd, (char *) &placeHolder, sizeof(placeHolder)) != sizeof(placeHolder))
        return false;

    // Write two zero bytes indicating interchange format
    std::uint8_t interchange[2] = {0};
    if (fileSystem->write(fd, (char *) &interchange, sizeof(interchange)) !=  sizeof(interchange))
        return false;

    if (!padToPage(fileSystem, fd)) // Advance to next page before writing objects
        return false;
        
    // Write objects
    std::int32_t objectSpaceLength = 0;
    for(int objectPointer = 2; objectPointer < storedObjectTableLength; objectPointer += 2)
    {
        if (!hasObject(objectPointer)) continue;
 
        // Write object to file... N.B. we do not store the extra word for HugeSize objects
        std::uint16_t header[2];
        std::uint16_t objectSize = sizeBitsOf(objectPointer);
        header[0] = objectSize;
        header[1] = (std::uint16_t) fetchClassOf(objectPointer);
        if (fileSystem->write(fd, (char *) &header, sizeof(header)) != sizeof(header))
            return false;
        int wordLengthOfObject = fetchWordLengthOf(objectPointer);
        for(int wordIndex = 0; wordIndex < wordLengthOfObject; wordIndex++ )
        {
            std::uint16_t word = (std::uint16_t) fetchWord_ofObject(wordIndex, objectPointer);
            if (fileSystem->write(fd, (char *) &word, sizeof(word)) != sizeof(word))
                return false;
        }
        
        objectSpaceLength += objectSize;
    }
    

   if (!padToPage(fileSystem, fd)) // Advance to next page before writing object table
       return false;

    // Write object table
    int objectImageWordAddress = 0;
    for(int objectPointer = 0; objectPointer < storedObjectTableLength; objectPointer += 2)
    {
        std::uint16_t oldOTValue = ot(objectPointer);
        std::uint16_t oldOTLocation = locationBitsOf(objectPointer);
        
        if (objectPointer >= 2)
        {
            if (!freeBitOf(objectPointer) && countBitsOf(objectPointer) == 0)
            {
                // This entry was for a free chunk of memory, but we don't save
                // free space in the image. Store as a
                // an available OT entry by setting free bit
                freeBitOf_put(objectPointer, 1);
            }
            
            if (freeBitOf(objectPointer))
            {
                // manual.pdf - page 3: free entries have freeBit set and other bits in both
                // words are 0.
                ot_put(objectPointer, 0);
                freeBitOf_put(objectPointer, 1);
                locationBitsOf_put(objectPointer, 0);
            }
            else
            {
                std::uint16_t objectSize = (std::uint16_t) sizeBitsOf(objectPointer);
            
                // Modify the location of the object table entry... we do this once we no longer
                // process this object table entry
                segmentBitsOf_put(objectPointer, objectImageWordAddress >> 16);
                locationBitsOf_put(objectPointer, objectImageWordAddress & 0xffff);
                objectImageWordAddress += objectSize;
            }
        }
        
        // Assemble object table entry
        std::uint16_t words[2];
        words[0] = ot(objectPointer);
        words[1] = locationBitsOf(objectPointer);
   
        // Restore OT entry
        ot_put(objectPointer, oldOTValue);
        locationBitsOf_put(objectPointer, oldOTLocation);
        
        // Write this entry
        if (fileSystem->write(fd, (char *) &words, sizeof(words)) != sizeof(words))
            return false;

    }
    
    // Now we can go back fill in the values for the image header.
    fileSystem->seek_to(fd, 0);
    fileSystem->write(fd, (char *) &objectSpaceLength, sizeof(objectSpaceLength));
    fileSystem->write(fd, (char *) &storedObjectTableLength, sizeof(storedObjectTableLength));
    
    return true;
}

// sweepCurrentSegmentFrom:
int ObjectMemory::sweepCurrentSegmentFrom(int lowWaterMark)
{
   int si;
   int di;
   int objectPointer;
   int size;


   /* "source"
    "ERROR dbanay - unused local 'space'"
   	si <- di <- lowWaterMark.
   	[si < HeapSpaceStop]
   		whileTrue: "for each object, si"
   			[(wordMemory segment: currentSegment word: si + 1) = NonPointer
   				ifTrue: "unallocated, so skip it"
   					[size <- wordMemory segment: currentSegment word: si.
   					si <- si + size]
   				ifFalse: "allocated, so keep it, but move it to compact storage"
   					[objectPointer
   						<- wordMemory segment: currentSegment word: si.
   					size <- self locationBitsOf: objectPointer.
   							"the reversed size"
   					self locationBitsOf: objectPointer
   						put: di. "point object table at new location"
   					self sizeBitsOf: objectPointer
   						put: size. "restore the size to its proper place"
   					si <- si + 1. "skip the size"
   					di <- di + 1. "skip the size"
   					2 to: (self spaceOccupiedBy: objectPointer) do:
   						"move the rest of the object"
   						[ :i |
   							wordMemory segment: currentSegment
   								word: di
   								put: (wordMemory segment:
   										currentSegment
   										word: si).
   							si <- si + 1.
   							di <- di + 1]]].
   	^di
   */
    
    si = di = lowWaterMark;
    while (si < HeapSpaceStop)  // for each object, si
    {
        if (wordMemory.segment_word(currentSegment, si + 1) == NonPointer)
        {
            // Unallocated, so skip it (see abandonFreeChunksInSegment)
            size = wordMemory.segment_word(currentSegment, si);
            si = si + size;
        }
        else
        {
            // allocated, so keep it, but move it to compact storage
            objectPointer = wordMemory.segment_word(currentSegment, si);  // reversed pointer!
            size = locationBitsOf(objectPointer);   // the reversed size (stored during pointer reversal step)
            locationBitsOf_put(objectPointer, di);  // point object table at new location (di)
            sizeBitsOf_put(objectPointer, size);    // restore the size to its proper place
            si++;  // skip the size header
            di++;  // skip the size header (size we wrote to di via sizeBitsOf_put)
            int limit = spaceOccupiedBy(objectPointer);
            for(int i = 2; i <= limit; i++) // move the rest of the object (already wrote size via sizeBitsOf_put)
            {
                wordMemory.segment_word_put(currentSegment, di,
                                            wordMemory.segment_word(currentSegment, si));
                si++;
                di++;
                                            
            }
        }
    }
    
    return di;
    
}


// compactCurrentSegment
void ObjectMemory::compactCurrentSegment()
{
   int lowWaterMark;
   int bigSpace;

   /* "source"
   	lowWaterMark <- self abandonFreeChunksInSegment: currentSegment.
   	lowWaterMark < HeapSpaceStop
   		ifTrue: [self reverseHeapPointersAbove: lowWaterMark.
   			bigSpace <- self sweepCurrentSegmentFrom: lowWaterMark.
   			self deallocate: (self obtainPointer:
   							(HeapSpaceStop + 1 - bigSpace)
   						location: bigSpace)]
   */
    
    RUNTIME_CHECK(currentSegment >= FirstHeapSegment && currentSegment <= LastHeapSegment);

    lowWaterMark = abandonFreeChunksInSegment(currentSegment);
    if (lowWaterMark < HeapSpaceStop)
    {
        reverseHeapPointersAbove(lowWaterMark);
        bigSpace = sweepCurrentSegmentFrom(lowWaterMark);
        deallocate(obtainPointer_location(HeapSpaceStop + 1 - bigSpace, bigSpace));
    }
}

// Force a garbage collection
void ObjectMemory::garbageCollect()
{
#ifdef GC_MARK_SWEEP
    reclaimInaccessibleObjects();
#endif
}


// releasePointer:
void ObjectMemory::releasePointer(int objectPointer)
{
   /* "source"
   	self freeBitOf: objectPointer put: 1.
   	self toFreePointerListAdd: objectPointer
   */
    freeBitOf_put(objectPointer, 1);
    toFreePointerListAdd(objectPointer);
}


// reverseHeapPointersAbove:
void ObjectMemory::reverseHeapPointersAbove(int lowWaterMark)
{
   int size;

   /* "source"
   	0 to: ObjectTableSize-2 by: 2 do:
   		[ :objectPointer |
   			(self freeBitOf: objectPointer) = 0
   				ifTrue: "the Object Table entry is in use"
   					[(self segmentBitsOf: objectPointer) = currentSegment
   						ifTrue: "the object is in this segment"
   							[(self locationBitsOf: objectPointer) < lowWaterMark
   								ifFalse: "the object will be swept"
   									[size <- self sizeBitsOf: objectPointer.
   											"rescue the size"
   									self sizeBitsOf: objectPointer
   										put: objectPointer.
   										"reverse the pointer"
   									self locationBitsOf: objectPointer
   										put: size "save the size"]]]]
   */

    /* pg. 673 G&R */
    for(int objectPointer = 0; objectPointer <= ObjectTableSize-2; objectPointer += 2)
    {
        if (freeBitOf(objectPointer) == 0)  // the Object Table entry is in use
        {
           // the object is in this segment
            if (segmentBitsOf(objectPointer) == currentSegment)
            {
                // the object will be swept
                if (locationBitsOf(objectPointer) >= lowWaterMark)
                {
                    size = sizeBitsOf(objectPointer); // rescue the size
                    sizeBitsOf_put(objectPointer, objectPointer); // reverse pointer
                    locationBitsOf_put(objectPointer, size); // save the size
                }
            }
        }
    }
}


// abandonFreeChunksInSegment:
int ObjectMemory::abandonFreeChunksInSegment(int segment)
{
   int lowWaterMark; // Location in the segment of the first free chunk
   int objectPointer;
   int nextPointer;

   /* "source"
   	lowWaterMark <- HeapSpaceStop. "first assume that no chunk is free"
   	HeaderSize to: BigSize do: "for each free-chunk list"
   		[ :size |
   			objectPointer <- self headOfFreeChunkList: size
   						inSegment: segment.
   			[objectPointer = NonPointer] whileFalse:
   				[lowWaterMark <- lowWaterMark min:
   							(self locationBitsOf: objectPointer).
   				nextPointer <- self classBitsOf: objectPointer.
   							"link to next free chunk"
   				self classBitsOf: objectPointer put: NonPointer.
   							"distinguish for sweep"
   				self releasePointer: objectPointer.
   							"add entry to free-pointer list"
   				objectPointer <- nextPointer].
   			self resetFreeChunkList: size inSegment: segment].
   	^lowWaterMark
   */
    
    // This computes the low water mark and sets the class field of free chunks to NonPointer
    // so that are identified as such and can be combined into one free block after compaction
    lowWaterMark = HeapSpaceStop; // first assume that no chunk is free
    for(int size = HeaderSize; size <= BigSize; size++)
    {
        objectPointer = headOfFreeChunkList_inSegment(size, segment);
        while (objectPointer != NonPointer)
        {
            lowWaterMark = std::min(lowWaterMark, locationBitsOf(objectPointer));
            nextPointer = classBitsOf(objectPointer); // link to next free chunk
            classBitsOf_put(objectPointer, NonPointer); // distinguish for sweep
            releasePointer(objectPointer); // Add entry to free (object table) pointer list
            objectPointer = nextPointer;
        }
        resetFreeChunkList_inSegment(size, segment);
    }
    return lowWaterMark;
}


#ifdef GC_MARK_SWEEP

// reclaimInaccessibleObjects
void ObjectMemory::reclaimInaccessibleObjects()
{
    
   /* "source"
   	self zeroReferenceCounts.
   	self markAccessibleObjects.
   	self rectifyCountsAndDeallocateGarbage
   */

    zeroReferenceCounts();
    markAccessibleObjects();
    rectifyCountsAndDeallocateGarbage();
}


// markObjectsAccessibleFrom:
int ObjectMemory::markObjectsAccessibleFrom(int rootObjectPointer)
{
   /* "source"
   	^self forAllObjectsAccessibleFrom: rootObjectPointer
   		suchThat: "the predicate tests for an unmarked object and marks it"
   			[ :objectPointer |
   				unmarked <- (self countBitsOf: objectPointer) = 0.
   				unmarked ifTrue: [self countBitsOf: objectPointer put: 1].
   				unmarked]
   		do: "the action restores the mark to count=1"
   			[ :objectPointer |
   				self countBitsOf: objectPointer put: 1]
   */
    return forAllObjectsAccessibleFrom_suchThat_do(rootObjectPointer,
       [this](int objectPointer) { // the predicate tests for an unmarked object and marks it
            bool unmarked = countBitsOf(objectPointer) == 0;
            if (unmarked)
            {
                countBitsOf_put(objectPointer, 1);
            }
            return unmarked;
       },
       [this](int objectPointer) { // the action restores the mark to count=1
            countBitsOf_put(objectPointer, 1);
       });

 }


// markAccessibleObjects
void ObjectMemory::markAccessibleObjects()
{
   /* "source"
   	"ERROR: rootObjectPointers not defined"
   	rootObjectPointers do:
   		[ :rootObjectPointer |
   			self markObjectsAccessibleFrom: rootObjectPointer]
   */
 
    for(int i = 0; i <= LastSpecialOop; i += 2)
    {
        addRoot(i);
    }

    if (gcNotification)
        gcNotification->prepareForCollection();
    
}
#endif


void ObjectMemory::outOfMemoryError()
{
    assert(0);
    hal->error("Out of memory");
}

// allocateChunk:
int ObjectMemory::allocateChunk(int size)
{
   int objectPointer;
  
   /* "source"
    "marking collector"
   	objectPointer <- self attemptToAllocateChunk: size.
   	objectPointer isNil ifFalse: [^objectPointer].
   	self reclaimInaccessibleObjects. "garbage collect and try again"
   	objectPointer <- self attemptToAllocateChunk: size.
   	objectPointer isNil ifFalse: [^objectPointer].
   	self outOfMemoryError "give up"
   */
    
    objectPointer = attemptToAllocateChunk(size);
#ifdef GC_MARK_SWEEP
    if (objectPointer == NilPointer)
    {
        reclaimInaccessibleObjects();
        objectPointer = attemptToAllocateChunk(size);
    }
#endif
    if (objectPointer != NilPointer)
    {
        if (freeWords >= size)
            freeWords -= size;

        return objectPointer;
    }
    outOfMemoryError(); // give up
    return NilPointer;
}

#ifdef GC_MARK_SWEEP

// rectifyCountsAndDeallocateGarbage
void ObjectMemory::rectifyCountsAndDeallocateGarbage()
{
   int count;

   /* "source"
   	"reset heads of free-chunk lists"
   	FirstHeapSegment to: LastHeapSegment do: "for every segment"
   		[ :segment |
   			HeaderSize to: BigSize do: "for every free chunk list"
   				[ :size | "reset the list head"
   					self resetFreeChunkList: size inSegment: segment]].
   	"rectify counts, and deallocate garbage"
   	0 to: ObjectTableSize-2 by: 2 do: "for every object table entry"
   		[ :objectPointer |
   			(self freeBitOf: objectPointer) = 0
   				ifTrue: "if it is not a free entry"
   					[(count <- self countBitsOf: objectPointer) = 0
   						ifTrue: "it is unmarked so deallocate it"
   							[self deallocate: objectPointer]
   						ifFalse: "it is marked so rectify reference counts"
   							[count < 128
   								ifTrue: "subtract 1 to compensate for the mark"
   									[self countBitsOf: objectPointer put: count-1].
   							1 to: (self lastPointerOf: objectPointer)-1 do:
   								[ :offset | "increment the reference count
   										of each pointer"
   									self countUp:
   										(self heapChunkOf: objectPointer
   											word: offset)]]]].
   	"be sure the root objects don't disappear"
   	"ERROR: rootObjectPointers not defined"
   	rootObjectPointers do:
   		[ :rootObjectPointer | self countUp: rootObjectPointer].
   	self countBitsOf: NilPointer put: 128
   */
    // reset heads of free-chunk lists
    for(int segment = FirstHeapSegment; segment <= LastHeapSegment; segment++)
    {
        // for every free chunk list
        for(int size = HeaderSize; size <= BigSize; size++)
        {
            // reset the list head
            resetFreeChunkList_inSegment(size, segment);
        }
    }
    
    // rectify counts, and deallocate garbage
    for(int objectPointer = 0; objectPointer <= ObjectTableSize-2; objectPointer += 2)
    {
        if (freeBitOf(objectPointer) == 0) // if it is not a free entry
        {
            count = countBitsOf(objectPointer);
            if (count == 0)
            {
                // unmarked, so deallocate it
                freeWords += spaceOccupiedBy(objectPointer); //dbanay
                deallocate(objectPointer);
            }
            else
            {
                // it is marked so rectify reference counts
                if (count < 128)
                {
                    // subtract 1 to compensate for the mark
                    countBitsOf_put(objectPointer, count - 1);
                }

                int limit = lastPointerOf(objectPointer)-1;
                // increment the reference count of each pointer
                // NB start at offset 1, which is the class
                for(int offset = 1; offset <= limit; offset++)
                {
                    countUp(heapChunkOf_word(objectPointer, offset));
                }
            }
        }
    }
      
    countBitsOf_put(NilPointer, 128);
    
    freeOops = auditFreeOops();

    if (gcNotification) gcNotification->collectionCompleted();
    

}


// zeroReferenceCounts
void ObjectMemory::zeroReferenceCounts()
{
   /* "source"
   	0 to: ObjectTableSize-2 by: 2 do:
   		[ :objectPointer |
   			self countBitsOf: objectPointer put: 0]
   */

    for(int objectPointer = 0; objectPointer <= ObjectTableSize-2; objectPointer += 2)
    {
        countBitsOf_put(objectPointer, 0);
    }
}
#endif

// lastPointerOf:
// This returns the size of object up to the last pointer in it
int ObjectMemory::lastPointerOf(int objectPointer)
{
   // MethodClass is the object table index of CompiledMethod.
   static const int MethodClass = ClassCompiledMethod;
   int methodHeader;

   /* "source"
    "ERROR - dbanay this is WRONG."
   	(self pointerBitOf: objectPointer) = 0
   		ifTrue: [^HeaderSize]
   		"ERROR: MethodClass not defined"
   		ifFalse: [(self classBitsOf: objectPointer) = MethodClass
   				ifTrue: [methodHeader <- self heapChunkOf: objectPointer
   								word: HeaderSize.
   					^HeaderSize + 1 + ((methodHeader bitAnd: 126)
   								bitShift: -1)]
   				ifFalse: [^self sizeBitsOf: objectPointer]]
   */
    
    /* "source"
       "Fixed in July 1985 Ed of G&R"
     (self pointerBitOf: objectPointer) = 0
         ifTrue: [(self classBitsOf: objectPointer) = MethodClass
                 ifTrue: [methodHeader <- self heapChunkOf: objectPointer
                            word: HeaderSize.
                         ^HeaderSize + 1 + ((methodHeader bitAnd: 126) bitShift: -1)]
                 ifFalse: [^HeaderSize]]
         ifFalse:
             [^self sizeBitsOf: objectPointer]

     */
    
    if (pointerBitOf(objectPointer) == 0)
    { // Not pointer object
        // CompiledMethods are special in that they are marked as having no pointers but actually do
        if (classBitsOf(objectPointer) == MethodClass)
        {
            methodHeader = heapChunkOf_word(objectPointer, HeaderSize);
            // Header Size + Method Header Size + Literal Count
            return HeaderSize + 1 + ((methodHeader & 126) >> 1);
        }
        else
            return HeaderSize; // Class field is last pointer in the object
    }
    
    return sizeBitsOf(objectPointer);
  
}


// spaceOccupiedBy:
int ObjectMemory::spaceOccupiedBy(int objectPointer)
{
   int size;

   /* "source"
   	size <- self sizeBitsOf: objectPointer.
   	(size < HugeSize or: [(self pointerBitOf: objectPointer) = 0])
   		ifTrue: [^size]
   		ifFalse: [^size + 1]
   */
    size = sizeBitsOf(objectPointer);
    if (size < HugeSize || pointerBitOf(objectPointer) == 0)
        return size;
    return size+1; // Account for extra word used for traversal algorithm (G&R pg. 679)
}


// allocate:odd:pointer:extra:class:
int ObjectMemory::allocate_odd_pointer_extra_class(
      int size,
      int oddBit,
      int pointerBit,
      int extraWord,
      int classPointer
   )
{
   int objectPointer;
   int defaultValue;

   /* "source"
   	self countUp: classPointer.
   	objectPointer <- self allocateChunk: size + extraWord.
   	self oddBitOf: objectPointer put: oddBit.
   	self pointerBitOf: objectPointer put: pointerBit.
   	self classBitsOf: objectPointer put: classPointer.
   	default <- pointerBit=0 ifTrue: [0] ifFalse: [NilPointer].
   	HeaderSize to: size-1 do:
   		[ :i | self heapChunkOf: objectPointer word: i put: default].
   	self sizeBitsOf: objectPointer put: size.
   	^objectPointer
   */
    countUp(classPointer);
    objectPointer = allocateChunk(size + extraWord);
    oddBitOf_put(objectPointer, oddBit);
    pointerBitOf_put(objectPointer, pointerBit);
    classBitsOf_put(objectPointer, classPointer);
    defaultValue = (pointerBit == 0) ? 0 : NilPointer;
    for(int i = HeaderSize; i <= size-1; i++)
    {
        heapChunkOf_word_put(objectPointer, i, defaultValue);
    }
    sizeBitsOf_put(objectPointer, size);
    freeOops--; // dbanay
    return objectPointer;
}

// headOfFreePointerList
int ObjectMemory::headOfFreePointerList()
{
    /* "source"
     ^wordMemory segment: ObjectTableSegment
     word: FreePointerList
     */
    
    return wordMemory.segment_word(ObjectTableSegment,
                                   FreePointerList);
}

// toFreeChunkList:add:
void ObjectMemory::toFreeChunkList_add(int size, int objectPointer)
{
   int segment;

   /* "source"
   	segment <- self segmentBitsOf: objectPointer.
   	self classBitsOf: objectPointer
   		put: (self headOfFreeChunkList: size inSegment: segment).
   	self headOfFreeChunkList: size
   		inSegment: segment
   		put: objectPointer
   */
    
    segment = segmentBitsOf(objectPointer);
    classBitsOf_put(objectPointer,
            headOfFreeChunkList_inSegment(size, segment));
    headOfFreeChunkList_inSegment_put(size, segment, objectPointer);

}


// headOfFreeChunkList:inSegment:put:
int ObjectMemory::headOfFreeChunkList_inSegment_put(int size, int segment, int objectPointer)
{
   /* "source"
   	^wordMemory segment: segment
   		word: FirstFreeChunkList + size
   		put: objectPointer
   */
    
    RUNTIME_CHECK(size >= HeaderSize && size <= BigSize);
    
    return wordMemory.segment_word_put(segment,
        FirstFreeChunkList + size,
        objectPointer);
}


// removeFromFreePointerList
int ObjectMemory::removeFromFreePointerList()
{
   int objectPointer;

   /* "source"
   	objectPointer <- self headOfFreePointerList.
   	objectPointer = NonPointer ifTrue: [^nil].
   	self headOfFreePointerListPut: (self locationBitsOf: objectPointer).
   	^objectPointer
   */
    
    objectPointer = headOfFreePointerList();
    if (objectPointer == NonPointer)
        return NilPointer;
    headOfFreePointerListPut(locationBitsOf(objectPointer));
    return objectPointer;
}

// toFreePointerListAdd:
void ObjectMemory::toFreePointerListAdd(int objectPointer)
{
   /* "source"
   	self locationBitsOf: objectPointer
   		put: (self headOfFreePointerList).
   	self headOfFreePointerListPut: objectPointer
   */
    
    locationBitsOf_put(objectPointer, headOfFreePointerList());
    headOfFreePointerListPut(objectPointer);

}


// removeFromFreeChunkList:
int ObjectMemory::removeFromFreeChunkList(int size)
{
   int objectPointer;
   int secondChunk;
    

   /* "source"
   	objectPointer <- self headOfFreeChunkList: size
   				inSegment: currentSegment.
   	objectPointer = NonPointer ifTrue: [^nil].
   	secondChunk <- self classBitsOf: objectPointer.
   	self headOfFreeChunkList: size
   		inSegment: currentSegment
   		put: secondChunk.
   	^objectPointer
   */

    RUNTIME_CHECK(currentSegment >= FirstHeapSegment && currentSegment <= LastHeapSegment);
    
    objectPointer = headOfFreeChunkList_inSegment(size, currentSegment);
    if (objectPointer == NonPointer) return NilPointer;
    secondChunk = classBitsOf(objectPointer);
    headOfFreeChunkList_inSegment_put(size,
            currentSegment,
            secondChunk);
    return objectPointer;

}


// resetFreeChunkList:inSegment:
void ObjectMemory::resetFreeChunkList_inSegment(int size, int segment)
{
   /* "source"
   	self headOfFreeChunkList: size
   		inSegment: segment
   		put: NonPointer
   */
    RUNTIME_CHECK(size >= 2 && size <= BigSize);
    headOfFreeChunkList_inSegment_put(size, segment, NonPointer);
}


// headOfFreeChunkList:inSegment:
int ObjectMemory::headOfFreeChunkList_inSegment(int size, int segment)
{
   /* "source"
   	^wordMemory segment: segment
   		word: FirstFreeChunkList + size
   */
    RUNTIME_CHECK(size >= 2 && size <= BigSize);
    return wordMemory.segment_word(segment,
            FirstFreeChunkList + size);
}


// headOfFreePointerListPut:
int ObjectMemory::headOfFreePointerListPut(int objectPointer)
{
    /* "source"
     ^wordMemory segment: ObjectTableSegment
         word: FreePointerList
         put: objectPointer
    */
    
    return wordMemory.segment_word_put(ObjectTableSegment,
                FreePointerList,
                objectPointer);
}


// countDown:
int ObjectMemory::countDown(int rootObjectPointer)
{
 
   /* "source"
   	(self isIntegerObject: rootObjectPointer)
   		ifTrue: [^rootObjectPointer]
   		ifFalse: "this is a pointer, so decrement its reference count"
   			[^self forAllObjectsAccessibleFrom: rootObjectPointer
   				suchThat:
   					"the predicate decrements the count and tests for zero"
   					[ :objectPointer |
   						count <- (self countBitsOf: objectPointer) - 1.
   						count < 127
   							ifTrue: [self countBitsOf: objectPointer
   									put: count].
   						count = 0]
   				do: "the action zeroes the count and deallocates the object"
   					[ :objectPointer |
   						self countBitsOf: objectPointer put: 0.
   						self deallocate: objectPointer]]
   */
    
    if (isIntegerObject(rootObjectPointer))
        return rootObjectPointer;
    
    RUNTIME_CHECK(countBitsOf(rootObjectPointer)>0);
    
    // this is a pointer, so decrement its reference count
    return forAllObjectsAccessibleFrom_suchThat_do(rootObjectPointer,
       [this](int objectPointer) { // predicate
            int count = countBitsOf(objectPointer) - 1;
            RUNTIME_CHECK(count >= 0);
            if (count < 127)
                countBitsOf_put(objectPointer, count);
            return count == 0;
       },
       [this](int objectPointer) { // action
           // std::cout << "reference count zero. freeing " << objectPointer << " (" << classNameOfObject(fetchClassOf(objectPointer)) << ") free oops = " << freeOops << "\n";
            countBitsOf_put(objectPointer, 0);
            freeWords += spaceOccupiedBy(objectPointer); //dbanay
            freeOops++;
            deallocate(objectPointer);
       });

}


// countUp:
int ObjectMemory::countUp(int objectPointer)
{
   int count;

   /* "source"
   	(self isIntegerObject: objectPointer)
   		ifFalse: [count <- (self countBitsOf: objectPointer) + 1.
   			count < 129 ifTrue: [self countBitsOf: objectPointer put: count]].
   	^objectPointer
   */
    if (!isIntegerObject(objectPointer))
    {
        count = countBitsOf(objectPointer) + 1;
        if (count < 129) // Count sticks at 128
            countBitsOf_put(objectPointer, count);
    }
    
    return objectPointer;
}


// deallocate:
void ObjectMemory::deallocate(int objectPointer)
{
   int space;

   /* "source"
   	space <- self spaceOccupiedBy: objectPointer.
   	self sizeBitsOf: objectPointer put: space.
   	self toFreeChunkList: (space min: BigSize) add: objectPointer
   */
  
    space = spaceOccupiedBy(objectPointer);
    sizeBitsOf_put(objectPointer, space);
    toFreeChunkList_add(std::min(space, (int) BigSize), objectPointer);
}

#ifdef RECURSIVE_MARKING
// recursive version -- stack hungry
// forAllOtherObjectsAccessibleFrom:suchThat:do:
int ObjectMemory::forAllOtherObjectsAccessibleFrom_suchThat_do(
      int objectPointer,
      const std::function <bool (int)>& predicate,
      const std::function <void (int)>& action
    )
{
   int next;

   /* "source"
       1 to: (self lastPointerOf: objectPointer) - 1 do:
           [ :offset |
               next <- self heapChunkOf: objectPointer word: offset.
               ((self isIntegerObject: next)==false and: [predicate value: next])
                   ifTrue: "it's a non-immediate object and it should be processed"
                       [self forAllOtherObjectsAccessibleFrom: next
                           suchThat: predicate
                           do: action]].
       "all pointers have been followed; now perform the action"
       action value: objectPointer.
       ^objectPointer
   */
    
    int limit = lastPointerOf(objectPointer) - 1;
    //N.B. start at offset 1, which is class field
    for(int offset = 1; offset <= limit; offset++)
    {
        next = heapChunkOf_word(objectPointer, offset);
        if (!isIntegerObject(next) && predicate(next))
        {
            // it's a non-immediate object and it should be processed
            forAllOtherObjectsAccessibleFrom_suchThat_do(next,
                                                         predicate,
                                                         action);
        }
    }
    
    // all pointers have been followed; now perform the action
    action(objectPointer);
    return objectPointer;
}
#else
// forAllOtherObjectsAccessibleFrom:suchThat:do:
int ObjectMemory::forAllOtherObjectsAccessibleFrom_suchThat_do(
  int objectPointer,
  const std::function <bool (int)>& predicate,
  const std::function <void (int)>& action
)
{
   int prior;
   int current;
   int offset;
   int size;
   int next;

   /* "source"
   	"compute prior, current, offset, and size to begin processing objectPointer"
   	prior <- NonPointer.
   	current <- objectPointer.
   	offset <- size <- self lastPointerOf: objectPointer.
   	[true] whileTrue: "for all pointers in all objects traversed"
   		[(offset <- offset - 1) > 0 "decrement the field index"
   			ifTrue: "the class hasn't been passed yet"
   				[next <- self heapChunkOf: current word: offset.
   						"one of the pointers"
   				((self isIntegerObject: next)==false and: [predicate value: next])
   					ifTrue: "it's a non-immediate object and it should be processed"
   						["reverse the pointer chain"
   						self heapChunkOf: current word: offset put: prior.
   						"save the offset either in the count field or in the extra word"
   						size < HugeSize
   							ifTrue: [self countBitsOf: current put: offset]
   							ifFalse: [self heapChunkOf: current
   									word: size+1 put: offset]. "ERROR dbanay size not size + 1"
   						"compute prior, current, offset, and size to begin processing next"
   						prior <- current.
   						current <- next.
   						offset <- size <- self lastPointerOf: current]]
   			ifFalse: ["all pointers have been followed; now perform the action"
   				action value: objectPointer. "ERROR dbanay - should be current"
   				"did we get here from another object?"
   				prior = NonPointer
   					ifTrue: "this was the root object, so we are done"
   						[^objectPointer].
   				"restore next, current and size to resume processing prior"
   				next <- current.
   				current <- prior.
   				size <- self lastPointerOf: current.
   				"restore offset either from the count field or from the extra word"
   				size < HugeSize
   					ifTrue: [offset <- self countBitsOf: current]
   					ifFalse: [offset <- self heapChunkOf: current word: size+1]. "ERROR dbanay size not size + 1"
   				"restore prior from the reversed pointer chain"
   				prior <- self heapChunkOf: current word: offset.
   				"restore (un-reverse) the pointer chain"
   				self heapChunkOf: current word: offset put: next]]
   */
    
    prior = NonPointer;
    current = objectPointer;
    offset = size = lastPointerOf(objectPointer);
    for(;;)
    {
        // for all pointers in all objects traversed
        offset--; // decrement the field index
        if (offset > 0) // the class hasn't been passed yet
        {
            next = heapChunkOf_word(current, offset); // one of the pointers
            if (!isIntegerObject(next) && predicate(next))
            {
                // it's a non-immediate object and it should be processed
                // reverse pointer chain
                heapChunkOf_word_put(current, offset, prior);
                // save the offset either in the count field or in the extra word
                if (size < HugeSize)
                    countBitsOf_put(current, offset);
                else
                    heapChunkOf_word_put(current, size, offset);
                // compute prior, current, offset, and size to begin processing next
                prior = current;
                current = next;
                offset = size = lastPointerOf(current);
            }
        }
        else
        {
            // all pointers have been followed; now perform the action
            action(current);
            // did we get here from another object?
            if (prior == NonPointer) // this was the root object, so we are done
                return objectPointer;
            // restore next, current and size to resume processing prior
            next = current;
            current = prior;
            size = lastPointerOf(current);
            // restore offset either from the count field or from the extra word
            if (size < HugeSize)
                offset = countBitsOf(current);
            else
                offset = heapChunkOf_word(current, size);
            // restore prior from the reversed pointer chain
            prior = heapChunkOf_word(current, offset);
            // restore (un-reverse) the pointer chain
            heapChunkOf_word_put(current, offset, next);
         }
    }
 }
#endif


// forAllObjectsAccessibleFrom:suchThat:do:
int ObjectMemory::forAllObjectsAccessibleFrom_suchThat_do(int objectPointer,
                                                          const std::function <bool (int)>& predicate,
                                                          const std::function <void (int)>& action)
{
   /* "source"
   	(predicate value: objectPointer)
   		ifTrue: [^self forAllOtherObjectsAccessibleFrom: objectPointer
   				suchThat: predicate
   				do: action]
   */
    
    if (predicate(objectPointer))
    {
        return forAllOtherObjectsAccessibleFrom_suchThat_do(objectPointer,
                predicate,
                action);
    }
   
    return NilPointer;
    
}


// storePointer:ofObject:withValue:
int ObjectMemory::storePointer_ofObject_withValue(int fieldIndex, int objectPointer, int valuePointer)
{
   int chunkIndex;

   /* "source"
   	chunkIndex <- HeaderSize + fieldIndex.
   	self countUp: valuePointer.
   	self countDown: (self heapChunkOf: objectPointer word: chunkIndex).
   	^self heapChunkOf: objectPointer word: chunkIndex put: valuePointer
   */

    RUNTIME_CHECK(fieldIndex >= 0 && fieldIndex < fetchWordLengthOf(objectPointer));
    RUNTIME_CHECK(valuePointer > 0);

    chunkIndex = HeaderSize + fieldIndex;
    
#ifdef GC_REF_COUNT
    countUp(valuePointer);
    countDown(heapChunkOf_word(objectPointer, chunkIndex));
#endif
    return heapChunkOf_word_put(objectPointer, chunkIndex, valuePointer);
}


// storeWord:ofObject:withValue:
int ObjectMemory::storeWord_ofObject_withValue(int wordIndex, int objectPointer, int valueWord)
{
   /* "source"
   	^self heapChunkOf: objectPointer word: HeaderSize + wordIndex
   		put: valueWord
   */
    RUNTIME_CHECK(wordIndex >= 0 && wordIndex < fetchWordLengthOf(objectPointer));
    return heapChunkOf_word_put(objectPointer, HeaderSize + wordIndex,
                                valueWord);
}


// initialInstanceOf:
int ObjectMemory::initialInstanceOf(int classPointer)
{
    // Mario checks for the count bit not being zero. This is necessary
    // because an OT entry with a clear free bit but a ZERO count marks
    // an allocated chunk of memory available for use
   /* "source"
   	0 to: ObjectTableSize-2 by: 2 do:
   		[ :pointer |
   			"ERROR in next line, second part of test omitted"
   			((self freeBitOf: pointer) = 0 and: [(self countBitOf: pointer) ~= 0])
   				ifTrue: [(self fetchClassOf: pointer) = classPointer
   						ifTrue: [^pointer]]].
   	^NilPointer
   */
    
    for(int pointer = 0; pointer <= ObjectTableSize-2; pointer += 2)
    {
        // Only consider non-free entries that are not free chunks
        if (freeBitOf(pointer) == 0 && countBitsOf(pointer) != 0)
        {
            if (fetchClassOf(pointer) == classPointer)
                return pointer;
        }
    }
    return NilPointer;
}



// swapPointersOf:and:
void ObjectMemory::swapPointersOf_and(int firstPointer, int secondPointer)
{

   int firstSegment;
   int firstLocation;
   int firstPointerBit; /* dbanay - ERROR was named firstPointer */
   int firstOdd;

   /* "source"
   	"ERROR in next line, firstPointer redefined"
    "dbanay - should have been called firstPointerBit (FYI - NOT FIXED IN July 1985 ed)"
   	firstSegment <- self segmentBitsOf: firstPointer.
   	firstLocation <- self locationBitsOf: firstPointer.
    "dbanay ERROR - next should be: firstPointerBit <-  self pointerBitOf: firstPointer.
   	firstPointer <- self pointerBitOf: firstPointer.
   	firstOdd <- self oddBitOf: firstPointer.
   	self segmentBitsOf: firstPointer put: (self segmentBitsOf: secondPointer).
   	self locationBitsOf: firstPointer put: (self locationBitsOf: secondPointer).
   	self pointerBitOf: firstPointer put: (self pointerBitOf: secondPointer).
   	self oddBitOf: firstPointer put: (self oddBitOf: secondPointer).
   	self segmentBitsOf: secondPointer put: firstSegment.
   	self locationBitsOf: secondPointer put: firstLocation.
    "ERROR dbanay - should be firstPointerBit"
   	self pointerBitOf: secondPointer put: firstPointer.
   	self oddBitOf: secondPointer put: firstOdd
   */

    firstSegment = segmentBitsOf(firstPointer);
    firstLocation = locationBitsOf(firstPointer);
    firstPointerBit = pointerBitOf(firstPointer);
    firstOdd = oddBitOf(firstPointer);
    
    segmentBitsOf_put(firstPointer, segmentBitsOf(secondPointer));
    locationBitsOf_put(firstPointer, locationBitsOf(secondPointer));
    pointerBitOf_put(firstPointer, pointerBitOf(secondPointer));
    oddBitOf_put(firstPointer, oddBitOf(secondPointer));
    
    segmentBitsOf_put(secondPointer, firstSegment);
    locationBitsOf_put(secondPointer, firstLocation);
    pointerBitOf_put(secondPointer, firstPointerBit);
    oddBitOf_put(secondPointer, firstOdd);
}


// instantiateClass:withWords:
int ObjectMemory::instantiateClass_withWords(int classPointer, int length)
{
   int size;

   /* "source"
   	size <- HeaderSize + length.
   	^self allocate: size odd: 0 pointer: 0 extra: 0 class: classPointer
   */
    size = HeaderSize + length;
    return allocate_odd_pointer_extra_class(size, 0, 0, 0, classPointer);
}


// instantiateClass:withBytes:
int ObjectMemory::instantiateClass_withBytes(int classPointer, int length)
{
   int size;

   /* "source"
   	size <- HeaderSize + ((length + 1)/2).
   	^self allocate: size odd: length\\2 pointer: 0 extra: 0 class: classPointer
   */
    size = HeaderSize + ((length + 1)/2);
    return allocate_odd_pointer_extra_class(size, length % 2, 0, 0, classPointer);
}


// hasObject:
bool ObjectMemory::hasObject(int objectPointer)
{
   /* "source"
   	"is objectPointer pointing to a valid object ?"
   	^(self freeBitOf: objectPointer) = 0
   		"not sure that the first clause is necessary"
   		and: [(self countBitsOf: objectPointer) ~= 0]
   */
    cantBeIntegerObject(objectPointer); // dbanay
    return freeBitOf(objectPointer) == 0 && countBitsOf(objectPointer) != 0;
}

int ObjectMemory::auditFreeOops()
{
    int count = 0;
    for(int objectPointer = 2; objectPointer < ObjectTableSize; objectPointer+=2)
    {
        if (!hasObject(objectPointer))
            count++;
    }
    
    return count;
}



// instantiateClass:withPointers:
int ObjectMemory::instantiateClass_withPointers(int classPointer, int length)
{
   int size;
   int extra;

   /* "source"
   	size <- HeaderSize + length.
   	extra <- size < HugeSize ifTrue: [0] ifFalse: [1].
   	^self allocate: size odd: 0 pointer: 1 extra: extra class: classPointer
   */
    
    size = HeaderSize + length;
    extra = size < HugeSize ? 0 : 1;
    return allocate_odd_pointer_extra_class(size, 0, 1, extra, classPointer);
}


// instanceAfter:
int ObjectMemory::instanceAfter(int objectPointer)
{
   int classPointer;

   /* "source"
    "July 1985 ed also got this wrong"
   	"ERROR: next line omitted by G&R"
   	classPointer <- self fetchClassOf: objectPointer.
    "dbanay - ERROR - was 'objectPointer to: ObjectTableSize-2 by: 2 do:' should start at objectPointer+2'"
    "dbanay - ERROR - should also check count bits to skip free Chunk entries"
   	objectPointer to: ObjectTableSize-2 by: 2 do:
   		[ :pointer |
   			(self freeBitOf: pointer) = 0
   				ifTrue: [(self fetchClassOf: pointer) = classPointer
   						ifTrue: [^pointer]]].
   	^NilPointer
   */
   
    classPointer = fetchClassOf(objectPointer);
    
    for(int pointer = objectPointer+2; pointer <= ObjectTableSize-2; pointer += 2)
    {
        if (hasObject(pointer))
        {
            if (fetchClassOf(pointer) == classPointer)
                return pointer;
        }
    }
    return NilPointer;
    
}



// cantBeIntegerObject:
void ObjectMemory::cantBeIntegerObject(int objectPointer)
{
   /* "source"
   	(self isIntegerObject: objectPointer)
   		"ERROR: Sensor and notify: not defined"
   		ifTrue: [Sensor notify: 'A small integer has no object table entry']
   */

    assert(!isIntegerObject(objectPointer));
    if (isIntegerObject(objectPointer))
        hal->error("A small integer has no object table entry");
}



// obtainPointer:location:
int ObjectMemory::obtainPointer_location(int size, int location)
{
   int objectPointer;

   /* "source"
   	objectPointer <- self removeFromFreePointerList.
   	objectPointer isNil ifTrue: [^nil].
   	self ot: objectPointer put: 0.
   	self segmentBitsOf: objectPointer put: currentSegment.
   	self locationBitsOf: objectPointer put: location.
   	self sizeBitsOf: objectPointer put: size.
   	^objectPointer
   */
    RUNTIME_CHECK(currentSegment >= FirstHeapSegment && currentSegment <= LastHeapSegment);
    objectPointer = removeFromFreePointerList();
    if (objectPointer == NilPointer)
        return NilPointer;
    ot_put(objectPointer, 0);
    segmentBitsOf_put(objectPointer, currentSegment);
    locationBitsOf_put(objectPointer, location);
    sizeBitsOf_put(objectPointer, size);
    return objectPointer;
}


// attemptToAllocateChunk:
int ObjectMemory::attemptToAllocateChunk(int size)
{
   int objectPointer;
  
   /* "source"
   	objectPointer <- self attemptToAllocateChunkInCurrentSegment: size.
   	objectPointer isNil ifFalse: [^objectPointer].
   	1 to: HeapSegmentCount do:
   		[ :i |
   			currentSegment <- currentSegment + 1.
   			currentSegment > LastHeapSegment
   				ifTrue: [currentSegment <- FirstHeapSegment].
   			self compactCurrentSegment.
   			objectPointer
   				 <- self attemptToAllocateChunkInCurrentSegment: size.
   			objectPointer isNil ifFalse: [^objectPointer]].
   	^nil
   */
 
    RUNTIME_CHECK(currentSegment >= FirstHeapSegment && currentSegment <= LastHeapSegment);
    objectPointer = attemptToAllocateChunkInCurrentSegment(size);
    if (objectPointer != NilPointer) return objectPointer;
    for(int i = 1; i <= HeapSegmentCount; i++)
    {
        currentSegment++;
        if (currentSegment > LastHeapSegment)
            currentSegment = FirstHeapSegment;
        compactCurrentSegment();
        objectPointer = attemptToAllocateChunkInCurrentSegment(size);
        if (objectPointer != NilPointer)
            return objectPointer;

    }
    return NilPointer;
}


// attemptToAllocateChunkInCurrentSegment:
int ObjectMemory::attemptToAllocateChunkInCurrentSegment(int size)
{
   int objectPointer = NilPointer;
   int predecessor;
   int next;
   int availableSize;
   int excessSize;
   int newPointer;

   /* "source"
    ERROR dbanay - LastFreeChunkList is a LOCATION and not a size. Should use BigSize
   	size < BigSize
   		ifTrue: [objectPointer <- self removeFromFreeChunkList: size].
   	objectPointer notNil
   		ifTrue: [^objectPointer]. "small chunk of exact size handy so use it"
   	predecessor <- NonPointer.
   		"remember predecessor of chunk under consideration"
   	objectPointer <- self headOfFreeChunkList: LastFreeChunkList
   				inSegment: currentSegment.
   	"the search loop stops when the end of the linked list is encountered"
   	[objectPointer = NonPointer] whileFalse:
   		[availableSize <- self sizeBitsOf: objectPointer.
   		availableSize = size
   			ifTrue: "exact fit - remove from free chunk list and return"
   				[next <- self classBitsOf: objectPointer.
   						"the link to the next chunk"
   				predecessor = NonPointer
   					ifTrue: "it was the head of the list; make the next item the head"
   						[self headOfFreeChunkList: LastFreeChunkList
   							inSegment: currentSegment
   							put: next]
   					ifFalse: "it was between two chunks; link them together"
   						[self classBitsOf: predecessor
   							put: next].
   				^objectPointer].
   			"this chunk was either too big or too small; inspect the amount of variance"
   			excessSize <- availableSize - size.
   			excessSize >= HeaderSize
   				ifTrue: "can be broken into two usable parts: return the second part"
   					["obtain an object table entry for the second part"
   					newPointer <- self obtainPointer: size
   							location: (self locationBitsOf: objectPointer)
   								+ excessSize.
   					newPointer isNil ifTrue: [^nil].
   					"correct the size of the first part (which remains on the free list)"
   					self sizeBitsOf: objectPointer put: excessSize.
   					^newPointer]
   				ifFalse: "not big enough to use; try the next chunk on the list"
   					[predecessor <- objectPointer.
   					objectPointer <- self classBitsOf: objectPointer]].
   	^nil "the end of the linked list was reached and no fit was found"
   */
    RUNTIME_CHECK(currentSegment >= FirstHeapSegment && currentSegment <= LastHeapSegment);
    if (size < BigSize)
        objectPointer = removeFromFreeChunkList(size);
    
    if (objectPointer != NilPointer)
    {
        return objectPointer; // small chunk of exact size handy so use it
    }
    predecessor = NonPointer; // remember predecessor of chunk under consideration
    objectPointer = headOfFreeChunkList_inSegment(BigSize, currentSegment);
    
    // the search loop stops when the end of the linked list is encountered
    while (objectPointer != NonPointer)
    {
        availableSize = sizeBitsOf(objectPointer);
        if (availableSize == size)
        {
            // exact fit - remove from free chunk list and return
            next = classBitsOf(objectPointer);  // the link to the next chunk
            if (predecessor == NonPointer)
            {
                // it was the head of the list; make the next item the head
                headOfFreeChunkList_inSegment_put(BigSize, currentSegment, next);
            }
            else
            {
                // it was between two chunks; link them together
                classBitsOf_put(predecessor, next);
            }
            return objectPointer;
        }
        
        // this chunk was either too big or too small; inspect the amount of variance
        excessSize = availableSize - size;
        if (excessSize >= HeaderSize)
        {
            // can be broken into two usable parts: return the second part
            // obtain an object table entry for the second part
            newPointer = obtainPointer_location(size, locationBitsOf(objectPointer) + excessSize);
            if (newPointer == NilPointer)
                return NilPointer;
            // correct the size of the first part (which remains on the free list)
            sizeBitsOf_put(objectPointer, excessSize);
            return newPointer;
        }
        else
        {
            // not big enough to use; try the next chunk on the list
            predecessor = objectPointer;
            objectPointer = classBitsOf(objectPointer);
        }
    }

    return NilPointer; // the end of the linked list was reached and no fit was found
}

