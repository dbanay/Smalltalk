//
//  bitblt.cpp
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

#include "bitblt.h"

/* "source"
 "initialize a table of bit masks ... p.356"
 RightMasks <-
     #(0    16r1 16r3 16r7 16rF
         16r1F 16r3F 16r7F 16rFF
         16r1FF 16r3FF 16r7FF 16rFFF
         16r1FFF 16r3FFF 16r7FFF 16rFFFF).
 AllOnes <- 16rFFFF
*/

static std::uint16_t RightMasks[] = {
    0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF,
    0x1FF, 0x3FF, 0x7FF, 0xFFF, 0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF
};


// clipRange
void BitBlt::clipRange()
{
   /* "source"
   	"clip and adjust source origin and extent appropriately"
   	"first in x"
   	destX >= clipX
   		ifTrue: [sx <- sourceX.
   			dx <- destX.
   			w <- width]
   		ifFalse: [sx <- sourceX + (clipX - destX).
   			w <- width - (clipX - destX).
   			dx <- clipX].
   	(dx + w) > (clipX + clipWidth)
   		ifTrue: [w <- w - ((dx + w) - (clipX + clipWidth))].
   	"then in y"
   	destY >= clipY
   		ifTrue: [sy <- sourceY.
   			dy <- destY.
   			h <- height]
   		ifFalse: [sy <- sourceY + clipY - destY.
   			h <- height - (clipY - destY).
   			dy <- clipY].
   	(dy + h) > (clipY + clipHeight)
   		ifTrue: [h <- h - ((dy + h) - (clipY + clipHeight))].
   	sx < 0
   		ifTrue: [dx <- dx - sx.
   			w <- w + sx.
   			sx <- 0].
    "ERROR dbanay need to check if sourceForm is nil."

   	sx + w > sourceForm width
   		ifTrue: [w <- w - (sx + w - sourceForm width)].
   	sy < 0
   		ifTrue: [dy <- dy - sy.
   			h <- h + sy.
   			sy <- 0].
   	sy + h > sourceForm height
   		ifTrue: [h <- h - (sy + h - sourceForm height)]
   */
    
     if (clipX < 0)
     {
         clipWidth += clipX;
         clipX = 0;
     }
     if (clipY < 0)
     {
         clipHeight += clipY;
         clipY = 0;
     }
    
     if ((clipX + clipWidth) > destFormWidth)
     {
         clipWidth = destFormWidth - clipX;
     }
    
     if ((clipY + clipHeight) > destFormHeight)
     {
         clipHeight = destFormHeight - clipY;
     }

    // clip and adjust source origin and extent appropriately
   // first in x
    if (destX >= clipX)
    {
        sx = sourceX;
        dx = destX;
        w = width;
    }
    else
    {
        sx = sourceX + (clipX - destX);
        w = width - (clipX - destX);
        dx = clipX;
    }
    if  ((dx + w) > (clipX + clipWidth))
    {
        w = w - ((dx + w) - (clipX + clipWidth));
    }
    
    //then in y
    if (destY >= clipY)
    {
        sy = sourceY;
        dy = destY;
        h = height;
    }
    else
    {
        sy = sourceY + clipY - destY;
        h = height - (clipY - destY);
        dy = clipY;
    }
    if ((dy + h) > (clipY + clipHeight))
    {
        h = h - ((dy + h) - (clipY + clipHeight));
    }
    
    if (sourceForm == NilPointer)
        return;
    
    if (sx < 0)
    {
        dx = dx - sx;
        w = w + sx;
        sx = 0;
    }
    
    if (sx + w > sourceFormWidth)
    {
        w = w - (sx + w - sourceFormWidth);
    }
    
    if (sy < 0)
    {
        dy = dy - sy;
        h = h + sy;
        sy = 0;
    }
    
    if (sy + h > sourceFormHeight)
    {
        h = h - (sy + h - sourceFormHeight);
    }
}


// WordArray class>>maxSize
// maxSize
//      "The maximum size of a WordArray is 64640 elements."
//      ^64640
//
// When a Form is allocated, the number of words required to hold the bits is
// (width + 15)/16 * height. If the form is large enough this will exceed maxSize
// and rather than fail, Smalltalk clamps the array size to 64640! This mucks
// things up if such a form is used as a source or target.

// copyBits
bool BitBlt::copyBits()
{
   /* "source"
   	"from copyBits, p.356"
   	"sets w and h"
   	self clipRange.
   	(w <= 0 or: [h <= 0]) ifTrue: [^self ]. "null range"
   	self computeMasks.
   	self checkOverlap.
   	self calculateOffsets.
   	self copyLoop
   */
    
    // from copyBits, p.356
    clipRange();
    if (w > 0 && h > 0)
    {
        updatedX = dx;
        updatedY = dy;
        updatedWidth = w;
        updatedHeight = h;
        computeMasks();
        // Check if source or dest is "bad"
        if (sourceForm != NilPointer && formWordCount(sourceFormWidth, sourceFormHeight) != sourceBitsWordLength  )
            return false;
        if (formWordCount(destFormWidth, destFormHeight) != destBitsWordLength  )
            return false;

        checkOverlap();
        calculateOffsets();
        copyLoop();
    }
    else
    {
        updatedX = 0;
        updatedY = 0;
        updatedWidth = 0;
        updatedHeight = 0;

    }
    
    return true;
    
}


// copyLoop
void BitBlt::copyLoop()
{
   std::uint16_t prevWord;
   std::uint16_t thisWord = 0; //eliminate warning
   std::uint16_t skewWord;
   std::uint16_t mergeMask;
   std::uint16_t halftoneWord;
   std::uint16_t mergeWord;
   int word;

   /* "source"
   	1 to: h do: "here is the vertical loop"
   		[ :i |
   			(halftoneForm notNil)
   				ifTrue: [halftoneWord <- halftoneBits at: (1 + (dy bitAnd: 15)).
   					dy <- dy + vDir]
   				ifFalse: [halftoneWord <- AllOnes].
   			skewWord <- halftoneWord.
   			preload
   				ifTrue: [prevWord <- sourceBits at: sourceIndex + 1.
   					"load the 32-bit shifter"
   					sourceIndex <- sourceIndex + hDir]
   				ifFalse: [prevWord <- 0].
   			mergeMask <- mask1.
   			"ERROR: extra to on next line"
   			1 to: nWords do: "here is the inner horizontal loop"
   				[ :word |
   					sourceForm notNil "if source used"
   						ifTrue: [prevWord <- prevWord bitAnd: skewMask.
                            "ERROR: dbanay need to check if out of range. use 0 if that is the case"
   							thisWord <- sourceBits at: sourceIndex + 1.
   									"pick up next word"
   							skewWord <-
   								prevWord bitOr: (thisWord bitAnd:
   									skewMask bitInvert).
   							prevWord <- thisWord.
   							skewWord <- (skewWord bitShift: skew) bitOr:
   									(skewWord bitShift: skew - 16)].
   									"16-bit rotate"
   					mergeWord <- self merge: (skewWord bitAnd: halftoneWord)
   							with: (destBits at: destIndex + 1).
   					destBits
   						at: destIndex + 1
   						put: ((mergeMask bitAnd: mergeWord)
   							bitOr: (mergeMask bitInvert
   								bitAnd: (destBits at: destIndex + 1))).
   					sourceIndex <- sourceIndex + hDir.
   					destIndex <- destIndex + hDir.
   					word = (nWords - 1)
   						ifTrue: [mergeMask <- mask2]
   						ifFalse: [mergeMask <- AllOnes]].
   			sourceIndex <- sourceIndex + sourceDelta.
   			destIndex <- destIndex + destDelta]
   */
   
    
 
    for(int i = 1; i <=h; i++)
    {
        // here is the vertical loop
        if (halftoneForm != NilPointer)
        {
            halftoneWord = memory.fetchWord_ofObject((dy & 15), halftoneBits);
            dy = dy + vDir;
        }
        else
        {
            halftoneWord = AllOnes;
        }
        skewWord = halftoneWord;
        if (preload)
        {
            // load the 32-bit shifter
            
            prevWord = memory.fetchWord_ofObject(sourceIndex, sourceBits);
            sourceIndex = sourceIndex + hDir;
        }
        else
        {
            prevWord = 0;
        }
        mergeMask = mask1;
        
        // here is the inner horizontal loop
        for(word = 1; word <= nWords; word++)
        {
            if (sourceForm != NilPointer)  // if source used
            {
                prevWord = prevWord & skewMask;
                if (word <= sourceRaster && sourceIndex >= 0 && sourceIndex < sourceBitsWordLength)
                    thisWord = memory.fetchWord_ofObject(sourceIndex, sourceBits);
                //else
                //    thisWord = 0;
                skewWord = prevWord | (thisWord & ~skewMask);
                
                prevWord = thisWord;
                // Note: replaced left shift by negative value with a right shift of complement
                skewWord = (skewWord << skew) | (skewWord >> (16-skew));
            }
            
            if (destIndex >= destBitsWordLength) return;
            
            std::uint16_t destWord =  memory.fetchWord_ofObject(destIndex, destBits);
            // 16-bit rotate
            mergeWord = merge_with(skewWord & halftoneWord,  destWord);
            
            memory.storeWord_ofObject_withValue(destIndex, destBits,
                                                (mergeMask & mergeWord) |
                                                (~mergeMask & destWord));
            sourceIndex = sourceIndex + hDir;
            destIndex = destIndex + hDir;
 
            
            if (word == (nWords - 1))
                mergeMask = mask2;
            else
                mergeMask = AllOnes;
        }
        
        sourceIndex = sourceIndex + sourceDelta;
        destIndex = destIndex + destDelta;

    }
}


// calculateOffsets
void BitBlt::calculateOffsets()
{
   /* "source"
   	"check if need to preload buffer
   	(i.e., two words of source needed for first word of destination)"
   	preload <- (sourceForm notNil) and:
   			[skew ~= 0 and: [skew <= (sx bitAnd: 15)]].
   	hDir < 0 ifTrue: [preload <- preload == false].
   	"calculate starting offsets"
   	sourceIndex <- sy * sourceRaster + (sx // 16).
   	destIndex <- dy * destRaster + (dx // 16).
   	"calculate increments from end of 1 line to start of next"
   	sourceDelta <-
   		(sourceRaster * vDir) -
   			(nWords + (preload ifTrue: [1] ifFalse: [0]) * hDir).
   	destDelta <- (destRaster * vDir) - (nWords * hDir)
   */
   
    // check if need to preload buffer
    // (i.e., two words of source needed for first word of destination)
    preload = (sourceForm != NilPointer) && skew != 0 && skew <= (sx & 15);
    if (hDir < 0) preload = !preload;
    
    // calculate starting offsets
    sourceIndex = sy * sourceRaster + (sx / 16);
    destIndex = dy * destRaster + (dx / 16);
    // calculate increments from end of 1 line to start of next
    // dbanay - note operator precedence change!
    sourceDelta =
        (sourceRaster * vDir) - ((nWords + (preload ? 1 : 0)) * hDir);
    destDelta = (destRaster * vDir) - (nWords * hDir);
}


// computeMasks
void BitBlt::computeMasks()
{
   int startBits;
   int endBits;

   /* "source"
   	"calculate skew and edge masks"
   	destBits <- destForm bits.
   	destRaster <- destForm width - 1 // 16 + 1.
   	sourceForm notNil
   		ifTrue: [sourceBits <- sourceForm bits.
   			sourceRaster <- sourceForm width - 1 // 16 + 1].
   	halftoneForm notNil
   		ifTrue: [halftoneBits <- halftoneForm bits].
   	skew <- (sx - dx) bitAnd: 15.
   	"how many bits source gets skewed to right"
   	startBits <- 16 - (dx bitAnd: 15).
   	" how many bits in first word"
   	mask1 <- RightMasks at: startBits + 1.
   	endBits <- 15 - ((dx + w - 1) bitAnd: 15).
   	"how many bits in last word"
   	mask2 <- (RightMasks at: endBits + 1) bitInvert.
   	skewMask <- 
   		(skew = 0
   			ifTrue: [0]
   			ifFalse: [RightMasks at: 16 - skew + 1]).
   	"determine number of words stored per line; merge masks if necessary"
    "ERROR dbanay : nWords <-  (w - startBits + 15) // 16 + 1 for False case"
   	w < startBits
   		ifTrue: [mask1 <- mask1 bitAnd: mask2.
   			mask2 <- 0.
   			nWords <- 1]
   		ifFalse: [nWords <- (w - startBits - 1) // 16 + 2]
   */
    
    const int BitsInForm = 0;

    
    // calculate skew and edge masks
    destBits = memory.fetchPointer_ofObject(BitsInForm, destForm);
    destBitsWordLength = memory.fetchWordLengthOf(destBits);

    destRaster = (destFormWidth - 1) / 16 + 1;
    if (sourceForm != NilPointer)
    {
        sourceBits = memory.fetchPointer_ofObject(BitsInForm, sourceForm);
        sourceBitsWordLength = memory.fetchWordLengthOf(sourceBits);
        sourceRaster = (sourceFormWidth - 1) / 16 + 1;
    }
    else
        sourceBitsWordLength = 0;
    
    if (halftoneForm != NilPointer)
    {
       halftoneBits = memory.fetchPointer_ofObject(BitsInForm, halftoneForm);
    }
    
    // how many bits source gets skewed to right
    skew = (sx - dx) & 15;
    
    // how many bits in first word
    startBits = 16 - (dx & 15);
    
    mask1 = RightMasks[startBits]; // +1 removed - dbanay -- C/C++ arrays start at 0
    
    // how many bits in last word
    endBits = 15 - ((dx + w - 1) & 15);
    
    mask2 = ~RightMasks[endBits]; // +1 removed - dbanay -- C/C++ arrays start at 0
    skewMask = skew == 0 ? 0 : RightMasks[16 - skew];
    // determine number of words stored per line; merge masks if necessary
    // Bluebook had the nWords calculation wrong when w == startBits
    if (w < startBits)
    {
        mask1 = mask1 & mask2;
        mask2 = 0;
        nWords = 1;
    }
    else
    {
        nWords = (w - startBits + 15) / 16 + 1;
    }
}


// checkOverlap
void BitBlt::checkOverlap()
{
   int t;

   /* "source"
   	"check for possible overlap of source and destination"
   	hDir <- vDir <- 1. "defaults for no overlap"
   	(sourceForm == destForm and: [dy >= sy])
   		ifTrue: [dy > sy "have to start at bottom"
   				ifTrue: [vDir <- -1.
   					sy <- sy + h - 1.
   					dy <- dy + h - 1]
   				ifFalse: [dx > sx "y's are equal, but x's are backward"
   						ifTrue: [hDir <- -1.
   							sx <- sx + w - 1.
   							"start at right"
   							dx <- dx + w - 1.
   							"and fix up masks"
   							skewMask <- skewMask bitInvert.
   							t <- mask1.
   							mask1 <- mask2.
   						mask2 <- t]]]
   */

    // check for possible overlap of source and destination
    hDir = vDir = 1; // defaults for no overlap
    if (sourceForm == destForm && (dy >= sy))
    {
        if (dy > sy) // have to start at bottom
        {
            vDir = -1;
            sy = sy + h - 1;
            dy = dy + h - 1;
        }
        else
        {
            if (dx > sx)
            {
                // y's are equal, but x's are backward
                hDir = -1;
                // start at right
                sx = sx + w - 1;
                dx = dx + w - 1;
                // and fix up masks
                skewMask = ~skewMask;
                t = mask1;
                mask1 = mask2;
                mask2 = t;
            }
        }
    }
}



// CharacterScanner

int CharacterScanner::scanCharactersFrom_to_in_rightX_stopConditions_displaying(
                                                                                int startIndex,
                                                                                int stopIndex,
                                                                                int sourceString,
                                                                                int rightX,
                                                                                int stops,
                                                                                bool displaying)
{
    const int EndOfRun = 257; // TextConstants at: #EndOfRun put: 257.
    const int CrossedX = 258; // TextConstants at: #CrossedX put: 258.    
    
    /* "source"
     | ascii nextDestX |
     lastIndex <- startIndex.
     [lastIndex <= stopIndex]
         whileTrue:
             [ascii <- (sourceString at: lastIndex) asciiValue.
             (stopConditions at: ascii + 1) ~~ nil ifTrue: [^stops at: ascii + 1].
             sourceX <- xTable at: ascii + 1.
             nextDestX <- destX + (width _ (xTable at: ascii + 2) - sourceX).
             nextDestX > rightX ifTrue: [^stops at: CrossedX].
             display ifTrue: [self copyBits].
             destX <- nextDestX.
             lastIndex <- lastIndex + 1].
     lastIndex <- stopIndex.
     ^stops at: EndOfRun

     
     */
    int ascii;
    int nextDestX;
        
    lastIndex = startIndex;
    while (lastIndex <= stopIndex)
    {
        ascii = memory.fetchByte_ofObject(lastIndex-1, sourceString);

        if (memory.fetchPointer_ofObject(ascii, stopConditions) != NilPointer)
        {
            return memory.fetchPointer_ofObject(ascii, stops);
        }

        sourceX = memory.integerValueOf(memory.fetchPointer_ofObject(ascii, xTable));
        width =  memory.integerValueOf(memory.fetchPointer_ofObject(ascii+1, xTable)) - sourceX;
        nextDestX = destX + width;
        if (nextDestX > rightX)
        {
            return memory.fetchPointer_ofObject(CrossedX-1, stops);
        }
        if (displaying) copyBits();
        destX = nextDestX;
        lastIndex = lastIndex + 1;
    }
    
    lastIndex = stopIndex;
    return memory.fetchPointer_ofObject(EndOfRun-1, stops);
}

