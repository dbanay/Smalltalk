//
//  realwordmemory.h
//  Smalltalk-80
//
//  Created by Dan Banay on 2/25/20.
//  Copyright © 2020 Banay. All rights reserved.
//

#pragma once

#include <cstdint>
#include <cassert>

class RealWordMemory
{
public:
    static const int SegmentCount = 16;
    static const int SegmentSize = 65536; /* in words */
    
    RealWordMemory()
    {
    }
    
    inline int segment_word(int s, int w)
    {
        assert(s >= 0 && s < SegmentCount);
        assert(w >= 0 && w < SegmentSize);
        return memory[s][w];
    }
    
    inline int segment_word_put(int s, int w, int value)
    {
        assert(s >= 0 && s < SegmentCount);
        assert(w >= 0 && w < SegmentSize);
        assert(value >= 0 && value < 65536);
        memory[s][w] = value;
        return value;
    }
    
    inline int segment_word_byte(int s, int w, int byteNumber)
    {
        assert(s >= 0 && s < SegmentCount);
        assert(w >= 0 && w < SegmentSize);
        assert(byteNumber == 1 || byteNumber == 0);
        
        // byteNumber 0 is the byte of the word that appears first in member, 1 is the next
        /* big endian */
        //return (memory[s][w] >> (8*byteNumber)) & 0xff;
        return  ((std::uint8_t *) &memory[s][w])[byteNumber];
        /* little endian */
        //return  ((std::uint8_t *) &memory[s][w])[1-byteNumber];
        
    }
    
    inline int segment_word_byte_put(int s, int w, int byteNumber, int value)
    {
        assert(s >= 0 && s < SegmentCount);
        assert(w >= 0 && w < SegmentSize);
        assert(value >= 0 && value < 65536);
        assert(byteNumber == 1 || byteNumber == 0);
        
        // byteNumber 0 is the byte of the word that appears first in member, 1 is the next
        /* big endian */
        ((std::uint8_t *) &memory[s][w])[byteNumber] = value;
        /* little endian */
        //((std::uint8_t *) &memory[s][w])[1-byteNumber] = value;
        
        return value;
    }
    
    // The most significant bit in a word will be referred to with the index 0 and the least significant with the index 15. G&R 657
    inline int segment_word_bits_to(int s, int w, int firstBitIndex, int lastBitIndex)
    {
        assert(s >= 0 && s < SegmentCount);
        assert(w >= 0 && w < SegmentSize);
        
        
        std::uint16_t shift = memory[s][w] >> (15-lastBitIndex);
        std::uint16_t mask = (1 << (lastBitIndex - firstBitIndex + 1)) - 1;
        
        return shift & mask;
    }
    
    inline int segment_word_bits_to_put(int s, int w, int firstBitIndex, int lastBitIndex, int value)
    {
        assert(s >= 0 && s < SegmentCount);
        assert(w >= 0 && w < SegmentSize);
        assert(value >= 0 && value < 65536);
        
        std::uint16_t mask = (1 << (lastBitIndex - firstBitIndex + 1)) - 1;
        assert((value & mask) == value); // make sure it fits
        memory[s][w] = (memory[s][w] & ~(mask << (15-lastBitIndex))) | (value << (15 - lastBitIndex));
        return value;
    }
    
private:
    std::uint16_t memory[SegmentCount][SegmentSize];
    
    
    
};

