//
//  main.cpp
//  Smalltalk-80
//
//  Created by Dan Banay on 2/20/20.
//  Copyright © 2020 Dan Banay. All rights reserved.
//

#include <iostream>
#include <cassert>
#include "objmemory.h"
#include "oops.h"
#include "UnitTests/UnitTests.h"


#define VIRTUAL_IMAGE_PATH " /Users/dbanay/projects/st80/tools/imageswapper/NewImage"


// Dump interesting things...
void DumpObjects(ObjectMemory& memory)
{
    int pointer;
    
#if 0
    // Dump all strings...
    pointer = memory.initialInstanceOf(ClassStringPointer);
    assert(pointer != NilPointer);
    int count = 0;
    while (pointer != NilPointer)
    {
        count++;
        std::cout << "\"";
        int length = memory.fetchByteLengthOf(pointer);
        for(int i = 0; i < length; i++)
        {
            int ch = memory.fetchByte_ofObject(i, pointer);
            std::cout << (char) ch;
        }
        std::cout << "\"\n";
        
        
        pointer = memory.instanceAfter(pointer);
    }
#endif
    
    static const int FormClass = 0xC52;
    static const int WordArrayClass = 0xA72;
    static const int DisplayScreen = 0x342;
    static const int DisplayBitmap = 0x001E;
    
    // Enumerate Forms in image
    pointer = memory.initialInstanceOf(FormClass);
    assert(pointer != NilPointer);
    while (pointer != NilPointer)
    {
        int displayBitmap = memory.fetchWord_ofObject(0, pointer);
        // displayBitmap    int    12696 is trashed
        int cls = memory.fetchClassOf(displayBitmap);
        assert(cls == WordArrayClass); // WordArray
        int w = memory.integerValueOf(memory.fetchWord_ofObject(1, pointer));
        int h = memory.integerValueOf(memory.fetchWord_ofObject(2, pointer));
        
        std::cout << "Form " << w << "x" << h << "\n";
        pointer = memory.instanceAfter(pointer);
    }

    pointer = memory.initialInstanceOf(DisplayScreen);
    assert(pointer != NilPointer);
    assert(memory.instanceAfter(pointer) == NilPointer); // Only one!
    int displayBitmap = memory.fetchWord_ofObject(0, pointer);
    assert(memory.fetchClassOf(displayBitmap) == DisplayBitmap); //
    int w = memory.integerValueOf(memory.fetchWord_ofObject(1, pointer));
    int h = memory.integerValueOf(memory.fetchWord_ofObject(2, pointer));
    
    std::cout << "Display " << w << "x" << h << "\n";


}





int main(int argc, const char * argv[]) {

   // static ObjectMemory memory;
    
    //memory.loadSnapshot("/Users/dbanay/projects/st80/Smalltalk-80/NewImage");
  //  memory.saveSnapshot("/Users/dbanay/SavedImage4");
  //  memory.loadSnapshot("/Users/dbanay/SavedImage");
 //   memory.saveSnapshot("/Users/dbanay/SavedImage2");

    RunUnitTests();
    
    // memory.saveSnapshot("/Users/dbanay/SavedImage5");
    
    //DumpObjects(memory);
    return 0;
}
