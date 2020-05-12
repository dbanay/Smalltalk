//
//  main.c
//
//  Created by Dan Banay on 12/10/19.
//  Copyright © 2019 Banay. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>



#define FalseOop 0x6404
#define FalseClassOop 0x6406
#define TrueOop 0x643A
#define TrueClassOop 0x643C
#define UndefinedObjectOop 0x6480
#define UndefinedObjectClassOop 0x6482
#define String 0xE
#define Form 0xC52
#define DisplayBitmap 0x1E
#define CompiledMethod 0x22
#define Float 0x14
#define LargePositiveInteger 0x1C
#define LargeNegativeInteger 0x1DA0
#define WordArrayClass 0xA72



typedef uint16_t oop;

static long next_page(long pos)
{
    return  ((pos + 512 - 1) / 512) * 512;
}


static void advance_to_page_boundary(FILE *f)
{
    long pos = ftell(f);
    long skip = next_page(pos) - pos;
    fseek(f, skip, SEEK_CUR);

}

static void pad_to_page_boundary(FILE *f)
{
    long pos = ftell(f);
    long pad = next_page(pos) - pos;
    while (pad-- > 0)
    {
        fputc(0, f);
    }

}

uint32_t swap32(uint32_t v)
{
    return ((v & 0xff000000) >> 24) | ((v & 0x00ff0000) >> 8) | ((v & 0x0000ff00) << 8) | (v << 24);
    
}

uint16_t swap16(uint16_t v)
{
    return ((v & 0x00ff)<<8)|((v & 0xff00)>>8);
    
}

uint32_t read_uint32(FILE *f)
{
    uint32_t v;
    size_t len = fread(&v, sizeof(v), 1, f);
    return swap32(v);
}

void write_uint32(FILE *f, uint32_t v)
{
    fwrite(&v, sizeof(v), 1, f);
 }


uint16_t read_uint16(FILE *f)
{
    uint16_t v;
    fread(&v, sizeof(v), 1, f);
    return swap16(v);
}

void write_uint16(FILE *f, uint16_t v)
{
    fwrite(&v, sizeof(v), 1, f);
}




int main(int argc, const char * argv[]) {
    
    FILE *input = fopen(argv[1], "rb");
    FILE *output = fopen(argv[2], "wb");
    
    uint32_t object_space_length = read_uint32(input); // in words
    uint32_t object_table_length = read_uint32(input);
    
    printf("Object Space Len = %u\n", object_space_length);
    printf("Object Table Len = %u\n", object_table_length);

    // If the next two bytes are zero, then the image is in interchange format.
    uint8_t interchange[2];
    fread(interchange, sizeof(interchange), 1, input);
    
    advance_to_page_boundary(input);
    
    uint8_t *om = malloc(object_space_length*2);
    uint16_t *word_object_memory = (uint16_t*) om;
    fread(word_object_memory, object_space_length*sizeof(uint16_t), 1, input);
      
    advance_to_page_boundary(input);

    uint16_t *object_table = (uint16_t *) malloc(object_table_length*sizeof(uint16_t));
    for(int i = 0; i < object_table_length; i++)
        object_table[i] = read_uint16(input);
    
    
    /*
     We realize that some implementations would prefer to have words stored low byte followed by high byte. Unfortunately, there is not one consistent "other" way to store bytes in words. We think that the transformation of the image that would work for most machines is to swap the bytes of all fields accessed as words, and to not swap the fields accessed as bytes. This has been done by a number of other implementations already (see Smalltalk-80, Bits of History, Words of Advice, Glenn Krasner, Ed., Addison-Wesley Publishing Co., Reading Massachusetts, September, 1983). The transformation typically works best by having an auxiliary program translate the image from its interchange format to your internal format. The suggested algorithm which may or may not be best for your system is:
     
     For word-type objects: swap every field.
     For CompiledMethods: swap Length, Class, Header and Literal fields only.
     For all other byte-type objects: swap Length and Class fields only.
     
     
     We conVert the image from the standard interchange format into the HP format by the following transformations:
     1. Convert the interchange format state entries into the HP layout.
     2. Move the class and length from the fields of each object into the object table entry.
     3. Swap the bytes of all non-byte objects (our hosts use low- byte/high-byte ordering rather than the high/low of the image in- terchange format).
     4. Convert objects of class Float in the image from IEEE format to VAX format. The ranges are slightly different so the conversion program indicates when i t encounters a n IEEE floating point number which has no VAX format correspondent.
     5. Create the special HP system state objects in which the snapshot process saves relevant details so that the system may continue lat- er from the same state.

     
     
     */
    
    
    int objectsLoaded = 0;
    int i = 2;
    // entry 0/1 is unused
    while (i < object_table_length)
    {
        uint32_t segment = object_table[i] & 0xf;
        uint32_t ptr = (object_table[i] & 0x40) >> 6; // 1 means objects contain pointers. 0 means words or bytes in body
        uint32_t oddsize = (object_table[i] & 0x80) >> 7;
        uint32_t is_free = (object_table[i] & 0x20) >> 5;
        uint32_t location  = (segment<<16) + object_table[i+1];
        
        if (!is_free)
        {
            uint16_t size = swap16(word_object_memory[location]);
            uint16_t class = swap16(word_object_memory[location+1]);
            int fields = size - 2;

            objectsLoaded++;
            
            
            word_object_memory[location]   = size;
            word_object_memory[location+1] = class;
            
            
            if (class == CompiledMethod)
            {
                uint16_t header = swap16(word_object_memory[location+2]);
                uint16_t literal_count = (header & 0x7E) >> 1;

                assert(header & 1);
                // Swap header/literal fields
                word_object_memory[location+2] = header;
                for(int j = 0; j < literal_count; j++)
                {
                    word_object_memory[location+3+j] = swap16(word_object_memory[location+3+j]);
                }
                
            }
            else if (class == Float)
            {
                uint8_t temp[4];
                uint8_t *bytes = (uint8_t *) &word_object_memory[location+2];
                 
                temp[0] = bytes[3];
                temp[1] = bytes[2];
                temp[2] = bytes[1];
                temp[3] = bytes[0];
                
                bytes[0] = temp[0];
                bytes[1] = temp[1];
                bytes[2] = temp[2];
                bytes[3] = temp[3];
                
                
                printf("Float %f\n", * (float *) bytes);

            }
            else if (ptr || class == DisplayBitmap || class == WordArrayClass)
            {
                // Swap all fields
                for(int f = 0; f < fields; f++)
                {
                    word_object_memory[location+2+f] = swap16(word_object_memory[location+2+f]);
                }
            }

        }
        
        


        
        /* printf("object %d\n", i);
         printf("seg %u\n", segment);
         printf("ptr %u\n", ptr);
         printf("odd %u\n", oddsize);
         printf("count %u\n", count);
         printf("location %u\n", location); //
         printf("free %u\n", is_free);
         printf("=====\n");*/
        
        /* Object layout (obj with N fields):
         
         SIZE = N + 2     (2 for size/class "header")  size is in 16-bits words.
         CLASS Oop
         N FIELDS (each is 16 bits)
         */
        
        uint16_t size = swap16(word_object_memory[location]);
        uint16_t class = swap16(word_object_memory[location+1]);
        int fields = size - 2;
#if 0
        if (class == String)
        {
            unsigned len = fields*2;
            if (oddsize)
            {
                len--;
            }
            char *s = (char *) &word_object_memory[location+2];
            for(int j = 0; j < len; j++)
            {
                printf("%c", s[j]);
            }
            printf("\n");
        }
#endif
#if 0
        if (class == Form)
        {
            /*
             DisplayMedium subclass: #Form
             instanceVariableNames: 'bits width height offset '
             classVariableNames: 'OneBitForm '
             poolDictionaries: ''
             category: 'Graphics-Display Objects'!
             
             
             FORM FIELD 0: 31084
             FORM FIELD 1: 236
             FORM FIELD 2: 165
             FORM FIELD 3: 31086
             
             */
            
            oop display_bitmap = swap16(word_object_memory[location+2]);
            uint32_t seg = object_table[display_bitmap] & 0xf;
            uint32_t loc  = (seg<<16) + object_table[display_bitmap+1]; // word index?
            uint16_t sz = swap16(word_object_memory[loc]);
            uint16_t klass = swap16(word_object_memory[loc+1]);
            int width = asInt(swap16(word_object_memory[location+3]));
            int height = asInt(swap16(word_object_memory[location+4]));;
            static int image_id = 0;
            
            dump_image(image_id++, (char *) &word_object_memory[loc+2], width, height);
            
            
            printf("\n");
        }
#endif
        
        i += 2;
    }
    
    
    // Write new image
    write_uint32(output, object_space_length);
    write_uint32(output, object_table_length);
    
    fwrite(interchange, sizeof(interchange), 1, output);

    pad_to_page_boundary(output);
    long pg = ftell(output);
    
    
    
    fwrite(word_object_memory, object_space_length*sizeof(uint16_t), 1, output);
    

    pad_to_page_boundary(output);

    pg = ftell(output);
    for(int i = 0; i < object_table_length; i++)
        write_uint16(output, object_table[i]);



    
    // Write updated image
    
    fclose(input);
    fclose(output);
    
        
    free(object_table);
    free(om);

    return 0;
}
