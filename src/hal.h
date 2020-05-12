//
//  hal.h
//  Smalltalk-80
//
//  Created by Dan Banay on 4/13/20.
//  Copyright © 2020 Dan Banay. All rights reserved.
//

#pragma once
#include <cstdint>

class IHardwareAbstractionLayer
{
public:
    // Specify the semaphore to signal on input
    virtual void set_input_semaphore(int semaphore) = 0;
    
    // the number of seconds since 00:00 in the morning of January 1, 1901
    virtual std::uint32_t get_smalltalk_epoch_time() = 0;
    
    // the number of milliseconds since the millisecond clock was
    // last reset or rolled over (a 32-bit unsigned number)
    virtual std::uint32_t get_msclock() = 0;
    
    // schedule a semaphore to be signaled at a time. Only one outstanding
    // request may be scheduled at anytime. When called any outstanding
    // request will be replaced (or canceled if semaphore is 0).
    // Will signal immediate if scheduled time has passed.
    virtual void signal_at(int semaphore, std::uint32_t msClockTime) = 0;
    
    // Set the cursor image
    // (a 16 word form)
    virtual void set_cursor_image(std::uint16_t *image) = 0;
 
    // Set the mouse cursor location
    virtual void set_cursor_location(int x, int y) = 0;
    virtual void get_cursor_location(int *x, int *y) = 0;
    virtual void set_link_cursor(bool link) = 0;
    
    // Set the display size
    virtual bool set_display_size(int width, int height) = 0;
    
    // Notify that screen contents changed
    virtual void display_changed(int x, int y, int width, int height) = 0;
    
    //Input queue
    virtual bool next_input_word(std::uint16_t *word) = 0;
    
    virtual void error(const char *message) = 0;
    
    // lifetime
    virtual void signal_quit() = 0;
    virtual void exit_to_debbuger() = 0; // calls abort();
    virtual const char *get_image_name() = 0;
    virtual void set_image_name(const char *new_name) = 0;
};
