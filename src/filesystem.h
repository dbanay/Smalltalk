//
//  filesystem.h
//  Smalltalk-80
//
//  Created by Dan Banay on 5/3/20.
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
#include <functional>
#include <cstdint>

class IFileSystem
{
public:
    // File oriented operations
    virtual int create_file(const char *name) = 0;
    virtual int open_file(const char *name) = 0;
    virtual int close_file(int file_handle) = 0;

    virtual int seek_to(int file_handle, int position) = 0;
    virtual int tell(int file_handle) = 0;
    virtual int read(int file_handle, char *buffer, int bytes) = 0;
    virtual int write(int file_handle, const char *buffer, int bytes) = 0;


    virtual bool truncate_to(int file_handle, int length) = 0;
    virtual int  file_size(int file_handle) = 0;
    virtual bool file_flush(int file_handle) = 0;

    // Directory orientated operations
    virtual void enumerate_files(const std::function <void (const char *) >& each) = 0;
    
    virtual bool rename_file(const char *old_name, const char *new_name) = 0;
    virtual bool delete_file(const char* file_name) = 0;

    // Error handling
    virtual const int last_error() = 0;
    virtual const char *error_text(int code) = 0;
};


