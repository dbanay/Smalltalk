//
//  filesystem.h
//  Smalltalk-80
//
//  Created by Dan Banay on 5/3/20.
//  Copyright © 2020 Banay. All rights reserved.
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


