//
//  posixfilesystem.h
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

#include "filesystem.h"
#include <string>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <cassert>

#ifndef _WIN32
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#else
#include <windows.h>
#include <io.h>
#endif

class PosixST80FileSystem: public IFileSystem
{
public:

    PosixST80FileSystem(const std::string& root) : root_directory(root)
    {
    }

    std::string path_for_file(const char *name)
    {
        return root_directory + "/" + name;
    }

    // File oriented operations
    int open_file(const char *name)
    {
        int fd;
        std::string path = path_for_file(name);
#ifdef _WIN32

        errno_t code = _sopen_s(&fd, path.c_str(), _O_RDWR | _O_BINARY, _SH_DENYNO, _S_IREAD | _S_IWRITE);

#else
        fd = open(path.c_str(), O_RDWR );

#endif
        return fd;
    }

    int create_file(const char *name)
    {
        int fd;
        std::string path = path_for_file(name);
#ifdef _WIN32
        errno_t code = _sopen_s(&fd, path.c_str(), _O_RDWR | _O_BINARY | _O_CREAT | _O_TRUNC, _SH_DENYNO, _S_IREAD | _S_IWRITE);
#else
        fd = open(path.c_str(), O_RDWR | O_CREAT | O_TRUNC, 0644);
#endif
        return fd;
    }

    
    int close_file(int file_handle)
    {
#ifdef _WIN32
        if (file_handle == -1) return -1;
        return _close(file_handle);
#else
        return close(file_handle);
#endif
    }

    int read(int file_handle, char *buffer, int bytes)
    {
#ifdef _WIN32
        return _read(file_handle, buffer, bytes);
#else
        return (int) ::read(file_handle, buffer, bytes);
#endif
    }

    int write(int file_handle, const char *buffer, int bytes)
    {
#ifdef _WIN32
        return _write(file_handle, buffer, bytes);
#else
        return (int) ::write(file_handle, buffer, bytes);
#endif
    }


    bool truncate_to(int file_handle, int length)
    {
#ifdef _WIN32
        return _chsize(file_handle, length) != -1;
#else
        return ftruncate(file_handle, length) != -1;
#endif
    }

    int file_size(int file_handle)
    {
#ifdef _WIN32
        if (file_handle == -1) return -1;
        
        struct _stat s;
        if (_fstat(file_handle, &s) != -1)
        {
            return (int) s.st_size;
        }

        return -1;
#else
        struct stat s;
        if (fstat(file_handle, &s) != -1)
        {
            return (int) s.st_size;
        }

        return -1;
#endif
    }

    bool file_flush(int file_handle)
    {
#ifndef _WIN32
        return fsync(file_handle) != -1;
#else
        return true;
#endif
    }
    
#ifndef _WIN32
    bool is_diretory(const char *name)
    {
        std::string path = path_for_file(name);
        struct stat statbuf;
        if (stat(path.c_str(), &statbuf) != 0)
            return 0;
        return S_ISDIR(statbuf.st_mode);
    }
#endif

    // Directory orientated operations
    void enumerate_files(const std::function <void (const char *) >& each)
    {
#ifndef _WIN32
        DIR *dir = opendir(root_directory.c_str());
        struct dirent *entry;

        while ((entry = readdir(dir)))
        {
            // skip hidden files and ., and ..
            if (entry->d_name[0] != '.' && !is_diretory(entry->d_name))
            {
                each(entry->d_name);
            }
        }

        closedir(dir);
#else
        HANDLE hFind = INVALID_HANDLE_VALUE;
        _WIN32_FIND_DATAA findFileData;

        std::string pattern = root_directory + "\\*";
        
        hFind = FindFirstFileA(pattern.c_str(), &findFileData);
        if (hFind != INVALID_HANDLE_VALUE)
        {
            do
            {
                if (findFileData.cFileName[0] != '.')
                {
                    if ((findFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
                    {
                        each(findFileData.cFileName);
                    }
                }
            }
            while (FindNextFileA(hFind, &findFileData) != 0);
            FindClose(hFind);

        }
#endif
    }

    bool rename_file(const char *old_name, const char *new_name)
    {
        std::string old_path = path_for_file(old_name);
        std::string new_path = path_for_file(new_name);

        return rename(old_path.c_str(), new_path.c_str()) != -1;
    }

    bool delete_file(const char* file_name)
    {
        std::string path = path_for_file(file_name);

#ifdef _WIN32
        return _unlink(path.c_str()) != -1;

#else
        return unlink(path.c_str()) != -1;
#endif
    }

    int seek_to(int file_handle, int position)
    {
#ifdef _WIN32
        return (int) _lseek(file_handle, position, SEEK_SET);
#else
        return (int) lseek(file_handle, position, SEEK_SET);
#endif

    }
    
    int tell(int file_handle)
    {
#ifdef _WIN32
        return (int) _lseek(file_handle, 0, SEEK_CUR);
#else
        return (int) lseek(file_handle, 0, SEEK_CUR);
#endif
    }

    // Error handling
    const int last_error()
    {
        return errno;
    }

    const char *error_text(int code)
    {
#ifdef _WIN32
        static char errmsg[512];
        strerror_s(errmsg, sizeof(errmsg), code);
        return errmsg;
#else
        return strerror(code);
#endif
    }

private:
    std::string root_directory;

};
