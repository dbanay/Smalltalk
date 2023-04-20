You will need SDL2.0.12 or later to build Smalltalk.

Snarf the source code from https://www.libsdl.org/ and unpack, configure, make,
and make install, or simply install the SDL2 package from ports.

## OpenBSD

Tested to work on OpenBSD 7.3

Install SDL2:

```
pkg_add sdl2
```

## FreeBSD

Tested to work on FreeBSD 13.2-RELEASE

Install SDL2:

```
pkg install sdl2
```

## NetBSD

This process is not fully working. Smalltalk launches with a blank screen and
the error:

```
ERROR: Couldn't LOCK SDL: Parameter 'texture' is invalid
```

However, if you do want to try to get this working, this will get you to the
above point:

Modify the Makefile to point to `/usr/pkg/include` and `/usr/pkg/lib` for the
`-I` and `-L` flags respectively. Also, either install `clang` or change
`clang++ to g++`.

Before launching Smalltalk, `export LD_LIBRARY_PATH=/usr/pkg/lib`.

## DragonFlyBSD

Unknown if working.
