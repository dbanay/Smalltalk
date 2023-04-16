CFLAGS := -std=c++17 -O3 -I/usr/local/include

CC = clang++
SRC := ../src
OBJS := objmemory.o bitblt.o main.o interpreter.o

Smalltalk: $(OBJS)
	$(CC) -o $@ $(OBJS) -L/usr/local/lib -lSDL2main -lSDL2

$(OBJS): $(SRC)/*.cpp
	$(CC) $(CFLAGS) -c $<


clean:
	rm -f *.o Smalltalk

