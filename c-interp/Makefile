CC := gcc
CFLAGS :=  -g3
%.o: %.c scheme.h

all: scheme.o gc.o prims.o types.o read.o write.o  debug.o  scheme.h
	$(CC) -o scheme scheme.o gc.o prims.o types.o read.o write.o debug.o



