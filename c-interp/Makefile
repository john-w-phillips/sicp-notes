CC := gcc
# -DGC_RECORD_STATS=1 
CFLAGS :=  -O1 -g3 -L. 
LIBS := -ldl
%.o: %.c scheme.h
	$(CC) $(CFLAGS) -c -fPIC -o $@ $<

libscheme.so: scheme.o gc.o prims.o types.o read.o write.o debug.o scheme.h hash.o
	$(CC) $(CFLAGS) -shared -o $@ $^ $(LIBS)

$(module): $(module).o $(module).c
	$(CC) $(CFLAGS) -shared -o $@.so $< -L. -lscheme
mods: $(module)

all: repl.o libscheme.so
	$(CC) $(CFLAGS) -o scheme repl.o -lscheme $(LIBS)



