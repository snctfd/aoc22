CC = gcc
CFLAGS = -Wall -g -O2

default: build

build: cleanup

cleanup: cleanup.o
	${CC} ${CFLAGS} -o cleanup cleanup.o

cleanup.o: cleanup.c
	${CC} ${CFLAGS} -c cleanup.c

clean:
	rm -f cleanup.o cleanup