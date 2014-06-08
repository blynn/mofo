.PHONY: target test

CC := gcc
CFLAGS := -O2 --std=gnu99 -Wall

target: mofo

mofo: mofo.c ; $(CC) $(CFLAGS) -o$@ mofo.c blt.c -lreadline -lgmp

test: mofo ; go test mofo_test.go
