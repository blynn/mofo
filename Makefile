.PHONY: target test

CC := gcc
CFLAGS := -O2 --std=gnu99 -Wall

target: mofo

mofo: mofo.c ; $(CC) $(CFLAGS) -o$@ mofo.c ~/pro/blt/blt.c -I ~/pro/blt -lreadline -lgmp

test: mofo ; go test mofo_test.go
