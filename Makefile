.PHONY: target

CC := gcc
CFLAGS := -O2 --std=gnu99 -Wall

target: mofo

mofo: monoforth.c ; $(CC) $(CFLAGS) -o$@ monoforth.c ~/pro/blt/blt.c -I ~/pro/blt -lreadline -lgmp
