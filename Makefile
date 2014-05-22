CC := gcc
CFLAGS := -O2 --std=gnu99 -Wall

target: ; $(CC) $(CFLAGS) mono.c ~/pro/blt/blt.c -I ~/pro/blt -lreadline -lgmp
