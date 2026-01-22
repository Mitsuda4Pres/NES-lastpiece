ca65 -g main.asm
ld65 --dbgfile main.dbg main.o -o main.nes -t nes
.\main.nes