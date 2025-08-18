ca65 main.asm
ca65 pently\pentlysound.s
ca65 pently\pentlymusic.s
ca65 pently\musicseq.s
ca65 pently\ntscPeriods.s
ld65 main.o pentlysound.o pentlymusic.o musicseq.o ntscPeriod.o -o main.nes -t nes
.\main.nes