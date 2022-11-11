
diego.prg: diego.asm include/bios.inc include/kernel.inc
	asm02 -L -b diego.asm

clean:
	-rm -f diego.lst
	-rm -f diego.bin

