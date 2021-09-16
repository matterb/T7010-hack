PAYLOAD_DIR := /home/bannisp/src/coreboot/payloads/camelforth
LIBPAYLOAD_DIR := /home/bannisp/src/coreboot/payloads/libpayload/install/libpayload
SRC_DIR := /home/bannisp/src/camelforth/camel_x86
LPGCC := $(LIBPAYLOAD_DIR)/bin/lpgcc
LPAS := $(LIBPAYLOAD_DIR)/bin/lpas
CFLAGS := -Wall -Werror -Os
SOURCES := camel86_32_1.0.S camel86_32d_1.0.S camel86_32h_1.0.S camel86_32o_1.0.S support.c Makefile

#CFLAGS=
#LDFLAGS=
#ASFLAGS=
#clean:
# 
#link:
#	ld $(LDFLAGS) -o  $(SOURCES)
#.S.o:
#	gcc  $(ASFLAGS) $< 

all: camelforth.elf camel86_32

# camel86_32: camel86_32.S camel86_32d.S camel86_32h.S camel86_32o.S Makefile
#	gcc -g -m32 -Wa,-aclmns=camel86_32.lst -nostdlib -static -Wl,-Map=camel86_32.map   -Wl,--build-id=none  -Wl,--cref -o camel86_32  camel86_32.S
#	gcc -ggdb  -m32 -Wa,-aclmns=camel86_32.lst -nostdlib -static -Wl,-Map=camel86_32.map   -Wl,--build-id=none  -Wl,--cref -o camel86_32  camel86_32.S

camel86_32: camel86_32_1.0.S camel86_32d_1.0.S camel86_32h_1.0.S camel86_32o_1.0.S Makefile support.c
	gcc -DLINUX  -g -m32 -Wa,-aclmns=camel86_32.lst   -Wl,-Map=camel86_32.map   -Wl,--build-id=none  -Wl,--cref -o camel86_32  camel86_32_1.0.S support.c  


camelforth.elf: $(SOURCES)
	$(LPGCC) -DCOREBOOT -c -o camelforth.o ${SRC_DIR}/camel86_32_1.0.S
	$(LPGCC) -DCOREBOOT -c -o support.o support.c 
	$(LPGCC) -DCOREBOOT -o camelforth.elf camelforth.o support.o 
	strip camelforth.elf

support.o:	support.c
	$(LPGCC) -c -o support.o  support.c

	
archive: all
	tar -zcvf camel86_32_linux.tgz ./camel86_32*.S ./Makefile ./glossary* ./support.c ./misc ./test   README BUGS


clean:
	rm -f camelforth.elf camel86_32 *.o

