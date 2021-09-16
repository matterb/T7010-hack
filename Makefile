#SOURCES=

#CFLAGS=
#LDFLAGS=
#ASFLAGS=

#all: $(SOURCES) link 

#link:
#	ld $(LDFLAGS) -o  $(SOURCES)

#.S.o:
#	gcc  $(ASFLAGS) $< 

all: camelforth.elf camel86_32

	
archive: ./runtime/all.fth 
	tar -zcvhf thinclient.tgz ./Makefile  ./apps  ./grph-dev ./lib ./runtime ./devices ./kernel ./forth ./ugui README


