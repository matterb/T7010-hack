\ @@FILE:startup.fth
\ vocabulary lay out for thinclient
\ forth :- system,filesystem,memory,graphics,console,diagnostic
\ system :- gx1,pci,pnp,DOC,cs5530,usb,ide,serial,parport,spi,i2c
\ filesystem :- file-ids,ext2fs
\ memory :- init-memory,free,allocate,resize
\ graphics :- x-max y-max fg bg plot-xy line xnow ynow 
\ console :- at-xy 
\ diagnostic
 
only forth definitions
vocabulary system
vocabulary filesystem
vocabulary memory
vocabulary graphics
vocabulary console
vocabulary diagnostic
