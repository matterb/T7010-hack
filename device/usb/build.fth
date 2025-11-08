\ init-struct defined in dev-info and edtd

: pci-dev ;
: pci-w@ ;
: pci-w! ;

include util.fth
include preamble.fth
\ Generic HCD stuff
include pkt-data.fth      \ USB packet definitions
include pkt-func.fth
include hcd.fth           \ Common HCD methods
include error.fth         \ Common HCD error manipulations
include dev-info.fth      \ Common internal device info

\ OHCI HCD stuff
include edtd.fth      \ OHCI HCCA, ED & TD manipulations
include ohci.fth      \ OHCI methods FIXME check original!!!!
include control-ohci.fth    \ OHCI control pipe operations
include bulk.fth      \ OHCI bulk pipes operations
include intr.fth      \ OHCI interrupt pipes operations
include control-hcd.fth  \ Common control pipe API
\ OHCI usb bus probing stuff
include device.fth      \ Make child node and its properties
include probe.fth       \ Probe root hub


