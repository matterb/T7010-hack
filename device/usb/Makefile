# init-struct defined in dev-info and edtd

SRCS=preamble.fth
# Generic HCD stuff
SRCS += pkt-data.fth # USB packet definitions
SRCS += pkt-func.fth # 
SRCS += hcd.fth # Common HCD methods
SRCS += error.fth # Common HCD error manipulations
SRCS += dev-info.fth # Common internal device info

# OHCI HCD stuff
SRCS += edtd.fth # OHCI HCCA, ED & TD manipulations
SRCS += ohci.fth # OHCI methods FIXME check original!!!!
SRCS += control-ohci.fth # OHCI control pipe operations
SRCS += bulk.fth # OHCI bulk pipes operations
SRCS += intr.fth # OHCI interrupt pipes operations
SRCS += control-hcd.fth # Common control pipe API
# OHCI usb bus probing stuff
SRCS += device.fth # Make child node and its properties
SRCS += probe.fth # Probe root hub

all: ${SRCS}
	cat ${SRCS} > usb-ohci.fth


