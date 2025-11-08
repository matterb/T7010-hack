#ifndef USB_HC_H
#define USB_HC_H

#include "usb.h"
#include "pci.h"

#define UHCI    0x00
#define OHCI    0x10
#define EHCI    0x20
#define XHCI    0x30
#define NO_HCI  0x80
#define ANY_HCI 0xFE


typedef enum
{
    USB_BULK, USB_CONTROL, USB_INTERRUPT, USB_ISOCHRONOUS
} usb_transferType_t;

typedef struct
{
    void*              data;
    usb_transferType_t type;
    uint32_t           endpoint;
    uint32_t           packetSize;
    usb_device_t*      device;
    list_t             transactions;
    bool               success;
} usb_transfer_t;

typedef enum
{
    USB_TT_SETUP, USB_TT_IN, USB_TT_OUT
} usb_transactionType_t;

typedef struct
{
    void*                 data; // Contains pointer to *hci_transaction_t
    usb_transactionType_t type;
} usb_transaction_t;


struct hc;

typedef struct
{
    port_t        port;
    struct hc*    hc;
    bool          connected;
    usb_device_t* device;
    void*         data;
} hc_port_t;

typedef struct hc
{
    hc_port_t*     rootPorts;         // root ports
    list_t         otherPorts;        // non-root ports
    uint8_t        rootPortCount;     // number of rootports
} hc_t;


void usb_hc_install(pciDev_t* PCIdev);

void usb_setupTransfer(usb_device_t* usbDevice, usb_transfer_t* transfer, usb_transferType_t type, uint32_t endpoint, size_t maxLength);
void usb_setupTransaction(usb_transfer_t* transfer, uint32_t tokenBytes, uint8_t type, uint8_t req, uint8_t hiVal, uint8_t loVal, uint16_t index, uint16_t length);
void usb_inTransaction(usb_transfer_t* transfer, bool controlHandshake, void* buffer, size_t length);
void usb_outTransaction(usb_transfer_t* transfer, bool controlHandshake, void* buffer, size_t length);
void usb_issueTransfer(usb_transfer_t* transfer);
void hc_setupUSBDevice(hc_t* hc, uint8_t portNumber);
uint8_t hc_addPort(usb_device_t* usbDevice);
hc_port_t* hc_getPort(hc_t* hc, uint8_t num);
uint8_t hc_aquirePort(hc_t* hc, void* data);


#endif
