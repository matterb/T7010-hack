/*
*  license and disclaimer for the use of this source code as per statement below
*  Lizenz und Haftungsausschluss für die Verwendung dieses Sourcecodes siehe unten
*/

#include "usb_hc.h"
#include "uhci.h"
#include "ohci.h"
#include "ehci.h"
#include "xhci.h"
#include "video/console.h"
#include "kheap.h"
#include "util/util.h"


void usb_hc_install(pciDev_t* PCIdev)
{
  #ifdef _HCI_DIAGNOSIS_
    printf("- USB ");

    switch (PCIdev->interfaceID)
    {
        case UHCI:
            printf("UHCI ");
            break;
        case OHCI:
            printf("OHCI ");
            break;
        case EHCI:
            printf("EHCI ");
            break;
        case XHCI:
            printf("xHCI ");
            break;
        case NO_HCI:
            printf("no HCI ");
            break;
        case ANY_HCI:
            printf("any ");
            break;
    }
  #endif

    for (uint8_t i=0; i < 6; i++) // check BARs
    {
      #ifdef _HCI_DIAGNOSIS_
        switch (PCIdev->bar[i].memoryType)
        {
            case PCI_MMIO:
                printf(" %Xh MMIO", PCIdev->bar[i].baseAddress & 0xFFFFFFF0);
                break;
            case PCI_IO:
                printf(" %xh I/O",  PCIdev->bar[i].baseAddress & 0xFFFC);
                break;
        }
      #endif

        if (PCIdev->bar[i].memoryType != PCI_INVALIDBAR)
        {
          #ifdef _HCI_DIAGNOSIS_
            printf(" sz: %u", PCIdev->bar[i].memorySize);
          #endif

            switch (PCIdev->interfaceID)
            {
                case XHCI:
                  #ifdef _XHCI_ENABLE_
                    xhci_install(PCIdev, PCIdev->bar[i].baseAddress & 0xFFFFFFF0);
                  #endif
                    break;
                case EHCI:
                  #ifdef _EHCI_ENABLE_
                    ehci_install(PCIdev, PCIdev->bar[i].baseAddress & 0xFFFFFFF0);
                  #endif
                    break;
                case OHCI:
                  #ifdef _OHCI_ENABLE_
                    ohci_install(PCIdev, PCIdev->bar[i].baseAddress & 0xFFFFFFF0, PCIdev->bar[i].memorySize);
                  #endif
                    break;
                case UHCI:
                  #ifdef _UHCI_ENABLE_
                    uhci_install(PCIdev, PCIdev->bar[i].baseAddress & 0xFFFFFFF0, PCIdev->bar[i].memorySize);
                  #endif
                    break;
            }
        }
    }
}


void usb_setupTransfer(usb_device_t* usbDevice, usb_transfer_t* transfer, usb_transferType_t type, uint32_t endpoint, size_t maxLength)
{
    transfer->device       = usbDevice;
    transfer->endpoint     = endpoint;
    transfer->type         = type;
    transfer->packetSize   = min(maxLength, usbDevice->endpoints[endpoint].mps);
    transfer->success      = false;
    list_construct(&transfer->transactions);

    if (transfer->device->port->type == &USB_XHCI)
    {
        xhci_setupTransfer(transfer);
    }
    else if (transfer->device->port->type == &USB_EHCI)
    {
        ehci_setupTransfer(transfer);
    }
    else if (transfer->device->port->type == &USB_OHCI)
    {
        ohci_setupTransfer(transfer);
    }
    else if (transfer->device->port->type == &USB_UHCI)
    {
        uhci_setupTransfer(transfer);
    }
    else
    {
        printf("\nusb_setupTransfer - Unknown port type: Xfer: %X, dev: %X, port: %X, type: %X",
            transfer, transfer->device, transfer->device->port, transfer->device->port->type);
    }
}

void usb_setupTransaction(usb_transfer_t* transfer, uint32_t tokenBytes, uint8_t type, uint8_t req, uint8_t hiVal, uint8_t loVal, uint16_t index, uint16_t length)
{
    usb_transaction_t* transaction = malloc(sizeof(usb_transaction_t), 0, "usb_transaction_t");
    transaction->type = USB_TT_SETUP;

    if (transfer->device->port->type == &USB_XHCI)
    {
        xhci_setupTransaction(transfer, transaction, false, tokenBytes, type, req, hiVal, loVal, index, length);
    }
    else if (transfer->device->port->type == &USB_EHCI)
    {
        ehci_setupTransaction(transfer, transaction, false, tokenBytes, type, req, hiVal, loVal, index, length);
    }
    else if (transfer->device->port->type == &USB_OHCI)
    {
        ohci_setupTransaction(transfer, transaction, false, tokenBytes, type, req, hiVal, loVal, index, length);
    }
    else if (transfer->device->port->type == &USB_UHCI)
    {
        uhci_setupTransaction(transfer, transaction, false, tokenBytes, type, req, hiVal, loVal, index, length);
    }
    else
    {
        printf("\nusb_setupTransaction - Unknown port type: Xfer: %X, dev: %X, port: %X, type: %X",
            transfer, transfer->device, transfer->device->port, transfer->device->port->type);
    }

    list_append(&transfer->transactions, transaction);

    transfer->device->endpoints[transfer->endpoint].toggle = true;
}

void usb_inTransaction(usb_transfer_t* transfer, bool controlHandshake, void* buffer, size_t length)
{
    size_t clampedLength = min(transfer->packetSize, length);
    length -= clampedLength;
    uint16_t remainingTransactions = length / transfer->packetSize;
    if (length % transfer->packetSize != 0)
        remainingTransactions++;

    usb_transaction_t* transaction = malloc(sizeof(usb_transaction_t), 0, "usb_transaction_t");
    transaction->type = USB_TT_IN;

    if (controlHandshake) // Handshake transaction of control transfers have always set toggle to 1
    {
        transfer->device->endpoints[transfer->endpoint].toggle = true;
    }

    if (transfer->device->port->type == &USB_XHCI)
    {
        xhci_inTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength, remainingTransactions);
    }
    else if (transfer->device->port->type == &USB_EHCI)
    {
        ehci_inTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength);
    }
    else if (transfer->device->port->type == &USB_OHCI)
    {
        ohci_inTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength);
    }
    else if (transfer->device->port->type == &USB_UHCI)
    {
        uhci_inTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength);
    }
    else
    {
        printf("\nusb_inTransaction - Unknown port type: Xfer: %X, dev: %X, port: %X, type: %X",
            transfer, transfer->device, transfer->device->port, transfer->device->port->type);
    }

    list_append(&transfer->transactions, transaction);

    transfer->device->endpoints[transfer->endpoint].toggle = !transfer->device->endpoints[transfer->endpoint].toggle; // Switch toggle

    if (remainingTransactions > 0)
    {
        usb_inTransaction(transfer, transfer->device->endpoints[transfer->endpoint].toggle, buffer+clampedLength, length);
    }
}

void usb_outTransaction(usb_transfer_t* transfer, bool controlHandshake, void* buffer, size_t length)
{
    size_t clampedLength = min(transfer->packetSize, length);
    length -= clampedLength;
    uint16_t remainingTransactions = length / transfer->packetSize;
    if (length % transfer->packetSize != 0)
        remainingTransactions++;

    usb_transaction_t* transaction = malloc(sizeof(usb_transaction_t), 0, "usb_transaction_t");
    transaction->type = USB_TT_OUT;

    if (controlHandshake) // Handshake transaction of control transfers have always set toggle to 1
    {
        transfer->device->endpoints[transfer->endpoint].toggle = true;
    }

    if (transfer->device->port->type == &USB_XHCI)
    {
        xhci_outTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength, remainingTransactions);
    }
    else if (transfer->device->port->type == &USB_EHCI)
    {
        ehci_outTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength);
    }
    else if (transfer->device->port->type == &USB_OHCI)
    {
        ohci_outTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength);
    }
    else if (transfer->device->port->type == &USB_UHCI)
    {
        uhci_outTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength);
    }
    else
    {
        printf("\nusb_outTransaction - Unknown port type: Xfer: %X, dev: %X, port: %X, type: %X",
            transfer, transfer->device, transfer->device->port, transfer->device->port->type);
    }

    list_append(&transfer->transactions, transaction);

    transfer->device->endpoints[transfer->endpoint].toggle = !transfer->device->endpoints[transfer->endpoint].toggle; // Switch toggle

    if (remainingTransactions > 0)
    {
        usb_outTransaction(transfer, transfer->device->endpoints[transfer->endpoint].toggle, buffer+clampedLength, length);
    }
}

void usb_issueTransfer(usb_transfer_t* transfer)
{
    if (transfer->device->port->type == &USB_XHCI)
    {
        xhci_issueTransfer(transfer);
    }
    else if (transfer->device->port->type == &USB_EHCI)
    {
        ehci_issueTransfer(transfer);
    }
    else if (transfer->device->port->type == &USB_OHCI)
    {
        ohci_issueTransfer(transfer);
    }
    else if (transfer->device->port->type == &USB_UHCI)
    {
        uhci_issueTransfer(transfer);
    }
    else
    {
        printf("\nusb_issueTransfer - Unknown port type: Xfer: %X, dev: %X, port: %X, type: %X",
            transfer, transfer->device, transfer->device->port, transfer->device->port->type);
    }

    for (dlelement_t* e = transfer->transactions.head; e != 0; e = e->next)
    {
        free(e->data);
    }

    list_destruct(&transfer->transactions);
}

void hc_setupUSBDevice(hc_t* hc, uint8_t portNumber)
{
    hc_port_t* port = hc_getPort(hc, portNumber);
    port->device = usb_createDevice(&port->port);
    usb_setupDevice(port->device, portNumber+1);
}

uint8_t hc_addPort(usb_device_t* usbDevice)
{
    hc_t* hc = ((hc_port_t*)usbDevice->port->data)->hc;
    hc_port_t* port = malloc(sizeof(hc_port_t), 0, "hc_port");
    uint8_t num = hc_aquirePort(hc, port);

    if (usbDevice->port->type == &USB_XHCI)
    {
        xhci_constructPort(port, hc, num);
    }
    else if (usbDevice->port->type == &USB_EHCI)
    {
        ehci_constructPort(port, hc, num);
    }
    else if (usbDevice->port->type == &USB_OHCI)
    {
        ohci_constructPort(port, hc, num);
    }
    else if (usbDevice->port->type == &USB_UHCI)
    {
        uhci_constructPort(port, hc, num);
    }
    else
    {
        printf("\nhc_addPort - Unknown port type");
    }
    return num;
}

hc_port_t* hc_getPort(hc_t* hc, uint8_t num)
{
    if (num < hc->rootPortCount)
        return hc->rootPorts+num;

    num -= hc->rootPortCount;
    dlelement_t* elem = hc->otherPorts.head;
    for (uint8_t i = 0; i < num && elem; i++)
        elem = elem->next;
    return elem->data;
}

uint8_t hc_aquirePort(hc_t* hc, void* data)
{
    // Find next free list element
    uint16_t i = hc->rootPortCount;
    for (dlelement_t* elem = hc->otherPorts.head; elem; elem = elem->next)
    {
        if (elem->data == 0)
        {
            elem->data = data;
            return i;
        }
        i++;
    }

    list_append(&hc->otherPorts, data);
    return i;
}


/*
* Copyright (c) 2010-2014 The PrettyOS Project. All rights reserved.
*
* http://www.c-plusplus.de/forum/viewforum-var-f-is-62.html
*
* Redistribution and use in source and binary forms, with or without modification,
* are permitted provided that the following conditions are met:
*
* 1. Redistributions of source code must retain the above copyright notice,
*    this list of conditions and the following disclaimer.
*
* 2. Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in the
*    documentation and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
* TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
* PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
* OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
* WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
* OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
* ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
