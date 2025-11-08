/*
*  license and disclaimer for the use of this source code as per statement below
*  Lizenz und Haftungsausschluss für die Verwendung dieses Sourcecodes siehe unten
*/

#include "usb_hub.h"
#include "usb_hc.h"
#include "usb.h"
#include "kheap.h"
#include "video/console.h"
#include "timer.h"


bool usb_getHubDescriptor(usb_device_t* device)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: GET_HUB_DESCRIPTOR");
    textColor(TEXT);
  #endif

    usb_hubDescriptor_t* hd = malloc(sizeof(usb_hubDescriptor_t), 0, "usb_hubDescriptor");
    device->data = hd;

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0xA0, 6, 0x29, 0, 0, 9); // auf 9 byte gefixt
    usb_inTransaction(&transfer, false, hd, 9);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        printf("\nNumber of Ports:  %u\n", hd->numberPorts);
    }
    else
    {
        textColor(ERROR);
        printf("\nTransfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

void usb_setupHub(usb_device_t* device)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: SETUP_HUB");
    textColor(TEXT);
  #endif

    usb_getHubStatus(device);

    for (uint8_t j=1; j<=((usb_hubDescriptor_t*)device->data)->numberPorts; j++)
    {
        usb_getPortStatus(device, j);
        waitForKeyStroke();

        uint8_t feature = 8; // Table 11-17 Power
        usb_setPortFeature(device, feature, 0, j);

        usb_getPortStatus(device, j);
        waitForKeyStroke();

        feature = 4; // Table 11-17 Reset
        usb_setPortFeature(device, feature, 0, j);
        sleepMilliSeconds(100);

        usb_getPortStatus(device, j);
        waitForKeyStroke();

        uint8_t num = hc_addPort(device);
        waitForKeyStroke();

        //Enumerate enabled
        if (usb_isHubPortEnabled(device, j))
        {
          #ifdef _USB_HUB_DIAGNOSIS_
            printf("\nEnumerating now device at port %u as: %u\n", j, num);
          #endif
            hc_setupUSBDevice(((hc_port_t*)device->port->data)->hc, num);
        }
    }
}

void usb_destroyHub(usb_device_t* device)
{
    free(device->data);
}

bool usb_clearHubFeature(usb_device_t* device, uint8_t feature)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: ClearHubFeature");
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x20, 1, 0, feature, 0, 0); // check hi/lo val
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        //...
    }
    else
    {
        textColor(ERROR);
        printf("\nClearHubFeature: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

bool usb_clearPortFeature(usb_device_t* device, uint8_t feature, uint8_t port) // The port number must be a valid port number for that hub, greater than zero.
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: ClearPortFeature");
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x23, 1, 0, feature, port, 0); // check hi/lo val and wIndex
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        //...
    }
    else
    {
        textColor(ERROR);
        printf("\nClearPortFeature: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

bool usb_clearTTBuffer(usb_device_t* device, uint8_t port)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: ClearTTBuffer");
    textColor(TEXT);
  #endif
    /*
    If the hub supports a TT per port, then wIndex must specify the port number of the TT that encountered the high-speed errors (e.g., with the busy TT buffer).
    If the hub provides only a single TT, then wIndex must be set to one.
    */

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0xA0, 6, device->num, 0/*endpoint*/, port, 0);
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        //...
    }
    else
    {
        textColor(ERROR);
        printf("\nClearTTBuffer: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

bool usb_getHubStatus(usb_device_t* device)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: GetHubStatus");
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0xA0, 0, 0, 0, 0, 4);
    usb_inTransaction(&transfer, false, &((usb_hubDescriptor_t*)device->data)->hubStatusFieldandHubChangeField, 4);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
      #ifdef _USB_HUB_DIAGNOSIS_
        printf("\nStatus/Change: LocalPowerSource: %u, Over-current condition: %u,\nLPS change: %u, OC change: %u",
            ((usb_hubDescriptor_t*)device->data)->hubStatusFieldandHubChangeField.localPowerSource,
            ((usb_hubDescriptor_t*)device->data)->hubStatusFieldandHubChangeField.overCurrent,
            ((usb_hubDescriptor_t*)device->data)->hubStatusFieldandHubChangeField.localPowerStatusChange,
            ((usb_hubDescriptor_t*)device->data)->hubStatusFieldandHubChangeField.overCurrentChange);
      #endif
    }
    else
    {
        textColor(ERROR);
        printf("\nGetHubStatus: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

bool usb_getPortStatus(usb_device_t* device, uint8_t port)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: GetPortStatus");
    textColor(TEXT);
  #endif

    portStatusAndChange_t portStatusAndChange;

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0xA3, 0, 0, 0, port, 4);
    usb_inTransaction(&transfer, false, &portStatusAndChange, 4);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
      #ifdef _USB_HUB_DIAGNOSIS_
        printf("\nHubPort: %u: enabled: %u, connected: %u LS: %u HS: %u",
            port, portStatusAndChange.portEnabledDisabled, portStatusAndChange.currentConnectStatus,
            portStatusAndChange.lowSpeedDevAttached, portStatusAndChange.hiSpeedDevAttached);
      #endif
    }
    else
    {
        textColor(ERROR);
        printf("\nGetPortStatus: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

bool usb_isHubPortEnabled(usb_device_t* device, uint8_t port)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: isHubPortEnabled");
    textColor(TEXT);
  #endif

    portStatusAndChange_t portStatusAndChange;

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0xA3, 0, 0, 0, port, 4);
    usb_inTransaction(&transfer, false, &portStatusAndChange, 4);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        return (bool)portStatusAndChange.portEnabledDisabled;
    }
    else
    {
        textColor(ERROR);
        printf("\nisHubPortEnabled - GetPortStatus: Transfer not successful!");
        textColor(TEXT);
    }
    return false;
}

bool usb_getTTState(usb_device_t* device, uint8_t TT_flags, uint8_t port) // for vendor specific Debugging
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: GetTTState");
    textColor(TEXT);
  #endif
    /*
    The TT_Flags bits 7..0 are reserved for future USB definition and must be set to zero. <== loVal
    The TT_Flags bits 15..8 are for vendor specific usage. <== hiVal
    */

    uint32_t TTstate = 0; // Bits 15..0 are reserved for future USB definition and must be set to zero.
                          // Bits 31..16 are for vendor specific usage.
    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0xA3, 10, TT_flags, 0, port, 4);
    usb_inTransaction(&transfer, false, &TTstate, 4);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        //...
    }
    else
    {
        textColor(ERROR);
        printf("\nGetTTState: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

bool usb_resetTT(usb_device_t* device, uint8_t port)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: ResetTT");
    textColor(TEXT);
  #endif
    /*
    If the hub supports multiple TTs, then wIndex must specify the port number of the TT that is to be reset.
    If the hub provides only a single TT, then Port must be set to one.
    For a single TT Hub, the Hub can ignore the Port number.
    */
    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x23, 9, 0, 0, port, 0);
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        //...
    }
    else
    {
        textColor(ERROR);
        printf("\nResetTT: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

bool usb_setHubDescriptor(usb_device_t* device, uint8_t* hdData, uint8_t hdLength) // write hd in once
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: SetHubDescriptor");
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x20, 7, 0x29, 0, 0, hdLength);
    usb_outTransaction(&transfer, true, hdData, hdLength); // in out OK?
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        //...
    }
    else
    {
        textColor(ERROR);
        printf("\nSetHubDescriptor: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

bool usb_setHubFeature(usb_device_t* device, uint8_t feature) // refer to Table 11-17 for the feature selector definitions
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: SetHubFeature");
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x20, 3, 0, feature, 0, 0);
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        //...
    }
    else
    {
        textColor(ERROR);
        printf("\nSetHubFeature: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}

bool usb_setPortFeature(usb_device_t* device, uint8_t feature, uint8_t selector, uint8_t port)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: SetPortFeature");
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x23, 3, 0, feature, selector << 8 | port, 0);
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        textColor(LIGHT_GRAY);
        printf("\nSetPortFeature: Transfer successful! Hub-Port: %u Feature: %u", port, feature);
        textColor(TEXT);
    }
    else
    {
        textColor(ERROR);
        printf("\nSetPortFeature: Transfer not successful! Feature: %u", feature);
        textColor(TEXT);
    }

    return (transfer.success);
}



bool usb_stopTT(usb_device_t* device, uint8_t port) // Restart a TT after a Stop_TT request via the Reset_TT request.
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: StopTT");
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x23, 11, 0, 0, port, 0);
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        //...
    }
    else
    {
        textColor(ERROR);
        printf("\nStopTT: Transfer not successful!");
        textColor(TEXT);
    }

    return (transfer.success);
}



/*
* Copyright (c) 2013-2014 The PrettyOS Project. All rights reserved.
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
