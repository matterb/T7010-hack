#ifndef USB_HUB_H
#define USB_HUB_H

#include "os.h"
#include "usb.h"


typedef struct // cf. 11.23.2.1 Hub Descriptor, Table 11-13
{
    // The hub descriptor itself (7 stable and 2 or more variable fields)
    uint8_t  length;            // descriptor length
    uint8_t  descriptorType;    // descriptor type, value: 29H for hub descriptor
    uint8_t  numberPorts;       // number of ports this hub supports

    uint16_t logicalPowerSwitchingMode  :  2;
    uint16_t compoundDevice             :  1;
    uint16_t overCurrentProtectionMode  :  2;
    uint16_t ttThinkTime                :  2;
    uint16_t portIndicatorsSupported    :  1;
    uint16_t reserved                   :  8;

    uint8_t  pwrOn2PwrGood;
    uint8_t  hubControllerCurrent;

    uint8_t  deviceRemovable; // bit0 = reserved, bit1 = port1, bit2 = port2, ..., bit7 = port7
    uint8_t  variableUse;     // with more than 7 ports logic goes on; otherwise PortPwrCtrlMask

    // Hub Status Field and Hub Change Field // Tables 11-19 and 11-20
    struct
    {
        uint32_t localPowerSource           :  1;
        uint32_t overCurrent                :  1;
        uint32_t reserved1                  : 14;
        uint32_t localPowerStatusChange     :  1;
        uint32_t overCurrentChange          :  1;
        uint32_t reserved2                  : 14;
    } __attribute__((packed)) hubStatusFieldandHubChangeField;
} __attribute__((packed)) usb_hubDescriptor_t;

typedef struct
{
    // status
    uint32_t currentConnectStatus      :  1; // bit0
    uint32_t portEnabledDisabled       :  1; // bit1
    uint32_t suspend                   :  1; // bit2
    uint32_t overCurrent               :  1; // bit3
    uint32_t reset                     :  1; // bit4
    uint32_t reserved1                 :  3; // bit5-7
    uint32_t portPower                 :  1; // bit8
    uint32_t lowSpeedDevAttached       :  1; // bit9  // 0=fullspeed 1=lowspeed
    uint32_t hiSpeedDevAttached        :  1; // bit10 // 0=fullspeed 1=hispeed
    uint32_t portTestMode              :  1; // bit11
    uint32_t portIndicatorControl      :  1; // bit12
    uint32_t reserved2                 :  3; // bit13-15

    //change
    uint32_t connectStatusChange       :  1; // bit0
    uint32_t portEnableDisableChange   :  1; // bit1
    uint32_t suspendChange             :  1; // bit2
    uint32_t overCurrentChange         :  1; // bit3
    uint32_t resetChange               :  1; // bit4
    uint32_t reserved3                 : 11; // bit5-15
} __attribute__((packed)) portStatusAndChange_t;


// usb2.0 spec - 11.24.2 Class-specific Requests
// The hub class defines requests to which hubs respond, cf. table 11-15
bool usb_getHubDescriptor(usb_device_t* device);
void usb_setupHub(usb_device_t* device);
void usb_destroyHub(usb_device_t* device);
void usb_destroyHub(usb_device_t* device);

bool usb_clearHubFeature(usb_device_t* device, uint8_t feature);
bool usb_clearPortFeature(usb_device_t* device, uint8_t feature, uint8_t port);
bool usb_clearTTBuffer(usb_device_t* device, uint8_t port);
bool usb_getHubStatus(usb_device_t* device);
bool usb_getPortStatus(usb_device_t* device, uint8_t port);
bool usb_isHubPortEnabled(usb_device_t* device, uint8_t port);
bool usb_getTTState(usb_device_t* device, uint8_t TT_flags, uint8_t port);
bool usb_resetTT(usb_device_t* device, uint8_t port);
bool usb_setHubDescriptor(usb_device_t* device, uint8_t* hdData, uint8_t hdLength);
bool usb_setHubFeature(usb_device_t* device, uint8_t feature);
bool usb_setPortFeature(usb_device_t* device, uint8_t feature, uint8_t selector, uint8_t port);
bool usb_stopTT(usb_device_t* device, uint8_t port);


#endif
