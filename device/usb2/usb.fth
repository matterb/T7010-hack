\ usb debug flag

0 value debug-usb?


\  #define OUT   0
\  #define IN    1
\  #define SETUP 2


begin-structure /urb            \ usb request packet
    cfield: >urb-type
    cfield: >urb-request
    cfield: >urb-value-lo
    cfield: >urb-value-hi
    wfield: >urb-index
    wfield: >urb-length
end-structure

0 constant EP_OUT
1 constant EP_IN
2 constant EP_BIDIR

begin-structure /usb-endpoint
    wfield: >uep-mps
    cfield: >uep-toggle
    field:  >uep-type
    cfield: >uep-interval
end-structure

begin-structure /usb_device
    field:  >usb-port
    wfield: >usb-usb-spec
    cfield: >usb-class
    cfield: >usb-subclass
    cfield: >usb-protocol
    wfield: >usb-vendor
    wfield: >usb-product
    wfield: >usb-release-number
    cfield: >usb-manufacturer-string-id
    cfield: >usb-product-string-id
    cfield: >usb-serNumber-string-id
    cfield: >usb-num-configurations
    cfield: >usb-max-lun
    cfield: >usb-num
    field:  >usb-endpoints         \ array of endpoints
    cfield: >usb-interface-class
    cfield: >usb-interface-subclass
d# 16 chars
    +field  >usb-product-name
d# 13 chars
    +field  >usb-serial-number
    field:  >usb-data
\  MSD specific - TODO: Move out
    cfield: >usb-num-interface-msd
    cfield: >usb-num-endpoint-in-msd
    cfield: >usb-num-endpoint-out-msd
end-structure

begin-structure /usb_deviceDescriptor
    cfield: >udd-length         \ 18
    cfield: >udd-type \ 1
    wfield: >udd-bcd-usb         \ 0x0210 means 2.10
    cfield: >udd-class
    cfield: >udd-subclass
    cfield: >udd-protocol
    cfield: >udd-max-packet-size  \ MPS0, must be 8,16,32,64
    wfield: >udd-id-vendor       
    wfield: >udd-id-product
    wfield: >udd-bcd-device      \ release of the device
    cfield: >udd-manufacturer   \ 
    cfield: >udd-product
    cfield: >udd-serial-number
    cfield: >udd-num-configurations  \ number of possible configurations
end-structure

begin-structure /usb_configurationDescriptor
    cfield: >ucd-length             \ 9
    cfield: >ucd-type     \ 2
    wfield: >ucd-total-length
    cfield: >ucd-num-interfaces
    cfield: >ucd-configuration-value
    cfield: >ucd-configuration
    cfield: >ucd-attributes
    cfield: >ucd-max-power
end-structure

begin-structure /usb_interfaceDescriptor
    cfield: >uid-length         \ 9
    cfield: >uid-type \ 4
    cfield: >uid-interface-number 
    cfield: >uid-alternate-setting
    cfield: >uid-num-end-points
    cfield: >uid-class
    cfield: >uid-subclass
    cfield: >uid-protocol
    cfield: >uid-interface
end-structure

begin-structure /usb_endpointDescriptor
    cfield: >ued-length         \ 7
    cfield: >ued-type \ 5
    cfield: >ued-endpoint-address 
    cfield: >ued-attributes
    wfield: >ued-max-packet-size
    cfield: >ued-interval
end-structure

begin-structure /usb-stringDescriptor
    cfield: >usd-length
    cfield: >usd-type     \ 3
d# 20 chars
    +field >usd-language-id         \ 
end-structure

begin-structure /usb-stringDescriptorU
    cfield: >usdU-length         \ 2 + 2 * numUnicodeCharacters
    cfield: >usdU-type \ 3
d# 60 chars 
    +field  >usdU-widechar       \ n = 30 test-wise 
end-structure


: usb-create-device ( port - dev )
    /usb-device allocate throw   ( port dev)
    dup >usb-serialNumber d# 13 erase   ( dev)
    tuck >usb-port !
    /usb-endpoint allocate throw ( dev uep)
    over >usb-endpoints !       \ set up at least one ED
    dup >usb-endpoints @ dup >uep-mps d# 64 swap w! 
    dup >uep-type EP_BIDIR swap ! 
    dup >uep-toggle 0 swap c!
;

: usb-remove-device ( dev --)
    dup >usb-interface-class @ 8 = if  \ mass storage
        msd-remove
    then
    dup >usb-class @ 9 = if        \ hub
        hub-remove
    then
    dup >usb-endpoints @ endpoints-free throw
    free throw
;


: usb-get-device-descriptor ( dev -- flg )
  debug-usb? if
    cr cr ."USB: GET_DESCRIPTOR Device" cr
  then

  alloc-td
    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x80, 6, 1, 0, 0, 18);
    usb_inTransaction(&transfer, false, &descriptor, 18);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {
        analyzeDeviceDescriptor(&descriptor, device);
        showDevice(device);
        waitForKeyStroke();
    }

    return (transfer.success);
}


: usb-setup-device (device addr -- )
    over >usb-num 0 swap  c!       \ device number has to be set to 0
    over >usb-data 0 swap !

    over usb-get-device-descriptor 0= if 
        over usb-get-device-descriptor 0= if
            ." Setup Device interrupted!" cr 2drop exit
        then
    then

    over >usb-class @ 9 = ( dev addr hub?)
   device->num = usb_setDeviceAddress(device, address);
    printf("\nusb-Device address: %u", device->num);

    if (hub)
    {
        printf(" <-- usb Hub");
    }

    success = usb_getConfigDescriptor(device);
    if (!success)
    {
        success = usb_getConfigDescriptor(device);
    }

    if (!success)
    {
        textColor(ERROR);
        printf("\nConfigDescriptor could not be read! Setup Device interrupted!");
        textColor(TEXT);
        return;
    }

    if (!hub)
    {
        usb_getStringDescriptor(device);
        waitForKeyStroke();

        for (uint8_t i=1; i<4; i++) // fetch 3 strings
        {
            usb_getUnicodeStringDescriptor(device, i);
        }
    }

    usb_setConfiguration(device, 1); // set first configuration

  #ifdef _USB_DIAGNOSIS_
    uint8_t config = usb_getConfiguration(device);
    printf("\nconfiguration: %u", config); // check configuration
    waitForKeyStroke();
  #endif

    if (hub)
    {
        printf("\nThis is a hub.");
        success = usb_getHubDescriptor(device);
        if (success)
        {
            printf("\nThe hub owns %u downstream ports.", ((usb_hubDescriptor_t*)device->data)->numberPorts);
            usb_setupHub(device);
        }
        else
        {
            textColor(ERROR);
            printf("\nHubDescriptor could not be read!");
            textColor(TEXT);
        }
        waitForKeyStroke();
    }
    else if (device->InterfaceClass == 0x08)
    {
        usb_setupMSD(device);
    }
    else
    {
        textColor(ERROR);
        printf("\nThis is no Mass Storage Device! MSD test and addition to device manager will not be carried out.");
        textColor(TEXT);
        waitForKeyStroke();
    }
}

uint8_t usb_setDeviceAddress(usb_device_t* device, uint8_t num)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: SET_ADDRESS");
    textColor(TEXT);
  #endif

    uint8_t new_address = num; // indicated port number

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x00, 5, 0, new_address, 0, 0);
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(HEADLINE);
    printf("\nnew address: %u", new_address);
    textColor(TEXT);
    waitForKeyStroke();
  #endif

    return new_address;
}

bool usb_getConfigDescriptor(usb_device_t* device)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: GET_DESCRIPTOR Config");
    textColor(TEXT);
  #endif

    char buffer[64];

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x80, 6, 2, 0, 0, 64);
    usb_inTransaction(&transfer, false, buffer, 64);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    if (transfer.success)
    {

      #ifdef _USB_TRANSFER_DIAGNOSIS_
        textColor(LIGHT_GRAY);
        printf("\n---------------------------------------------------------------------\n");
        textColor(GREEN);
      #endif

        // parse to config (len=9,type=2), interface (len=9,type=4) or endpoint (len=7,type=5)
        void* addr     = buffer;
        void* lastByte = addr + (*(uint16_t*)(addr+2)); // totalLength (WORD)

      #ifdef _USB_DIAGNOSIS_
        memshow(buffer, *(uint16_t*)(addr+2), false);
        putch('\n');
      #endif

        uint16_t numEndpoints = 1;
        // First pass. Retrieve usb_interfaceDescriptor which contains the number of endpoints
        while ((uintptr_t)addr < (uintptr_t)lastByte && addr < (void*)(buffer + 64))
        {
            uint8_t type = *(uint8_t*)(addr + 1);
            uint8_t length = *(uint8_t*)addr;

            if (length == 9 && type == 2)
            {
                struct usb_configurationDescriptor* descriptor = addr;
                showConfigurationDescriptor(descriptor);
            }
            else if (length == 9 && type == 4)
            {
                struct usb_interfaceDescriptor* descriptor = addr;
                showInterfaceDescriptor(descriptor);

                if (descriptor->interfaceClass == 8)
                {
                    // store interface number for mass storage transfers
                    device->numInterfaceMSD = descriptor->interfaceNumber;
                    device->InterfaceClass = descriptor->interfaceClass;
                    device->InterfaceSubclass = descriptor->interfaceSubclass;
                }
                numEndpoints += descriptor->numEndpoints;
            }
            else if (length == 7 && type == 5)
            {
            }
            else
            {
              #ifdef _USB_TRANSFER_DIAGNOSIS_
                printf("\nlength: %u type: %u - unknown\n", length, type);
                break;
              #endif
            }
            addr += length;
        }//while

        usb_endpoint_t* newEPs = malloc(sizeof(usb_endpoint_t) * numEndpoints, 0, "usbDev->endpoints");
        memcpy(newEPs, device->endpoints, sizeof(usb_endpoint_t) * 1);
        memset(newEPs + 1, 0, sizeof(usb_endpoint_t) * (numEndpoints - 1));
        free(device->endpoints);
        device->endpoints = newEPs;

        // Second pass. Fill in endpoint information
        addr = buffer;
        lastByte = addr + (*(uint16_t*)(addr + 2));
        while ((uintptr_t)addr < (uintptr_t)lastByte && addr < (void*)(buffer + 64))
        {
            uint8_t type = *(uint8_t*)(addr + 1);
            uint8_t length = *(uint8_t*)addr;

            if (length == 7 && type == 5)
            {
                struct usb_endpointDescriptor* descriptor = addr;
                showEndpointDescriptor(descriptor);

                uint8_t ep_id = descriptor->endpointAddress & 0xF;
                ASSERT(ep_id < numEndpoints);

                device->endpoints[ep_id].mps = descriptor->maxPacketSize;
                device->endpoints[ep_id].type = EP_BIDIR; // Can be overwritten below
                device->endpoints[ep_id].interval = descriptor->interval;

                // store endpoint numbers for IN/OUT mass storage transfers, attributes must be 0x2, because there are also endpoints with attributes 0x3(interrupt)
                if (descriptor->endpointAddress & 0x80 && descriptor->attributes == 0x2)
                {
                    if (ep_id < 3)
                        device->numEndpointInMSD = ep_id;
                    device->endpoints[ep_id].type = EP_IN;
                }

                if (!(descriptor->endpointAddress & 0x80) && descriptor->attributes == 0x2)
                {
                    if (ep_id < 3)
                        device->numEndpointOutMSD = ep_id;
                    device->endpoints[ep_id].type = EP_OUT;
                }
            }

            addr += length;
        }//while
    }//if

    return (transfer.success);
}

void usb_getStringDescriptor(usb_device_t* device)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: GET_DESCRIPTOR string, dev: %X endpoint: 0 languageIDs", device);
    textColor(TEXT);
  #endif

    struct usb_stringDescriptor descriptor;

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x80, 6, 3, 0, 0, sizeof(descriptor));
    usb_inTransaction(&transfer, false, &descriptor, sizeof(descriptor));
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

  #ifdef _USB_TRANSFER_DIAGNOSIS_
    memshow(&descriptor, sizeof(descriptor), false);
    putch('\n');
  #endif
    showStringDescriptor(&descriptor);
}

void usb_getUnicodeStringDescriptor(usb_device_t* device, uint32_t stringIndex)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: GET_DESCRIPTOR string, dev: %X endpoint: 0 stringIndex: %u", device, stringIndex);
    textColor(TEXT);
  #endif

    char buffer[64];

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x80, 6, 3, stringIndex, 0x0409, 64);
    usb_inTransaction(&transfer, false, buffer, 64);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

  #ifdef _USB_TRANSFER_DIAGNOSIS_
    memshow(buffer, 64, false);
    putch('\n');
  #endif

    showUnicodeStringDescriptor((struct usb_stringDescriptorUnicode*)buffer, device, stringIndex);
}

// http://www.lowlevel.eu/wiki/USB#SET_CONFIGURATION
void usb_setConfiguration(usb_device_t* device, uint32_t configuration)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: SET_CONFIGURATION %u", configuration);
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x00, 9, 0, configuration, 0, 0); // SETUP DATA0, 8 byte, request type, SET_CONFIGURATION(9), hi(reserved), configuration, index=0, length=0
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);
}

uint8_t usb_getConfiguration(usb_device_t* device)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: GET_CONFIGURATION");
    textColor(TEXT);
  #endif

    char configuration;

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, 0, 64);
    usb_setupTransaction(&transfer, 8, 0x80, 8, 0, 0, 0, 1);
    usb_inTransaction(&transfer, false, &configuration, 1);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    return configuration;
}

// new control transfer as TEST /////////////////////////////////////////////////
// seems not to work correct, does not set HALT ???
void usb_setFeatureHALT(usb_device_t* device, uint32_t endpoint)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: usbSetFeatureHALT, endpoint: %u", endpoint);
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, endpoint, 64);
    usb_setupTransaction(&transfer, 8, 0x02, 3, 0, 0, endpoint, 0);
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

  #ifdef _USB_TRANSFER_DIAGNOSIS_
    printf("\nset HALT at dev: %X endpoint: %u", device, endpoint);
  #endif
}

void usb_clearFeatureHALT(usb_device_t* device, uint32_t endpoint)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: usbClearFeatureHALT, endpoint: %u", endpoint);
    textColor(TEXT);
  #endif

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, endpoint, 64);
    usb_setupTransaction(&transfer, 8, 0x02, 1, 0, 0, endpoint, 0);
    usb_inTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

  #ifdef _USB_DIAGNOSIS_
    printf("\nclear HALT at dev: %X endpoint: %u", device, endpoint);
  #endif
}

uint16_t usb_getStatus(usb_device_t* device, uint32_t endpoint)
{
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    textColor(LIGHT_CYAN);
    printf("\n\nUSB: usbGetStatus at device: %X endpoint: %u", device, endpoint);
    textColor(TEXT);
  #endif

    uint16_t status;

    usb_transfer_t transfer;
    usb_setupTransfer(device, &transfer, USB_CONTROL, endpoint, 64);
    usb_setupTransaction(&transfer, 8, 0x02, 0, 0, 0, endpoint, 2);
    usb_inTransaction(&transfer, false, &status, 2);
    usb_outTransaction(&transfer, true, 0, 0);
    usb_issueTransfer(&transfer);

    return status;
}

void analyzeDeviceDescriptor(struct usb_deviceDescriptor* d, usb_device_t* usbDev)
{
    usbDev->usbSpec              = d->bcdUSB;
    usbDev->usbClass             = d->deviceClass;
    usbDev->usbSubclass          = d->deviceSubclass;
    usbDev->usbProtocol          = d->deviceProtocol;
    usbDev->vendor               = d->idVendor;
    usbDev->product              = d->idProduct;
    usbDev->releaseNumber        = d->bcdDevice;
    usbDev->manufacturerStringID = d->manufacturer;
    usbDev->productStringID      = d->product;
    usbDev->serNumberStringID    = d->serialNumber;
    usbDev->numConfigurations    = d->numConfigurations;
    usbDev->endpoints[0].mps     = d->maxPacketSize;
}

static void showDevice(usb_device_t* usbDev)
{
    textColor(IMPORTANT);
    if (usbDev->usbSpec == 0x0100 || usbDev->usbSpec == 0x0110 || usbDev->usbSpec == 0x0200 || usbDev->usbSpec == 0x0201 || usbDev->usbSpec == 0x0210 || usbDev->usbSpec == 0x0213 ||usbDev->usbSpec == 0x0300)
    {
        textColor(SUCCESS);
        printf("\nUSB %y.%y\t", BYTE2(usbDev->usbSpec), BYTE1(usbDev->usbSpec)); // e.g. 0x0210 means 2.10
        textColor(TEXT);
    }
    else
    {
        textColor(ERROR);
        printf("\nInvalid USB version %y.%y!", BYTE2(usbDev->usbSpec), BYTE1(usbDev->usbSpec));
        textColor(TEXT);
        return;
    }

    if (usbDev->usbClass == 0x09)
    {
        switch (usbDev->usbProtocol)
        {
            case 0:
                printf(" - Full speed USB hub");
                break;
            case 1:
                printf(" - Hi-speed USB hub with single TT");
                break;
            case 2:
                printf(" - Hi-speed USB hub with multiple TTs");
                break;
        }
    }

    printf("\nendpoint 0 mps: %u byte.", usbDev->endpoints[0].mps); // MPS0, must be 8,16,32,64
  #ifdef _USB_TRANSFER_DIAGNOSIS_
    printf("vendor:            %xh\n",   usbDev->vendor);
    printf("product:           %xh\t",   usbDev->product);
    printf("release number:    %y.%y\n", BYTE2(usbDev->releaseNumber), BYTE1(usbDev->releaseNumber));
    printf("manufacturer:      %xh\t",   usbDev->manufacturerStringID);
    printf("product:           %xh\n",   usbDev->productStringID);
    printf("serial number:     %xh\t",   usbDev->serNumberStringID);
    printf("number of config.: %u\n",    usbDev->numConfigurations); // number of possible configurations
    printf("numInterfaceMSD:   %u\n",    usbDev->numInterfaceMSD);
  #endif
    textColor(TEXT);
}

static void showConfigurationDescriptor(struct usb_configurationDescriptor* d)
{
    if (d->length)
    {
      #ifdef _USB_TRANSFER_DIAGNOSIS_
        textColor(IMPORTANT);
        printf("length:               %u\t\t", d->length);
        printf("descriptor type:      %u\n", d->descriptorType);
        textColor(LIGHT_GRAY);
        printf("total length:         %u\t", d->totalLength);
      #endif
        textColor(IMPORTANT);
        printf("\nNumber of interfaces: %u", d->numInterfaces);
      #ifdef _USB_TRANSFER_DIAGNOSIS_
        printf("ID of config:         %xh\t", d->configurationValue);
        printf("ID of config name     %xh\n", d->configuration);
        printf("remote wakeup:        %s\t", d->attributes & BIT(5) ? "yes" : "no");
        printf("self-powered:         %s\n", d->attributes & BIT(6) ? "yes" : "no");
        printf("max power (mA):       %u\n", d->maxPower*2); // 2 mA steps used
      #endif
        textColor(TEXT);
    }
}

static void showInterfaceDescriptor(struct usb_interfaceDescriptor* d)
{
    if (d->length)
    {
        putch('\n');
      #ifdef _USB_TRANSFER_DIAGNOSIS_
        textColor(LIGHT_GRAY);
        printf("---------------------------------------------------------------------\n");
      #endif
        textColor(IMPORTANT);
      #ifdef _USB_TRANSFER_DIAGNOSIS_
        printf("length:               %u\t\t", d->length);          // 9
        printf("descriptor type:      %u\n",   d->descriptorType);  // 4
      #endif
        switch (d->numEndpoints)
        {
            case 0:
                printf("Interface %u has no endpoint and belongs to class:\n", d->interfaceNumber);
                break;
            case 1:
                printf("Interface %u has only one endpoint and belongs to class:\n", d->interfaceNumber);
                break;
            default:
                printf("Interface %u has %u endpoints and belongs to class:\n", d->interfaceNumber, d->numEndpoints);
                break;
        }

        switch (d->interfaceClass)
        {
            case 0x01:
                printf("Audio");
                break;
            case 0x02:
                printf("Communications and CDC Control");
                break;
            case 0x03:
                printf("HID (Human Interface Device)");
                break;
            case 0x05:
                printf("Physical");
                break;
            case 0x06:
                printf("Image");
                break;
            case 0x07:
                printf("Printer");
                break;
            case 0x08:
                printf("Mass Storage, ");
                switch (d->interfaceSubclass)
                {
                    case 0x01:
                        printf("Reduced Block Commands, ");
                        break;
                    case 0x02:
                        printf("SFF-8020i or MMC-2(ATAPI), ");
                        break;
                    case 0x03:
                        printf("QIC-157 (tape device), ");
                        break;
                    case 0x04:
                        printf("UFI (e.g. Floppy Disk), ");
                        break;
                    case 0x05:
                        printf("SFF-8070i (e.g. Floppy Disk), ");
                        break;
                    case 0x06:
                        printf("SCSI transparent command set, ");
                        break;
                }
                switch (d->interfaceProtocol)
                {
                    case 0x00:
                        printf("CBI protocol with command completion interrupt.");
                        break;
                    case 0x01:
                        printf("CBI protocol without command completion interrupt.");
                        break;
                    case 0x50:
                        printf("Bulk-Only Transport protocol.");
                        break;
                }
                break;
            case 0x0A:
                printf("CDC-Data");
                break;
            case 0x0B:
                printf("Smart Card");
                break;
            case 0x0D:
                printf("Content Security");
                break;
            case 0x0E:
                printf("Video");
                break;
            case 0x0F:
                printf("Personal Healthcare");
                break;
            case 0xDC:
                printf("Diagnostic Device");
                break;
            case 0xE0:
                printf("Wireless Controller, subclass: %yh protocol: %yh.",d->interfaceSubclass,d->interfaceProtocol);
                break;
            case 0xEF:
                printf("Miscellaneous");
                break;
            case 0xFE:
                printf("Application Specific");
                break;
            case 0xFF:
                printf("Vendor Specific");
                break;
        }

      #ifdef _USB_TRANSFER_DIAGNOSIS_
        printf("\nalternate Setting:  %u\n",  d->alternateSetting);
        printf("interface class:      %u\n",  d->interfaceClass);
        printf("interface subclass:   %u\n",  d->interfaceSubclass);
        printf("interface protocol:   %u\n",  d->interfaceProtocol);
        printf("interface:            %xh\n", d->interface);
      #endif

        textColor(TEXT);
    }
}

: .endpoint-descriptor ( ued -- )
    dup >ued-length @ ?dup if
        ."length: "   u. cr dup >ued-descriptorType @
        ."descriptor type: " u. cr dup ued>endpointAddress @
        dup h# 80 and swap h# 0f and
        ."endpoint " u. [char] , emit if ." IN" else ." out" cr
        printf("attributes: %yh\t",     d->attributes);
        // bit 1:0 00 control    01 isochronous    10 bulk                         11 interrupt
        // bit 3:2 00 no sync    01 async          10 adaptive                     11 sync (only if isochronous)
        // bit 5:4 00 data endp. 01 feedback endp. 10 explicit feedback data endp. 11 reserved (Iso Mode)

        if (d->attributes == 2)
        {
            printf("\nbulk data,");
        }
        printf(" mps: %u byte",  d->maxPacketSize);
        printf(" interval: %u\n",  d->interval);
        textColor(TEXT);
    }
;

0 [if] \ Language Codes
\ build this as an arrary of strings
 0x400 Neutral
 0x401 Arabic
 0x402 Bulgarian
 0x403 Catalan
 0x404 Chinese
 0x405 Czech
0x406 Danish
0x407 German
0x408 Greek
                        ; 0x409 English
                        ; 0x40a Spanish
                        ; 0x40b Finnish
                        ; 0x40c French
                        ; 0x40d Hebrew
                        ; 0x40e Hungarian
                        ; 0x40f Icelandic
                        ; 0x410 Italian
                        ; 0x411 Japanese
                        ; 0x412 Korean
                        ; 0x413 Dutch
                        ; 0x414 Norwegian
                        ; 0x415 Polish
                        ; 0x416 Portuguese
                        ; 0x418 Romanian
                        ; 0x419 Russian
                        ; 0x41a Croatian
                        ; 0x41a Serbian
                        ; 0x41b Slovak
                        ; 0x41c Albanian
                        ; 0x41d Swedish
                        ; 0x41e Thai
                        ; 0x41f Turkish
                        ; 0x420 Urdu
                        ; 0x421 Indonesian
                        ; 0x422 Ukrainian
                        ; 0x423 Belarusian
                        ; 0x424 Slovenian
                        ; 0x425 Estonian
                        ; 0x426 Latvian
                        ; 0x427 Lithuanian
                        ; 0x429 Farsi
                        ; 0x42a Vietnamese
                        ; 0x42b Armenian
                        ; 0x42c Azeri
                        ; 0x42d Basque
                        ; 0x42f Macedonian
                        ; 0x436 Afrikaans
                        ; 0x437 Georgian
                        ; 0x438 Faeroese
                        ; 0x439 Hindi
                        ; 0x43e Malay
                        ; 0x43f Kazak
                        ; 0x440 Kyrgyz
                        ; 0x441 Swahili
                        ; 0x443 Uzbek
                        ; 0x444 Tatar
                        ; 0x446 Punjabi
                        ; 0x447 Gujarati
                        ; 0x449 Tamil
                        ; 0x44a Telugu
                        ; 0x44b Kannada
                        ; 0x44e Marathi
                        ; 0x44f Sanskrit
                        ; 0x450 Mongolian
                        ; 0x456 Galician
                        ; 0x457 Konkani
                        ; 0x45a Syriac
                        ; 0x465 Divehi 
[then]


: .string-Descriptor ( usd --)
    dup >usd-length c@ if
        cr cr ."languages: "
        d# 10 0 do
           dup >usd-languageID w@ i cells + @ dup h# 400 h# 465 between if
                case 
                    h# 401 of ." "Arabic" endof
                    h# 404 of ."Chinese" endof
                    case 0x407: printf("German");
                    case 0x409: printf("English");
                    case 0x40A: printf("Spanish");
                    case 0x40C: printf("French");
                    case 0x410: printf("Italian");
                    case 0x411: printf("Japanese");
                    case 0x416: printf("Portuguese");
                    case 0x419: printf("Russian");
                        ( default ) ."language code: " dup u.
                 endcase
            cr
         loop
    then
;

: .unicode-string-descriptor ( dev usdU idx -- )
    over >usdU-length @ if 
        
        printf("\nlength:          %u\t", d->length);
        printf("\tdescriptor type: %u", d->descriptorType);
        printf("\nstring:          ");
        
        char asciichar[31] = {0};

        for (uint8_t i=0; i<min(60, (d->length-2)); i+=2) // show only low value of Unicode character
        {
            if (d->widechar[i])
            {
              #ifdef _USB_TRANSFER_DIAGNOSIS_
                putch(d->widechar[i]);
              #endif
                asciichar[i/2] = d->widechar[i];
            }
        }
        if (stringIndex == 2) // product name
        {
            strncpy(device->productName, asciichar, 15);
            device->productName[15] = 0;

          #ifdef _USB_TRANSFER_DIAGNOSIS_
            printf(" product name: %s", device->productName);
          #endif
        }
        else if (stringIndex == 3) // serial number
        {
            // take the last 12 characters:

            int16_t last = strlen(asciichar); // store last position
            int16_t j = max(last-12, 0); // step 12 characters backwards, but not below zero

            strncpy(device->serialNumber, asciichar + j, 13);
          #ifdef _USB_TRANSFER_DIAGNOSIS_
            printf(" serial: %s", device->serialNumber);
          #endif
        }
    }
}

;
