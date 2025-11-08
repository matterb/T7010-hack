\ typedef enum usb_transferType_t
0 constant USB_BULK
1 constant USB_CONTROL
2 constant USB_INTERRUPT
3 constant USB_ISOCHRONOUS

begin-structure usb_transfer_t
    void*              data;
    usb_transferType_t type;
    uint32_t           endpoint;
    uint32_t           packetSize;
    usb_device_t*      device;
    list_t             transactions;
    bool               success;
end-structure

\ typedef enum usb_transactionType_t;
0 constant USB_TT_SETUP
1 constant USB_TT_IN
2 constant USB_TT_OUT

typedef struct usb_transaction_t;
{
    void*                 data; // Contains pointer to *hci_transaction_t
    usb_transactionType_t type;
}


typedef struct hc_port_t;
{
    port_t        port;
    struct hc*    hc;
    bool          connected;
    usb_device_t* device;
    void*         data;
}

typedef struct hc
{
    hc_port_t*     rootPorts;         // root ports
    list_t         otherPorts;        // non-root ports
    uint8_t        rootPortCount;     // number of rootports
} hc_t;



void usb_setupTransfer(usb_device_t* usbDevice, usb_transfer_t* transfer, usb_transferType_t type, uint32_t endpoint, size_t maxLength)
{
    transfer->device       = usbDevice;
    transfer->endpoint     = endpoint;
    transfer->type         = type;
    transfer->packetSize   = min(maxLength, usbDevice->endpoints[endpoint].mps);
    transfer->success      = false;
    list_construct(&transfer->transactions);
    ohci_setupTransfer(transfer);
}

void usb_setupTransaction(usb_transfer_t* transfer, uint32_t tokenBytes, uint8_t type, uint8_t req, uint8_t hiVal, uint8_t loVal, uint16_t index, uint16_t length)
{
    usb_transaction_t* transaction = malloc(sizeof(usb_transaction_t), 0, "usb_transaction_t");
    transaction->type = USB_TT_SETUP;
    ohci_setupTransaction(transfer, transaction, false, tokenBytes, type, req, hiVal, loVal, index, length);
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

    ohci_inTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength);
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

    ohci_outTransaction(transfer, transaction, transfer->device->endpoints[transfer->endpoint].toggle, buffer, clampedLength);
    list_append(&transfer->transactions, transaction);

    transfer->device->endpoints[transfer->endpoint].toggle = !transfer->device->endpoints[transfer->endpoint].toggle; // Switch toggle

    if (remainingTransactions > 0)
    {
        usb_outTransaction(transfer, transfer->device->endpoints[transfer->endpoint].toggle, buffer+clampedLength, length);
    }
}

void usb_issueTransfer(usb_transfer_t* transfer)
{
    ohci_issueTransfer(transfer);
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

    ohci_constructPort(port, hc, num);
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


