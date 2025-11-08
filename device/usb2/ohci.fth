\ file: ohci.fth

only forth definitions
[undefined] usbdefs [if]
   vocabulary usbdefs
[then]

also system
also usbdefs definitions

hex

1 value usb-debug?          \ flag for debugging output
0 value enabledPortFlag     \ root hub ports enabled
0 value hcca

\ HcControl Register
3 constant OHCI_CTRL_CBSR   \  relation between control and bulk (cbsr+1 vs. 1)
4 constant OHCI_CTRL_PLEi   \ activate periodical transfers
8 constant OHCI_CTRL_IE     \ activate isochronous transfers
10 constant OHCI_CTRL_CLE   \ activate control transfers
20 constant OHCI_CTRL_BLE   \ activate bulk transfers
0c0 constant OHCI_CTRL_HCFS \ HostControllerFunctionalState for USB
100 constant OHCI_CTRL_IR   \ redirect IRQ to SMB
200 constant OHCI_CTRL_RWC  \ remote wakeup
400 constant OHCI_CTRL_RWE  \ activate remote wakeup
7fff0000 constant OHCI_CTRL_FSLARGESTDATAPACKET 

\ HcCommandStatus Register
1 constant OHCI_STATUS_RESET    \ reset
2 constant OHCI_STATUS_CLF      \ control list filled
4 constant OHCI_STATUS_BLF      \ bulk list filled
8 constant OHCI_STATUS_OCR      \ ownership change request
30000 constant OHCI_STATUS_SOC  \ scheduling overrun count

\ HcInterruptStatus
\ HcInterruptEnable, HcInterruptDisable Register
1 constant OHCI_INT_SO     \ scheduling overrun
2 constant OHCI_INT_WDH    \ writeback DoneHead
4 constant OHCI_INT_SF     \ start of frame
8 constant OHCI_INT_RD     \ resume detected
10 constant OHCI_INT_UE    \ unrecoverable error
20 constant OHCI_INT_FNO   \ frame number overflow
40 constant OHCI_INT_RHSC  \ root hub status change
40000000 constant OHCI_INT_OC     \ ownership change
80000000 constant OHCI_INT_MIE    \ master interrupt enable

\ Root Hub Partition

\ HcRhDescriptorA Register
0ff constant OHCI_RHA_NDP \ number downstream ports (max. 15)
100 constant OHCI_RHA_PSM \ PowerSwitchingMode
200 constant OHCI_RHA_NPS \ NoPowerSwitching
400 constant OHCI_RHA_DT  \ DeviceType (always 0)
800 constant OHCI_RHA_OCPM   \ OverCurrentProtectionMode
1000 constant OHCI_RHA_NOCP  \ NoOverCurrentProtection
0ff000000 constant OHCI_RHA_POTPGT \ PowerOnToPowerGoodTime

\ HcRhStatus Register
1 constant OHCI_RHS_LPS           \ LocalPowerStatus
2 constant OHCI_RHS_OCI           \ OverCurrentIndicator
8000 constant OHCI_RHS_DRWE       \ DeviceRemoteWakeupEnable
10000 constant OHCI_RHS_LPSC      \ LocalPowerStatusChange
20000 constant OHCI_RHS_OCIC      \ OverCurrentIndicatorChange
80000000 constant OHCI_RHS_CRWE   \ ClearRemoteWakeupEnable

\ HcRhPortStatus[1:NDP] Register
1 constant OHCI_PORT_CCS          \ CurrentConnectStatus (0 = no device connected, 1 = device connected)
2 constant OHCI_PORT_PES          \ PortEnableStatus (0 = port is disabled, 1 = port is enabled)
4 constant OHCI_PORT_PSS          \ PortSuspendStatus
8 constant OHCI_PORT_POCI         \ PortOverCurrentIndicator
10 constant OHCI_PORT_PRS         \ PortResetStatus
100 constant OHCI_PORT_PPS        \ PortPowerStatus
200 constant OHCI_PORT_LSDA       \ LowSpeedDeviceAttached (0 = full speed device, 1 = low speed device)
10000 constant OHCI_PORT_CSC      \ ConnectStatusChange
20000 constant OHCI_PORT_PESC     \ PortEnableStatusChange
40000 constant OHCI_PORT_PSSC     \ PortSuspendStatusChange
80000 constant OHCI_PORT_OCIC     \ PortOverCurrentIndicatorChange
100000 constant OHCI_PORT_PRSC    \ PortResetStatusChange

OHCI_PORT_CSC OHCI_PORT_PESC or OHCI_PORT_PSSC or OHCI_PORT_OCIC or OHCI_PORT_PRSC or constant OHCI_PORT_WTC  

\ USB - operational states of the HC
0 constant OHCI_USB_RESET  
20 constant OHCI_USB_RESUME
80 constant OHCI_USB_OPERATIONAL
0c0 constant OHCI_USB_SUSPEND   

\ ED
0 constant OHCI_ED_TD    
1 constant OHCI_ED_OUT   
2 constant OHCI_ED_IN    

\ TD
0 constant OHCI_TD_SETUP 
1 constant OHCI_TD_OUT   
2 constant OHCI_TD_IN    
7 constant OHCI_TD_NOINT 
0f constant OHCI_TD_NOCC 


0 [if]
There are two communication channels between the HC and the HC Driver.
The first channel uses a set of operational registers located on the HC. The HC is the target for all communication on this channel.
The operational registers contain control, status, and list pointer registers.
Within the operational register set is a pointer to a location in shared memory named the HC Communications Area (HCCA)
The HCCA is the second communication channel. The HC is the master for all communication on this channel.
The HCCA contains the head pointers to the interrupt Endpoint Descriptor lists, the head pointer to the done queue,
and status information associated with start-of-frame processing.
HCCA is a 256-byte structure of system memory used to send and receive control/status information to and from the HC.
This structure must be located on a 256-byte boundary. Phys. address of this structure has to be stored into the OpReg HcHCCA.
[then]

\ ohci registers
10 constant HcInterruptEnable
14 constant HcInterruptDisable 
34 constant HcFmInterval

d# 32 constant #intr

begin-structure /hcca
#intr cells 
    +field  >hcca-intr        \ array of interrupt EDs
    wfield: >hcca-frame-number  \ current frame number
    wfield: >hcca-pad1          \ cleared when the HC updates frameNumber
    field: >hcca-done-head      \ holds at frame end the current value of HcDoneHead, and interrupt is sent
d# 120 chars 
    +field >hcca-reserved   \ pad to 256 bytes
end-structure

\ hcca interrupt entry

begin-structure  /intr-entry   \ An entry of intr
    field: >intr-head       \ Virtual address of interrupt head
    field: >intr-tail       \ Virtual address of interrupt tail
    field: >iso-head        \ Virtual address of isochronous head
    field: >iso-tail        \ Virtual address of isochronous tail
end-structure
/intr-entry #intr * constant /intr

\
\ Endpoint descriptor (ED) as defined by the OHCI Spec; 16-byte aligned
\
begin-structure /ed
    field: >hced-control   \ 7:dev-addr,4:endpNum,2:dir,1:speed,1:sKip,1:format
                           \ 11:mps,5:ours
    field: >hced-td-tail  \ last TD in queue
    field: >hced-td-head  \ head TD in queue
    field: >hced-next     \ next ED on the list
dup constant /hced
\ Driver specific fields
    field: >ed-phys        \ Physical address of HC ED
    field: >ed-next        \ Pointer to the next endpoint
    field: >ed-prev        \ Pointer to the previous endpoint
    field: >ed-size        \ Size of EDs+TDs
d# 32 round-up          \ Multiple of 32 bytes
                        \ 32 bytes because there are cases where
                        \ EDs and TDs are allocated together
end-structure
/ed #intr * constant /eds       \ Size of all eds allocated at a time


\
\ Transfer Descriptor (TD) as defined by the OHCI Spec:
\ general TDs are 16-byte aligned; isochronous TDs are 32-byte aligned.
\

begin-structure    /td     \ Beginning of General TD fields
    field: >hctd-control   \ 18:ours,1:bufRounding,2:direction,3:delayInt
                           \ 1:toggle,1:toggleFromTD,2:errCnt,4:cond
    field: >hctd-cbp       \ Physical address of current buffer pointer
    field: >hctd-next      \ physical address of next TD
    field: >hctd-be        \ physical address of buffer end
dup constant /gtd
\ Isochronous TD fields
    wfield: >hctd-offset0       \ Offset 0 / PSW 0
    wfield: >hctd-offset1       \ Offset 1 / PSW 1
    wfield: >hctd-offset2       \ Offset 2 / PSW 2
    wfield: >hctd-offset3       \ Offset 3 / PSW 3
    wfield: >hctd-offset4       \ Offset 4 / PSW 4
    wfield: >hctd-offset5       \ Offset 5 / PSW 5
    wfield: >hctd-offset6       \ Offset 6 / PSW 6
    wfield: >hctd-offset7       \ Offset 7 / PSW 7
dup constant /itd
\ Driver specific fields
    field: >td-phys        \ Physical address of HC TD
    field: >td-next        \ Address of next TD
    field: >td-cbp         \ Address of current buffer pointer
    field: >td-/cbp-all    \ Buffer length (size of the entire buffer)
                           \ For bulk and intr TDs
d# 32 round-up          \ Multiple of 32 bytes
end-structure

3 constant OHCI_RETRIES 3


: hc-cntl@  ( -- data )   4 usb@  ;
: hc-cntl!  ( data -- )   4 usb!  ;
: hc-stat@  ( -- data )   8 usb@  ;
: hc-cmd!   ( data -- )   8 usb!  ;
: hc-intr@  ( -- data )   c usb@  ;
: hc-intr!  ( data -- )   c usb!  ;
: hc-hcca@  ( -- data )  18 usb@  ;
: hc-hcca!  ( data -- )  18 usb!  ;


: hc-rh-desA@  ( -- data )  48 usb@  ;
: hc-rh-desA!  ( data -- )  48 usb!  ;
: hc-rh-desB@  ( -- data )  4c usb@  ;
: hc-rh-desB!  ( data -- )  4c usb!  ;
: hc-rh-stat@  ( -- data )  50 usb@  ;
: hc-rh-stat!  ( data -- )  50 usb!  ;

: hc-rh-psta@  ( port -- data )  4 * 54 + usb@  ;
: hc-rh-psta!  ( data port -- )  4 * 54 + usb!  ;

: hc-cntl-clr  ( bit-mask -- )  hc-cntl@ swap invert and hc-cntl!  ;
: hc-cntl-set  ( bit-mask -- )  hc-cntl@ swap or hc-cntl!  ;

: ohci-toggle-frame-interval ( -- )
    HcFmInterval usb@ h# 80000000
    2dup and if invert and else or then
    HcFmInterval usb!
;



\  PORTS  

\ number of downstream ports on root hub
: #ports ( -- u)  hc-rhdesA@ 0f and ;
    
: ohci-port-check( -- )
  #ports 0 do cr i hc-rh-psta@ OHCI_PORT_CCS and u. loop
;

 
: ohci-rh-port-reset ( port-- )
  dup hc-rh-psta@  OHCI_PORT_PRS or 
  over hc-rh-psta!            \ reset
  100       \ timeout
  begin 
  over hc-rh-psta@ OHCI_PORT_PRS and over and while
     1- 2 ms
  repeat
  0= if
    cr ." Timeout Error: ohci port reset bit still set to 1"
    drop
  else
    dup hc-rh-psta@ OHCI_PORT_PES or swap hc-rh-psta! \ enable
    10 ms
  then
;

0 [if]

\  ohci handler                                            *

: ohci-handler ( -- )    \ interrupt handler
\ Check if an OHCI controller issued this interrupt
\ check eds and tds
  HcInterruptStatus usb@ ?dup 0= if 
    cr ." Interrupt came from another OHCI device!"
    exit
  then

  dup OHCI_INT_WDH and if  \ write back done head
    cr ." Write back done head."
    \ the value has to be stored before resetting OHCI_INT_WDH
  then
  dup HcInterruptStatus usb!      \ reset interrupts

\    if (!((val & OHCI_INT_SF) || (val & OHCI_INT_RHSC)))
\    {
        printf("\nUSB OHCI %u: ", o->num);
\    }

  OHCI_INT_SO over and if      \ scheduling overrun
    cr ." Scheduling overrun."
  then

  OHCI_INT_RD over and if      \ resume detected
    cr ." Resume detected."
  then

  OHCI_INT_UE over and if      \ unrecoverable error
    cr ." Unrecoverable HC error."
    hc-stat@ OHCI_STATUS_RESET or  hc-cmd!
  then

  OHCI_INT_FNO over and if   \ frame number overflow
      cr ." Frame number overflow."
  then

  OHCI_INT_RHSC over and if  \ root hub status change
      ohci-port-check
  then
    
  OHCI_INT_OC over and      \ ownership change
    cr ." Ownership change."
  then
;


\     Transactions                                              *

typedef struct
{
    ohciTD_t*   TD;
    void*       TDBuffer;
    void*       inBuffer;
    size_t      inLength;
} ohci_transaction_t;


void ohci_setupTransfer(usb_transfer_t* transfer)
{
    ohci_t* o = (ohci_t*)((hc_port_t*)transfer->device->port->data)->hc;

    if (o->lastTT != transfer->type)
    {
        ohci_resetMempool(o, transfer->type);
        o->lastTT = transfer->type;
    }

    o->OpRegs->HcControl &= ~(OHCI_CTRL_CLE | OHCI_CTRL_BLE); \ de-activate control and bulk transfers
    o->OpRegs->HcCommandStatus &= ~OHCI_STATUS_CLF; \ control list not filled
    o->OpRegs->HcCommandStatus &= ~OHCI_STATUS_BLF; \ bulk list not filled

    \ recycle bulk ED/TDs
    if ((o->indexED >= NUM_ED-2) ||
        (o->indexTD >= NUM_TD-2))
    {
        ohci_resetMempool(o, USB_BULK);
    }

    \ recycle control ED/TDs
    if ((o->indexED == NUM_ED_BULK-2) || (o->indexED == NUM_ED_BULK-1) ||
        (o->indexTD == NUM_TD_BULK-2) || (o->indexTD == NUM_TD_BULK-1))
    {
        ohci_resetMempool(o, USB_CONTROL);
    }

    \ endpoint descriptor
    transfer->data = o->pED[o->indexED];

  #ifdef _OHCI_DIAGNOSIS_
    printf("\nsetupTransfer: indexED: %u", o->indexED);
  #endif
}

void ohci_setupTransaction(usb_transfer_t* transfer, usb_transaction_t* uTransaction, bool toggle, uint32_t tokenBytes, uint8_t type, uint8_t req, uint8_t hiVal, uint8_t loVal, uint16_t i, uint16_t length)
{
    ohci_transaction_t* oTransaction = uTransaction->data = malloc(sizeof(ohci_transaction_t), 0, "ohci_transaction_t");
    oTransaction->inBuffer = 0;
    oTransaction->inLength = 0;

    ohci_t* o = (ohci_t*)((hc_port_t*)transfer->device->port->data)->hc;

    oTransaction->TD = ohci_createTD_SETUP(o, transfer->data, 1, toggle, tokenBytes, type, req, hiVal, loVal, i, length, &oTransaction->TDBuffer);

  #ifdef _OHCI_DIAGNOSIS_
    usb_request_t* request = (usb_request_t*)oTransaction->TDBuffer;
    printf("\ntype: %u req: %u valHi: %u valLo: %u i: %u len: %u", request->type, request->request, request->valueHi, request->valueLo, request->index, request->length);
  #endif

    if (transfer->transactions.tail)
    {
        ohci_transaction_t* oLastTransaction = ((usb_transaction_t*)transfer->transactions.tail->data)->data;
        oLastTransaction->TD->nextTD = paging_getPhysAddr(oTransaction->TD); \ build TD queue
    }
}

void ohci_inTransaction(usb_transfer_t* transfer, usb_transaction_t* uTransaction, bool toggle, void* buffer, size_t length)
{
    ohci_t* o = (ohci_t*)((hc_port_t*)transfer->device->port->data)->hc;
    ohci_transaction_t* oTransaction = uTransaction->data = malloc(sizeof(ohci_transaction_t), 0, "ohci_transaction_t");
    oTransaction->inBuffer = buffer;
    oTransaction->inLength = length;

    oTransaction->TDBuffer = o->pTDbuff[o->indexTD];
    oTransaction->TD = ohci_createTD_IO(o, transfer->data, 1, OHCI_TD_IN, toggle, length);

    if (transfer->transactions.tail)
    {
        ohci_transaction_t* oLastTransaction = ((usb_transaction_t*)transfer->transactions.tail->data)->data;
        oLastTransaction->TD->nextTD = paging_getPhysAddr(oTransaction->TD); \ build TD queue
    }
}

void ohci_outTransaction(usb_transfer_t* transfer, usb_transaction_t* uTransaction, bool toggle, void* buffer, size_t length)
{
    ohci_t* o = (ohci_t*)((hc_port_t*)transfer->device->port->data)->hc;
    ohci_transaction_t* oTransaction = uTransaction->data = malloc(sizeof(ohci_transaction_t), 0, "ohci_transaction_t");
    oTransaction->inBuffer = 0;
    oTransaction->inLength = 0;

    oTransaction->TDBuffer = o->pTDbuff[o->indexTD];
    oTransaction->TD = ohci_createTD_IO(o, transfer->data, 1, OHCI_TD_OUT, toggle, length);

    if (buffer != 0 && length != 0)
    {
        memcpy(oTransaction->TDBuffer, buffer, length);
    }

    if (transfer->transactions.tail)
    {
        ohci_transaction_t* oLastTransaction = ((usb_transaction_t*)transfer->transactions.tail->data)->data;
        oLastTransaction->TD->nextTD = paging_getPhysAddr(oTransaction->TD); \ build TD queue
    }
}

void ohci_issueTransfer(usb_transfer_t* transfer)
{
    /*  A transfer is completed when the Host Controller successfully transfers, to or from an endpoint, the byte pointed to by BufferEnd.
        Upon successful completion, the Host Controller sets CurrentBufferPointer to zero, sets ConditionCode to NOERROR,
        and retires the General TD to the Done Queue.
    */

    usb_outTransaction(transfer, 1, 0, 0); \ dummy at the end of the TD chain (for different headPtr and tailPtr in the ED)

    ohci_t* o = (ohci_t*)((hc_port_t*)transfer->device->port->data)->hc;
    ohci_transaction_t* firstTransaction = ((usb_transaction_t*)transfer->transactions.head->data)->data;

  #ifdef _OHCI_DIAGNOSIS_
    printf("\nohci_createED: devNum = %u endp = %u packetsize = %u", transfer->device->num, transfer->endpoint, transfer->packetSize);
  #endif

    ohci_createED(transfer->data, paging_getPhysAddr(transfer->data), firstTransaction->TD, transfer->device->num, transfer->endpoint, transfer->packetSize);

    if (transfer->type == USB_CONTROL)
    {
        o->OpRegs->HcControlCurrentED = paging_getPhysAddr(transfer->data);
    }

    if (transfer->type == USB_BULK)
    {
        o->OpRegs->HcBulkCurrentED = paging_getPhysAddr(transfer->data);
    }

  #ifdef _OHCI_DIAGNOSIS_
    textColor(MAGENTA);
    printf("\nHcControlCurrentED: %X", o->OpRegs->HcControlCurrentED);
    printf(" ED->skip = %u ED->Halted = %u", ((ohciED_t*)transfer->data)->sKip, ((ohciED_t*)transfer->data)->tdQueueHead & BIT(0));
    printf("\nHeadP = %X TailP = %X", ((ohciED_t*)transfer->data)->tdQueueHead, ((ohciED_t*)transfer->data)->tdQueueTail);
    textColor(TEXT);

    for (uint8_t i=0; i<5; i++)
    {
        printf("\ni=%u\tED->TD:%X->%X TD->TD:%X->%X buf:%X",
               i, paging_getPhysAddr(o->pED[i]), o->pED[i]->tdQueueHead,
               paging_getPhysAddr(o->pTD[i]), o->pTD[i]->nextTD, o->pTD[i]->curBuffPtr);
    }
  #endif

    o->OpRegs->HcCommandStatus |= (OHCI_STATUS_CLF | OHCI_STATUS_BLF); \ control and bulk lists filled

  #ifdef _OHCI_DIAGNOSIS_
    textColor(MAGENTA);
    printf("\nHcCommandStatus: %X", o->OpRegs->HcCommandStatus);
    textColor(TEXT);
  #endif

    for (uint8_t i = 0; i < NUMBER_OF_OHCI_RETRIES && !transfer->success; i++)
    {
      #ifdef _OHCI_DIAGNOSIS_
        printf("\ntransfer try = %u\n", i);
      #endif

        transfer->success = true;

        o->OpRegs->HcCommandStatus |= (OHCI_STATUS_CLF | OHCI_STATUS_BLF); \ control and bulk lists filled
        o->pED[i]->tdQueueHead &= ~0x1; \ reset Halted Bit
        o->OpRegs->HcControl |= (OHCI_CTRL_CLE | OHCI_CTRL_BLE); \ activate control and bulk transfers ////////////////////// S T A R T /////////////////

      #ifdef _OHCI_DIAGNOSIS_
        printf("\nNumber of TD elements (incl. dummy-TD): %u", list_getCount(&transfer->transactions));
      #endif

        for (dlelement_t* elem = transfer->transactions.head; elem != 0; elem = elem->next)
        {
            while ((o->OpRegs->HcFmRemaining & 0x3FFF) < 16000) { /* wait for nearly full frame time */ }

          #ifdef _OHCI_DIAGNOSIS_
            printf(" remaining time: %u  frame number: %u", o->OpRegs->HcFmRemaining & 0x3FFF, o->OpRegs->HcFmNumber);
          #endif

            delay(50000); \ pause after transaction
        }

        delay(50000); \ pause after transfer

        \ check conditions - do not check the last dummy-TD
        for (dlelement_t* elem = transfer->transactions.head; elem && elem->next; elem = elem->next)
        {
            ohci_transaction_t* transaction = ((usb_transaction_t*)elem->data)->data;
            ohci_showStatusbyteTD(transaction->TD);

            transfer->success = transfer->success && (transaction->TD->cond == 0);
        }

      #ifdef _OHCI_DIAGNOSIS_
        if (!transfer->success)
        {
            printf("\nRetry transfer: %u", i+1);
        }
      #endif
    }

  #ifdef _OHCI_DIAGNOSIS_
    textColor(IMPORTANT);
    printf("\n\nED-Index: %u, Transfer->endpoint: %u, &o: %X", o->indexED, transfer->endpoint, o);
    printf("\nhcca->donehead: %X ", o->hcca->doneHead);
    textColor(TEXT);
    for (uint8_t i=0; i<5; i++)
    {
        printf("\ni=%u\tED->TD:%X->%X TD->TD:%X->%X buf:%X",
               i, paging_getPhysAddr(o->pED[i]), o->pED[i]->tdQueueHead,
               paging_getPhysAddr(o->pTD[i]), o->pTD[i]->nextTD, o->pTD[i]->curBuffPtr);
    }
  #endif

    for (dlelement_t* elem = transfer->transactions.head; elem != 0; elem = elem->next)
    {
        ohci_transaction_t* transaction = ((usb_transaction_t*)elem->data)->data;

        if (transaction->inBuffer != 0 && transaction->inLength != 0)
        {
            memcpy(transaction->inBuffer, transaction->TDBuffer, transaction->inLength);
        }
        free(transaction);
    }
    if (transfer->success)
    {
      #ifdef _OHCI_DIAGNOSIS_
        textColor(SUCCESS);
        printf("\nTransfer successful.");
        textColor(TEXT);
      #endif
    }
    else
    {
        textColor(ERROR);
        printf("\nTransfer failed.");
        textColor(TEXT);
    }

    o->indexED++;
}


\ ohci ED TD functions                                      *

ohciTD_t* ohci_createTD_SETUP(ohci_t* o, ohciED_t* oED, uintptr_t next, bool toggle, uint32_t tokenBytes, uint8_t type, uint8_t req, uint8_t hiVal, uint8_t loVal, uint16_t i, uint16_t length, void** buffer)
{
    ohciTD_t* oTD = (ohciTD_t*)o->pTD[o->indexTD];

  #ifdef _OHCI_DIAGNOSIS_
    printf("\nohci_createTD_SETUP: ED = %u  TD = %u toggle: %u", o->indexED, o->indexTD, oTD->toggle);
  #endif

    if (next != 0x1)
    {
        oTD->nextTD = paging_getPhysAddr((void*)next);
    }
    else
    {
        oTD->nextTD = BIT(0);
    }

    oTD->direction    = OHCI_TD_SETUP;
    oTD->toggle       = toggle;
    oTD->toggleFromTD = 1;
    oTD->cond         = OHCI_TD_NOCC; \ to be executed
    oTD->delayInt     = OHCI_TD_NOINT;
    oTD->errCnt       = 0;
    oTD->bufRounding  = 1;

    usb_request_t* request = *buffer = o->pTDbuff[o->indexTD];
    request->type     = type;
    request->request  = req;
    request->valueHi  = hiVal;
    request->valueLo  = loVal;
    request->index    = i;
    request->length   = length;

    oTD->curBuffPtr   = paging_getPhysAddr(request);
    oTD->buffEnd      = oTD->curBuffPtr + sizeof(usb_request_t) - 1; \ physical address of the last byte in the buffer

    oED->tdQueueTail = paging_getPhysAddr(oTD);

    o->indexTD++;
    return (oTD);
}

ohciTD_t* ohci_createTD_IO(ohci_t* o, ohciED_t* oED, uintptr_t next, uint8_t direction, bool toggle, uint32_t tokenBytes)
{
    ohciTD_t* oTD = o->pTD[o->indexTD];

  #ifdef _OHCI_DIAGNOSIS_
    printf("\nohci_createTD_IO: ED = %u  TD = %u toggle: %u", o->indexED, o->indexTD, oTD->toggle);
  #endif

    if (next != 0x1)
    {
        oTD->nextTD = paging_getPhysAddr((void*)next);
    }
    else
    {
        oTD->nextTD = BIT(0);
    }
    oTD->direction    = direction;
    oTD->toggle       = toggle;
    oTD->toggleFromTD = 1;
    oTD->cond         = OHCI_TD_NOCC; \ to be executed
    oTD->delayInt     = OHCI_TD_NOINT;
    oTD->errCnt       = 0;
    oTD->bufRounding  = 1;

    if (tokenBytes)
    {
        oTD->curBuffPtr  = paging_getPhysAddr(o->pTDbuff[o->indexTD]);
        oTD->buffEnd     = oTD->curBuffPtr + tokenBytes - 1; \ BufferEnd contains physical address of the last byte in the buffer
    }
    else
    {
        oTD->curBuffPtr  = paging_getPhysAddr(o->pTDbuff[o->indexTD]);
        oTD->buffEnd     = oTD->curBuffPtr;
    }

    oED->tdQueueTail = paging_getPhysAddr(oTD);

    o->indexTD++;
    return (oTD);
}


: ohci-create-ed ( ed horizPtr td dev ep size --) \check original 
  usb-debug? if ." next ED: " 5 pick >ed-head @ .hex8 then

    head->endpNum = endpoint;
    
    usb-debug? if
        cr ." endpoint: %u", head->endpNum"
    then

    head->devAddr = device;

  #ifdef _OHCI_DIAGNOSIS_
    printf("  device: %u", head->devAddr);
  #endif

    head->mps     = MPS_FULLSPEED; \ packetSize;
    head->dir     = OHCI_ED_TD ;   \ 00b Get direction From TD
    head->speed   = 0;  \ speed of the endpoint: full-speed (0), low-speed (1)
    head->format  = 0;  \ format of the TDs: Control, Bulk, or Interrupt Endpoint (0); Isochronous Endpoint (1)

    if (firstTD == 0)
    {
        head->tdQueueHead = 0x1; \ no TD in queue
        printf("\nno TD in queue");
    }
    else
    {
        head->tdQueueHead = paging_getPhysAddr((void*)firstTD) & ~0xD; \ head TD in queue // Flags 0 0 C H
      #ifdef _OHCI_DIAGNOSIS_
        printf("\nohci_createED: %X tdQueueHead = %X tdQueueTail = %X", paging_getPhysAddr(head), head->tdQueueHead, head->tdQueueTail); \ Tail is read-only
      #endif
    }

    head->sKip = 0;  \ 1: HC continues on to next ED w/o attempting access to the TD queue or issuing any USB token for the endpoint
}

[then]

\  analysis tools


: ohci-td-status? ( td -- cond?)
  >hctd-control @ dup case
    0 of cr ." Successful Completion."  endof 
    1 of cr ." Last data packet from endpoint contained a CRC error." endof
    2 of cr ." Last data packet from endpoint contained a bit stuffing violation." endof
    3 of cr ." Last packet from endpoint had data toggle PID that did not match the expected value." endof
    4 of cr ." TD was moved to the Done Queue because the endpoint returned a STALL PID." endof
    5 of cr ." Device: no response to token (IN) or no handshake (OUT)" endof
    6 of cr ." Check bits on PID from endpoint failed on data PID (IN) or handshake (OUT)." endof
    7 of cr ." Receive PID was not valid when encountered or PID value is not defined." endof
    8 of cr ." DATAOVERRUN: Too many data returned by the endpoint." endof
    9 of cr ." DATAUNDERRUN: Endpoint returned less than MPS." endof
   12 of cr ." BUFFEROVERRUN" endof 
   13 of cr ." BUFFERUNDERRUN" endof     
   15 of cr ." NOT ACCESSED" endof 
  endcase
;


\  HCCA
: ohci-hcca-init ( -- )

\ Initialize the device data HCCA block to match the current device data state;

    /hcca allocate throw to hcca \ HCCA must be minimum 256-byte aligned
   hcca /hcca erase

\  Initialize the Operational Registers to match the current device data state;

\ initialize ed and td
\  ed-td-init
\    o->OpRegs->HcControlHeadED = o->OpRegs->HcControlCurrentED = paging_getPhysAddr(o->pED[NUM_ED_CONTROL]);
\    o->OpRegs->HcBulkHeadED    = o->OpRegs->HcBulkCurrentED    = paging_getPhysAddr(o->pED[NUM_ED_BULK]);

    usb-debug? if
        cr ." HCCA (phys. address):" hcca .hex8
    then


\ Set HcInterruptEnable to have all interrupt enabled except Start-of-Frame detect
\   o->OpRegs->HcInterruptDisable = OHCI_INT_SF   | \ start of frame
\                                    OHCI_INT_MIE;   \ deactivates interrupts
\    o->OpRegs->HcInterruptStatus  = ~0;
\    o->OpRegs->HcInterruptEnable  = OHCI_INT_SO   | \ scheduling overrun
\                                    OHCI_INT_WDH  | \ write back done head
\                                    OHCI_INT_RD   | \ resume detected
\                                    OHCI_INT_UE   | \ unrecoverable error
\                                    OHCI_INT_FNO  | \ frame number overflow
\                                    OHCI_INT_RHSC | \ root hub status change
\                                    OHCI_INT_OC   | \ ownership change
\                                    OHCI_INT_MIE;   \ activates interrupts

\    o->OpRegs->HcControl &= ~(OHCI_CTRL_CLE | OHCI_CTRL_PLE | OHCI_CTRL_IE | OHCI_CTRL_BLE);  \ de-activate bulk, periodical and isochronous transfers

\    o->OpRegs->HcControl |= OHCI_CTRL_RWE; \ activate RemoteWakeup

\ Set HcPeriodicStart to a value that is 90% of the value in FrameInterval
\ field of the HcFmInterval register. When HcFmRemaining reaches this value,
\ periodic lists gets priority over control/bulk processing
\    o->OpRegs->HcPeriodicStart = (o->OpRegs->HcFmInterval & 0x3FFF) * 90/100;

\ The counter value represents the largest amount of data in bits which can
\ be sent or received by the HC in a single transaction at any given time
\ without causing scheduling overrun.
\    o->OpRegs->HcFmInterval &= ~OHCI_CTRL_FSLARGESTDATAPACKET; \ clear FSLargestDataPacket
\    o->OpRegs->HcFmInterval |= BIT(30); \ ???
\    ohci_toggleFrameInterval(o);

\ LSThreshold contains a value which is compared to the FrameRemaining field
\ prior to initiating a Low Speed transaction.
\ The transaction is started only if FrameRemaining >= this field.
\ The value is calculated by HCD with the consideration of transmission
\ and setup overhead.
\    o->OpRegs->HcLSThreshold = 0; \ HCD allowed to change?

\    printf("\nHcFrameInterval: %u", o->OpRegs->HcFmInterval & 0x3FFF);
\    printf("  HcPeriodicStart: %u", o->OpRegs->HcPeriodicStart);
\    printf("  FSMPS: %u bits", (o->OpRegs->HcFmInterval >> 16) & 0x7FFF);
\    printf("  LSThreshhold: %u", o->OpRegs->HcLSThreshold & 0xFFF);

\ ControlBulkServiceRatio (CBSR)
\    o->OpRegs->HcControl |= OHCI_CTRL_CBSR;  \ No. of Control EDs Over Bulk EDs Served = 4 : 1

\ The HCD then begins to send SOF tokens on the USB by writing to the
\ HcControl register with the HostControllerFunctionalState set to
\ USBOPERATIONAL and the appropriate enable bits set.
\ The Host Controller begins sending SOF tokens within one ms
\ (if the HCD needs to know when the SOFs it may unmask the StartOfFrame
\ interrupt).
\    printf("\n\nHC will be set to USB Operational.\n");

\    o->OpRegs->HcControl &= ~OHCI_CTRL_HCFS;      \ clear HCFS bits
\    o->OpRegs->HcControl |= OHCI_USB_OPERATIONAL; \ set specific HCFS bit

\    o->OpRegs->HcRhStatus |= OHCI_RHS_LPSC;           \ SetGlobalPower: turn on power to all ports
\    o->hc.rootPortCount = min(OHCIPORTMAX, BYTE1(o->OpRegs->HcRhDescriptorA)); \ NumberDownstreamPorts

\  o->OpRegs->HcRhDescriptorA &= ~OHCI_RHA_DT; \ DeviceType: This bit specifies that the Root Hub is not a compound device.
\ The Root Hub is not permitted to be a compound device. This field should always read/write 0.
\    o->OpRegs->HcRhDescriptorB = 0; \ DR: devices removable; PPCM: PortPowerControlMask is set to global power switching mode

\ duration HCD has to wait before accessing a powered-on port of the Root Hub.
\ It is implementation-specific. Duration is calculated as POTPGT * 2 ms.
\    o->powerWait = max(20, 2 * BYTE4(o->OpRegs->HcRhDescriptorA));

cr cr ." Found %u Rootports. Power wait: %u ms\n" \ rootPortCount, powerWait
\   o->hc.rootPorts = malloc(sizeof(hc_port_t)*o->hc.rootPortCount, 0, "ohci_port (root)");
\    for (uint8_t j = 0; j < o->hc.rootPortCount; j++)
\    {
\        ohci_constructPort(o->hc.rootPorts+j, &o->hc, j);
\    }
\    enabledPortFlag = true;
\    for (uint8_t j = 0; j < o->hc.rootPortCount; j++)
\    {
\        o->OpRegs->HcRhPortStatus[j] |= OHCI_PORT_PRS | OHCI_PORT_CCS | OHCI_PORT_PES;
\        sleepMilliSeconds(50);
\    }
;

: ohci-reset-hc ( -- )
 usb-debug? if cr ." >>>ohci_resetHostController<<<" then

 OHCI_INT_MIE   HcInterruptDisable usb! \ disable interrupts

  hc-cntl@ OHCI_CTRL_IR and if   \ SMM driver is active
    hc-stat@ OHCI_STATUS_OCR or hc-cmd! \ ownership change request
    \ monitor the IR bit to determine when the ownership change has taken effect
    d# 1000 begin
        hc-cntl@  OHCI_CTRL_IR and over and while
            1 ms 1-
    repeat
    if      \ bit cleared
        cr ." OHCI takes control from SMM"
    else
        cr ." Ownership change request did not work. SMM has still control."
        OHCI_CTRL_IR  hc-cntl-set
         200 ms
        hc-cntl@ OHCI_CTRL_IR or if \ SMM driver is still active
            cr ." OHCI taking control from SMM did not work."
        else cr ." Success in taking control from SMM." then
    then
 else   \ InterruptRouting bit is not set
    hc-cntl@ OHCI_CTRL_HCFS  OHCI_USB_RESET <> if
    \ there is an active BIOS driver, if the InterruptRouting bit is not set
    \ and the HostControllerFunctionalState (HCFS) is not USBRESET
        cr ." There is an active BIOS OHCI driver"
        hc-cntl@ OHCI_CTRL_HCFS and OHCI_USB_OPERATIONAL <> if
            \ If the HostControllerFunctionalState is not USBOPERATIONAL,
            \ the OS driver should set the HCFS to USBRESUME
            cr ." Activate RESUME"
            OHCI_CTRL_HCFS hc-cntl-clr     \ clear HCFS bits
            OHCI_USB_RESUME hc-cntl-set   \ set specific HCFS bit

            \ and wait the minimum time specified in the USB Specification
            \ for assertion of resume on the USB
            10 ms
        then

    else        \  HCFS is USBRESET
        \ Neither SMM nor BIOS
        10 ms
    then
 then

\ setup of the Host Controller
    cr ." Setup of the HC"
\ The HC Driver should now save the contents of the HcFmInterval register
    HcFmInterval usb@   ( fi )
\ issue a software reset
    hc-stat@ OHCI_STATUS_RESET or hc-cmd! 50 ms

    \ After the software reset is complete (a maximum of 10 ms), the Host Controller Driver
    \ should restore the value of the HcFmInterval register
    HcFmInterval usb!
    ohci-toggle-frame-interval

    \ The HC is now in the USBSUSPEND state;
    \ it must not stay in this state more than 2 ms
    \ or the USBRESUME state will need to be entered
    \ for the minimum time specified in the USB Specification
    \ for the assertion of resume on the USB.

    hc-cntl@ OHCI_CTRL_HCFS and OHCI_USB_SUSPEND = if
        OHCI_CTRL_HCFS hc-cntl-clr      \ clear HCFS bits
        OHCI_USB_RESUME hc-cntl-set     \ set specific HCFS bit
        100 ms
    then

    ohci-hcca-init

;


: ohci-init-hc ( -- )
 usb-debug? if cr ." >>>initOHCIHostController<<<" then

\ make device 0 13 0 a bus master
\ install interrupt handler
\ irq_installPCIHandler(o->PCIdevice->irq, ohci_handler, o->PCIdevice);

  ohci-reset-hc
;


: ohci-start ( --)
  usb-debug? if cr ." >>>startOHCI<<<" then
  ohci-init-hc
;




