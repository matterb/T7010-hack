only forth also system definitions


\ * Copyright (C) 2012 Michael Brown <mbrown@fensystems.co.uk>.
\ A 32-bit packet descriptor dword aligned 
begin-structure natsemi_descriptor_32
        field: ns>link      \ Link to next descriptor
        field: ns>cmdsts    \ Command / status 
        field: ns>bufptr    \ Buffer pointer 
end-structure

\ A packet descriptor
\ * The 32-bit and 64-bit variants are overlaid such that "cmdsts" can
\ * be accessed as a common field, and the overall size is a power of
\ * two (to allow the descriptor ring length to be used as an
\ * alignment).
begin-structure pkt-common
    \ Common fields 
    16 cells +field pkt-reserved-a \ Reserved
    field: pkt-cmdsts               \ Command / status
    16 chars +field pkt-reserved-b \ Reserved 
end-structure           \ pkt-common

\   struct {
\       /** Reserved */
\       uint8_t reserved[12];
\       /** Descriptor */
\       struct natsemi_descriptor_32 d32;
\   } __attribute__ (( packed )) d32pad;

\ Descriptor buffer size mask */

\ Packet descriptor flags */

h# 80000000 constant NATSEMI_DESC_OWN   \ Descriptor is owned by NIC
h# 20000000 constant NATSEMI_DESC_INTR  \ Request descriptor interrupt
h# 08000000 constant NATSEMI_DESC_OK    \ Packet OK


h# 20 constant NATSEMI_CR_RXR           \ Receiver reset
h# 10 constant NATSEMI_CR_TXR           \ Transmit reset
h# 04 constant NATSEMI_CR_RXE           \ Receiver enable
h# 01 constant NATSEMI_CR_TXE           \ Transmit enable

\ Maximum time to wait for a reset, in milliseconds 
d# 100 constant NATSEMI_RESET_MAX_WAIT_MS

\ Configuration and Media Status Register */
\ NATSEMI_CFG 0x0004
\ NATSEMI_CFG_LNKSTS    0x80000000UL    /**< Link status */
\ NATSEMI_CFG_SPDSTS1   0x40000000UL    /**< Speed status bit 1 */
\ NATSEMI_CFG_MODE_1000 0x00400000UL    /**< 1000 Mb/s mode control */
\ NATSEMI_CFG_PCI64_DET 0x00002000UL    /**< PCI 64-bit bus detected */
\ NATSEMI_CFG_DATA64_EN 0x00001000UL    /**< 64-bit data enable */
\ NATSEMI_CFG_M64ADDR   0x00000800UL    /**< 64-bit address enable */
\ NATSEMI_CFG_EXTSTS_EN 0x00000100UL    /**< Extended status enable */

\ EEPROM Access Register */
\ NATSEMI_MEAR 0x0008
\ NATSEMI_MEAR_EESEL    0x00000008UL    /**< EEPROM chip select */
\ NATSEMI_MEAR_EECLK    0x00000004UL    /**< EEPROM serial clock */
\ NATSEMI_MEAR_EEDO 0x00000002UL    /**< EEPROM data out */
\ NATSEMI_MEAR_EEDI 0x00000001UL    /**< EEPROM data in */

\ Word offset of MAC address within sane EEPROM layout */
\ NATSEMI_EEPROM_MAC_SANE 0x0a

\ Word offset of MAC address within insane EEPROM layout */
\ NATSEMI_EEPROM_MAC_INSANE 0x06

\ PCI Test Control Register */
\ NATSEMI_PTSCR 0x000c
\ NATSEMI_PTSCR_EELOAD_EN   0x00000004UL    /**< Enable EEPROM load */

\ Maximum time to wait for a configuration reload, in milliseconds */
\ NATSEMI_EELOAD_MAX_WAIT_MS 100

\ Interrupt Status Register */
\ NATSEMI_ISR 0x0010
\ NATSEMI_IRQ_TXDESC    0x00000080UL    /**< TX descriptor */
\ NATSEMI_IRQ_RXDESC    0x00000002UL    /**< RX descriptor */

\ Interrupt Mask Register */
\ NATSEMI_IMR 0x0014

\ Interrupt Enable Register */
\ NATSEMI_IER 0x0018
\ NATSEMI_IER_IE        0x00000001UL    /**< Interrupt enable */

\ Transmit Descriptor Pointer */
\ NATSEMI_TXDP 0x0020

\ Transmit Descriptor Pointer High Dword (64-bit) */
\ NATSEMI_TXDP_HI_64 0x0024

\ Number of transmit descriptors */
\ NATSEMI_NUM_TX_DESC 4

\ Transmit configuration register (32-bit) */
\ NATSEMI_TXCFG_32 0x24

\ Transmit configuration register (64-bit) */
\ NATSEMI_TXCFG_64 0x28
\ #define NATSEMI_TXCFG_CSI 0x80000000UL    /**< Carrier sense ignore */
\ #define NATSEMI_TXCFG_HBI 0x40000000UL    /**< Heartbeat ignore */
\ #define NATSEMI_TXCFG_ATP 0x10000000UL    /**< Automatic padding */
\ #define NATSEMI_TXCFG_ECRETRY 0x00800000UL    /**< Excess collision retry */
\ #define NATSEMI_TXCFG_MXDMA(x)    ( (x) << 20 )   /**< Max DMA burst size */
\ #define NATSEMI_TXCFG_FLTH(x) ( (x) << 8 )    /**< Fill threshold */
\ #define NATSEMI_TXCFG_DRTH(x) ( (x) << 0 )    /**< Drain threshold */

\ /** Receive Descriptor Pointer */
\ #define NATSEMI_RXDP 0x0030

\ /** Receive Descriptor Pointer High Dword (64-bit) */
\ #define NATSEMI_RXDP_HI_64 0x0034

\ /** Number of receive descriptors */
\ #define NATSEMI_NUM_RX_DESC 4

\ /** Receive buffer length */
\ #define NATSEMI_RX_MAX_LEN ( ETH_FRAME_LEN + 4 /* VLAN */ + 4 /* CRC */ )

\ /** Receive configuration register (32-bit) */
\ #define NATSEMI_RXCFG_32 0x34

\ /** Receive configuration register (64-bit) */
\ #define NATSEMI_RXCFG_64 0x38
\ #define NATSEMI_RXCFG_ARP 0x40000000UL    /**< Accept runt packets */
\ #define NATSEMI_RXCFG_ATX 0x10000000UL    /**< Accept transmit packets */
\ #define NATSEMI_RXCFG_ALP 0x08000000UL    /**< Accept long packets */
\ #define NATSEMI_RXCFG_MXDMA(x)    ( (x) << 20 )   /**< Max DMA burst size */
\ #define NATSEMI_RXCFG_DRTH(x) ( (x) << 1 )    /**< Drain threshold */

\ #define NATSEMI_RXCFG_MXDMA_DEFAULT NATSEMI_RXCFG_MXDMA ( 0x7 )

\ /** Drain threshold (in units of 8 bytes)
\  *
\  * Start draining after 64 bytes.
\  *
\  * Must be large enough to allow packet's accept/reject status to be
\  * determined before draining begins.
\  */
\ #define NATSEMI_RXCFG_DRTH_DEFAULT NATSEMI_RXCFG_DRTH ( 64 / 8 )

\ /** Receive Filter/Match Control Register */
\ #define NATSEMI_RFCR 0x0048
\ #define NATSEMI_RFCR_RFEN 0x80000000UL    /**< RX filter enable */
\ #define NATSEMI_RFCR_AAB  0x40000000UL    /**< Accept all broadcast */
\ #define NATSEMI_RFCR_AAM  0x20000000UL    /**< Accept all multicast */
\ #define NATSEMI_RFCR_AAU  0x10000000UL    /**< Accept all unicast */
\ #define NATSEMI_RFCR_RFADDR( addr ) ( (addr) << 0 ) /**< Extended address */
\ #define NATSEMI_RFCR_RFADDR_MASK NATSEMI_RFCR_RFADDR ( 0x3ff )

\ /** Perfect match filter address base */
\ #define NATSEMI_RFADDR_PMATCH_BASE 0x000

\ /** Receive Filter/Match Data Register */
\ #define NATSEMI_RFDR 0x004c
\ #define NATSEMI_RFDR_BMASK    0x00030000UL    /**< Byte mask */
\ #define NATSEMI_RFDR_DATA( value ) ( (value) & 0xffff ) /**< Filter data */

\ /** National Semiconductor network card flags */
\ enum natsemi_nic_flags {
\   /** EEPROM is little-endian */
\   NATSEMI_EEPROM_LITTLE_ENDIAN = 0x0001,
\   /** EEPROM layout is insane */
\   NATSEMI_EEPROM_INSANE = 0x0002,
\   /** Card supports 64-bit operation */
\   NATSEMI_64BIT = 0x0004,
\   /** Card supports 1000Mbps link */
\   NATSEMI_1000 = 0x0008,
\ };


base @ hex


\ Get hardware address from EEPROM as unsigned double
: hwaddr ( -- ud )
\ Read EEPROM contents */
nvs_read 
\ Extract MAC address from EEPROM contents */
;

\ Device reset
\ Command Register  NATSEMI_CR 0x0000

: soft-reset ( -- )
\ Initiate reset NATSEMI_CR_RST  0x00000100
    100 0 eth-io!

\ Wait for reset to complete 
    100 begin
        0 eth-io@ 100 and 
        over and while
             1 ms
             1-
     repeat
\ if counter is zero then timout occurred
    drop
;

\ NATSEMI_PTSCR 0x000c
\ NATSEMI_PTSCR_EELOAD_EN   0x00000004U
\ Reload configuration from EEPROM
: reloadr-config ( --)

\ Initiate reload */
  4 0c eth-io!
\ Wait for reload to complete 
    d# 100 begin
        0c eth-io@ 4 and 
        over and while
          1 ms 1-
     repeat
\ if counter is zero then timeout occurred
    drop
;

\ NATSEMI_CFG 0x0004
\ NATSEMI_CFG_LNKSTS    0x80000000 Link status
\ NATSEMI_CFG_SPDSTS1   0x40000000 Speed status bit 1
\ NATSEMI_CFG_MODE_1000 0x00400000 1000 Mb/s mode control
\ NATSEMI_CFG_PCI64_DET 0x00002000 PCI 64-bit bus detected
\ NATSEMI_CFG_DATA64_EN 0x00001000 64-bit data enable
\ NATSEMI_CFG_M64ADDR   0x00000800 64-bit address enable
\ NATSEMI_CFG_EXTSTS_EN 0x00000100 Extended status enable


: reset ( -- )      \ Reset hardware

  soft-reset reload-config
\ Configure 64-bit operation, if applicable 
  4 eth-io@ dup 4 and if
          8000 or 100 or dup 2000 and invert if
            1000 invert and
          then
  then
  4 eth-io!
;


\ Read link status */
: check-link ( -- ) 4 eth-io@  01f rshift ;
: full-duplex ( -- ) 4 eth-io@ 01d rshift 1 and ;

\ Enable or disable interrupts
: enable-intr ( -- )  1 18 ethio-io! ;
: disable-intr ( -- ) 0 18 eth-io! ;


 * Set perfect match filter address
: pmatch ( mac -- ) 


0 [if]
    for ( i = 0 ; i < ETH_ALEN ; i += sizeof ( *pmatch ) ) {

        /* Select receive filter register address */
        rfaddr = ( NATSEMI_RFADDR_PMATCH_BASE + i );
        rfcr = readl ( natsemi->regs + NATSEMI_RFCR );
        rfcr &= ~NATSEMI_RFCR_RFADDR_MASK;
        rfcr |= NATSEMI_RFCR_RFADDR ( rfaddr );
        writel ( rfcr, natsemi->regs + NATSEMI_RFCR );

        /* Write receive filter data */
        writel ( ( le16_to_cpu ( *(pmatch++) ) | NATSEMI_RFDR_BMASK ),
             natsemi->regs + NATSEMI_RFDR );
    }
}

[then]


drop
;

 \ Create descriptor ring
: create-ring ( -- ring )
\  Calculate descriptor offset

\ Allocate descriptor ring.  Align ring on its own size to
\     * ensure that it can't possibly cross the boundary of 32-bit
\    * address space.

;


: natsemi-refill-rx ( -- ) \ Refill receive descriptor ring

0 [if]
 
    while ( ( natsemi->rx.prod - natsemi->rx.cons ) < NATSEMI_NUM_RX_DESC ){

        /* Allocate I/O buffer */
        iobuf = alloc_iob ( NATSEMI_RX_MAX_LEN );
        if ( ! iobuf ) {
            /* Wait for next refill */
            return;
        }

        /* Check address is usable by card */
        address = virt_to_bus ( iobuf->data );
        if ( ! natsemi_address_ok ( natsemi, address ) ) {
            DBGC ( natsemi, "NATSEMI %p cannot support 64-bit RX "
                   "buffer address\n", natsemi );
            netdev_rx_err ( netdev, iobuf, -ENOTSUP );
            return;
        }

        /* Get next receive descriptor */
        rx_idx = ( natsemi->rx.prod++ % NATSEMI_NUM_RX_DESC );
        rx = &natsemi->rx.desc[rx_idx];

        /* Populate receive descriptor */
        if ( natsemi->flags & NATSEMI_64BIT ) {
            rx->d64.bufptr = cpu_to_le64 ( address );
        } else {
            rx->d32pad.d32.bufptr = cpu_to_le32 ( address );
        }
        wmb();
        rx->common.cmdsts = cpu_to_le32 ( NATSEMI_DESC_INTR |
                          NATSEMI_RX_MAX_LEN );
        wmb();

        /* Record I/O buffer */
        assert ( natsemi->rx_iobuf[rx_idx] == NULL );
        natsemi->rx_iobuf[rx_idx] = iobuf;

        /* Notify card that there are descriptors available */
        writel ( NATSEMI_CR_RXE, natsemi->regs + NATSEMI_CR );

        DBGC2 ( natsemi, "NATSEMI %p RX %d is [%llx,%llx)\n", natsemi,
            rx_idx, ( ( unsigned long long ) address ),
            ( ( unsigned long long ) address + NATSEMI_RX_MAX_LEN));
    }
}

[then]

;

: natsemi_open ( netdev -- ior ) \  Open network device

0 [if]

    struct natsemi_nic *natsemi = netdev->priv;
    int rc;

    /* Set MAC address */
    natsemi_pmatch ( natsemi, netdev->ll_addr );

    /* Create transmit descriptor ring */
    if ( ( rc = natsemi_create_ring ( natsemi, &natsemi->tx ) ) != 0 )
        goto err_create_tx;

    /* Set transmit configuration */
    writel ( ( NATSEMI_TXCFG_CSI | NATSEMI_TXCFG_HBI | NATSEMI_TXCFG_ATP |
           NATSEMI_TXCFG_ECRETRY | NATSEMI_TXCFG_MXDMA_DEFAULT |
           NATSEMI_TXCFG_FLTH_DEFAULT | NATSEMI_TXCFG_DRTH_DEFAULT ),
         ( natsemi->regs + ( ( natsemi->flags & NATSEMI_64BIT ) ?
                     NATSEMI_TXCFG_64 : NATSEMI_TXCFG_32 ) ) );

    /* Create receive descriptor ring */
    if ( ( rc = natsemi_create_ring ( natsemi, &natsemi->rx ) ) != 0 )
        goto err_create_rx;

    /* Set receive configuration */
    writel ( ( NATSEMI_RXCFG_ARP | NATSEMI_RXCFG_ATX | NATSEMI_RXCFG_ALP |
           NATSEMI_RXCFG_MXDMA_DEFAULT | NATSEMI_RXCFG_DRTH_DEFAULT ),
         ( natsemi->regs + ( ( natsemi->flags & NATSEMI_64BIT ) ?
                     NATSEMI_RXCFG_64 : NATSEMI_RXCFG_32 ) ) );

    /* Set receive filter configuration */
    writel ( ( NATSEMI_RFCR_RFEN | NATSEMI_RFCR_AAB | NATSEMI_RFCR_AAM |
           NATSEMI_RFCR_AAU ), natsemi->regs + NATSEMI_RFCR );

    /* Fill receive ring */
    natsemi_refill_rx ( netdev );

    /* Unmask transmit and receive interrupts.  (Interrupts will
     * not be generated unless enabled via the IER.)
     */
    writel ( ( NATSEMI_IRQ_TXDESC | NATSEMI_IRQ_RXDESC ),
         natsemi->regs + NATSEMI_IMR );

    /* Update link state */
    natsemi_check_link ( netdev );

    return 0;

    natsemi_destroy_ring ( natsemi, &natsemi->rx );
 err_create_rx:
    natsemi_destroy_ring ( natsemi, &natsemi->tx );
 err_create_tx:
    return rc;
}
[then]

;

: close ( -- )  \ Close network device

0 [if]
\ Mask transmit and receive interrupts
   0 NATSEMI_IMR eth-io!
\ Reset and disable transmitter and receiver
   NATSEMI_CR_RXR | NATSEMI_CR_TXR NATSEMI_CR eth-io!
\ Discard any unused receive buffers
    for ( i = 0 ; i < NATSEMI_NUM_RX_DESC ; i++ ) {
        if ( natsemi->rx_iobuf[i] )
            free_iob ( natsemi->rx_iobuf[i] );
        natsemi->rx_iobuf[i] = NULL;
    }

\ Destroy receive descriptor ring
    natsemi_destroy_ring ( natsemi, &natsemi->rx );

\ Destroy transmit descriptor ring */
    natsemi_destroy_ring ( natsemi, &natsemi->tx );

[then]

;


: transmit ( iobuf -- ) \ Transmit packet

0 [if]

{
    /* Check address is usable by card */
    address = virt_to_bus ( iobuf->data );

    /* Get next transmit descriptor */
    if ( ( natsemi->tx.prod - natsemi->tx.cons ) >= NATSEMI_NUM_TX_DESC ) {
        DBGC ( natsemi, "NATSEMI %p out of transmit descriptors\n",
               natsemi );
        return -ENOBUFS;
    }
    tx_idx = ( natsemi->tx.prod++ % NATSEMI_NUM_TX_DESC );
    tx = &natsemi->tx.desc[tx_idx];

    /* Populate transmit descriptor */
    if ( natsemi->flags & NATSEMI_64BIT ) {
        tx->d64.bufptr = cpu_to_le64 ( address );
    } else {
        tx->d32pad.d32.bufptr = cpu_to_le32 ( address );
    }
    wmb();
    tx->common.cmdsts = cpu_to_le32 ( NATSEMI_DESC_OWN | NATSEMI_DESC_INTR |
                      iob_len ( iobuf ) );
    wmb();

    /* Notify card that there are packets ready to transmit */
    writel ( NATSEMI_CR_TXE, natsemi->regs + NATSEMI_CR );

    DBGC2 ( natsemi, "NATSEMI %p TX %d is [%llx,%llx)\n", natsemi, tx_idx,
        ( ( unsigned long long ) address ),
        ( ( unsigned long long ) address + iob_len ( iobuf ) ) );

    return 0;
}
[then]

;


: poll-tx ( -- )        \ Poll for completed packets

0 [if]

\ Check for completed packets
    while ( natsemi->tx.cons != natsemi->tx.prod ) {

        /* Get next transmit descriptor */
        tx_idx = ( natsemi->tx.cons % NATSEMI_NUM_TX_DESC );
        tx = &natsemi->tx.desc[tx_idx];

        /* Stop if descriptor is still in use */
        if ( tx->common.cmdsts & cpu_to_le32 ( NATSEMI_DESC_OWN ) )
            return;

        /* Complete TX descriptor */
        if ( tx->common.cmdsts & cpu_to_le32 ( NATSEMI_DESC_OK ) ) {
            DBGC2 ( natsemi, "NATSEMI %p TX %d complete\n",
                natsemi, tx_idx );
            netdev_tx_complete_next ( netdev );
        } else {
            DBGC ( natsemi, "NATSEMI %p TX %d completion error "
                   "(%08x)\n", natsemi, tx_idx,
                   le32_to_cpu ( tx->common.cmdsts ) );
            netdev_tx_complete_next_err ( netdev, -EIO );
        }
        natsemi->tx.cons++;
    }
}
[then]

;


: poll-rx ( -- )        \ Poll for received packets

0 [if]

\ Check for received packets */
    while ( natsemi->rx.cons != natsemi->rx.prod ) {

        /* Get next receive descriptor */
        rx_idx = ( natsemi->rx.cons % NATSEMI_NUM_RX_DESC );
        rx = &natsemi->rx.desc[rx_idx];

        /* Stop if descriptor is still in use */
        if ( ! ( rx->common.cmdsts & NATSEMI_DESC_OWN ) )
            return;

        /* Populate I/O buffer */
        iobuf = natsemi->rx_iobuf[rx_idx];
        natsemi->rx_iobuf[rx_idx] = NULL;
        len = ( le32_to_cpu ( rx->common.cmdsts ) &
            NATSEMI_DESC_SIZE_MASK );
        iob_put ( iobuf, len - 4 /* strip CRC */ );

        /* Hand off to network stack */
        if ( rx->common.cmdsts & cpu_to_le32 ( NATSEMI_DESC_OK ) ) {
            DBGC2 ( natsemi, "NATSEMI %p RX %d complete (length "
                "%zd)\n", natsemi, rx_idx, len );
            netdev_rx ( netdev, iobuf );
        } else {
            DBGC ( natsemi, "NATSEMI %p RX %d error (length %zd, "
                   "status %08x)\n", natsemi, rx_idx, len,
                   le32_to_cpu ( rx->common.cmdsts ) );
            netdev_rx_err ( netdev, iobuf, -EIO );
        }
        natsemi->rx.cons++;
    }
}
[then]
;


\ Interrupt Status Register */
\ NATSEMI_ISR 0x0010
\ NATSEMI_IRQ_TXDESC    0x00000080UL    TX descriptor
\ NATSEMI_IRQ_RXDESC    0x00000002UL    RX descriptor 

\ Poll for completed and received packets
\ Interrupt Enable Register
\ NATSEMI_IER 0x0018
\ NATSEMI_IER_IE        0x00000001UL

: natsemi_poll ( -- )    \  Poll for link state.  
   check_link

\ Check for and acknowledge interrupts
    10 eth-io@  dup if 
       \ Poll for TX completions,
       dup 80 and if poll-tx then
       \ Poll for RX completions,
       dup 2 and if poll-rx then
    then
  \ Refill RX ring */
   refill-rx
;



base !

