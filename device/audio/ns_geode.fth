\ * ns_geode.c
\ *
\ * Copyright (c) 2001 Bruce R. Montague <brucem@mail.cruzio.com>
\ * The Geode "Audio Engine" has 6 PCI Bus Masters that can be
\ * considered independent DMA engines. Each is controlled by 3
\ * registers.  Each of these bus masters has a hard-wired function
\ * (they insert data into fixed AC'97 serial codec protocol "slots"):
\ *
\ * DMA 0 - 32-bit stereo output (PLAY device, 2 16-bit PCM channels).
\ * DMA 1 - 32-bit stereo input  (RECORD device, 2 16-bit PCM channels).
\ * DMA 2 - 16-bit mono output   (MONO-PLAY).
\ * DMA 3 - 16-bit mono input    (MONO-RECORD).
\ * DMA 4 - 16-bit mono output, codec slot programmable.
\ * DMA 5 - 16-bit mono input, codec slot programmable.
\ *
\ * The DMA engines are refered to in the code using:
\ *
\ * DMA 0 - DMA_PLAY_*
\ * DMA 1 - DMA_RECORD_*
\ * DMA 2 - DMA_PLAY_MONO_*
\ * DMA 3 - DMA_RECORD_MONO_*
\ *
\ * This driver assumes the codec is a National LM4548 AC'97 codec.
\ * The PCI Audio Function is accessed via the normal PCI configuration
\ * mechanism (I/O space 0xCF8 (address) and 0xCFC (data)). PCI
\ * Function 3 registers constitute the XpressAUDIO configuration
\ * registers, which are largely similar to those of any PCI function
\ * header. One of these, register 0x10, is the F3BAR register, the
\ * "Base Address" register. This contains the physical memory
\ * address of Audio control memory "in" the PCI audio device, that
\ * is, memory-mapped audio-specific registers. The command and
\ * status register of the codec and the registers that control each
\ * of the 6 bus engines are in these memory-mapped registers.
\ *
\ * Each PCM "channel" corresponds to a PCI bus master. The bus masters
\ * ("DMA engines") are controlled by arrays of "Physical Region
\ * Descriptors" (PRDs). Each element of a PRD array contains a
\ * physical memory pointer and a buffer length and flags word. The
\ * bus master is programmed with the physcial memory address of the
\ * PRD array via one of the memory-mapped bus-engine control registers.
\ * The bus master can process successive PRD entries automatically.
\ * Typically, a flag in each PRD entry will cause a bus master
\ * interrupt at the completion of the "DMA" corresponding to the
\ * PRD entry. The flags of the last PRD entry usually are used not
\ * to control a DMA operation, but instead to loop the bus master
\ * back to process the first element of the PRD array (thus supporting
\ * overlapped buffering).
\ *--------------------------------------------------------------

8     constant CODEC_STATUS_REG
h# 0c constant CODEC_CMD_REG

\ *- Audio Bus Master 0: 32-bit Output to codec, left/right 16-bit PCM channels
h# 20 constant DMA_PLAY_CMD
h# 21 constant DMA_PLAY_STATUS
h# 24 constant DMA_PLAY_PRD_ADR

\ *--- Audio Bus Master 1: 32-bit Input from codec, left/right 16-bit PCM channels.
h# 28 constant DMA_RECORD_CMD
h# 29 constant DMA_RECORD_STATUS
h# 2c constant DMA_RECORD_PRD_ADR

\ *--- Audio Bus Master 2: 16-bit PCM output to codec, mono.
h# 30 constant DMA_PLAY_MONO_CMD 
h# 31 constant DMA_PLAY_MONO_STATUS 
h# 34 constant DMA_PLAY_MONO_PRD_ADR

\ *--- Audio Bus Master 3: 16-bit PCM input from codec, mono.
h# 38 DMA_RECORD_MONO_CMD
h# 39 DMA_RECORD_MONO_STATUS  
h# 3c DMA_RECORD_MONO_PRD_ADR 

\ *--------------------------------------------------------------
\ * The "DMA engines" are connected to an AC'97 codec. The codec
\ * can be considered A/D and D/A converters for all the channels,
\ * with per-channel volume controls. The AC'97 spec defines a 
\ * number of registers in the codec (mostly volume controls, but
\ * also sample format and speed).
\ *
\ * The codec has a synchronous serial interface to the PCI audio
\ * controller managed via the PCI Audio Function memory-mapped 
\ * registers.
\ *
\ * All reads and writes of the 8-bit registers internal to the codec
\ * (that is, the AC'97 registers) are performed by writing 32-bit
\ * command words to the PCI-memory-mapped CODEC_CMD_REG. The top
\ * 8-bits of the command word indicate which register is to be read
\ * or written, and the top bit indicates if the operation is a read
\ * or write (1 is read). On a write, the bottom 16-bits of the
\ * CODEC_CMD_REG command word will be written over the serial link
\ * to the codec target register. A read from the codec puts the
\ * designated codec register into the low 16-bits of CODEC_STATUS_REG.
\ *--------------------------------------------------------------

\ CODEC_CMD_REG:
h# 10   constant  CODEC_CMD_VALID       \ Hw sets when cmd loaded, remains set
                                        \ till cmd is serially output to codec.
h# 0FF00FFFF0 constant CODEC_CMD_MASK   \ Codec reg num | reg value. 
 
/ CODEC_STATUS_REG:
h# 30000 constant CODEC_STATUS_VALID	\ Status (low 16-bits readable) AND "new". 
					\ "new" is set when low bits are written,
                                        \ prior to first read.
\ LM4548 AC97 compatible National Semiconductor codec registers and signatures.

0  constant LM4548_RESET 
2  constant LM4548_MASTER_VOLUME        
4  constant LM4548_LINE_OUT_VOLUME      
6  constant LM4548_MONO_VOLUME     
h# 0a constant LM4548_BEEP_VOLUME  
h# 0c constant LM4548_PHONE_VOLUME 
h# 0e constant LM4548_MIC_VOLUME   
h# 10 constant LM4548_LINE_IN_VOLUME 
h# 12 constant LM4548_CD_VOLUME 
h# 14 constant LM4548_VIDEO_VOLUME     
h# 16 constant LM4548_AUX_VOLUME       
h# 18 constant LM4548_PCM_OUT_VOLUME       
h# 1a constant LM4548_RECORD_SELECT        
h# 1c constant LM4548_RECORD_GAIN      
h# 20 constant LM4548_GENERAL_PURPOSE      
h# 22 constant LM4548_3D           
h# 26 constant LM4548_POWERDOWN        
h# 28 constant LM4548_EXTENDED_AUDIO       
h# 2a constant LM4548_EXTENDED_AUDIO_STATUS    
h# 2c constant LM4548_PCM_FRONT_DAC_RATE    \ Equiv to PCM's: AC97_REGEXT_FDACRATE h# 2c
h# 32  constant LM4548_PCM_ADC_RATE         \ Equiv to PCM's: AC97_REGEXT_LADCRATE h# 32
h# 7C constant LM4548_VENDOR_ID1       
h# 7E constant LM4548_VENDOR_ID2      

\  LM4548_RESET: 
h# 0d50 constant LM_RESET_VAL		\ Read feature vector from reset reg.

\ VENDOR_ID1 and VENDOR_ID2 values: 
h# 4E53 constant NSC_ID1 
h# 4331 constant NSC_ID2  

\ Debug enables
0 value F3BAR_TRACE

h# ffff constant GEODE_DEFAULT_BUFSZ 
h# ffff constant GEODE_MAX_BUFSZ

\ Use double-buffering for each channel.
3 constant NUM_PRD_ELEMS  \  Number of DMA descriptors + 1 entry for loop.

\ Structures.

struct sc_info;     \ Forward ref for sc_chinfo. */

\ * The sc_chinfo "ch_dtbl" pointer points to an array of PRD (Physical
\ * Region Descriptor) elements. Each channel has an array of these
\ * that describe the buffers to be "DMAed". PRD values need to be
\ * "really" written to memory. Careful, no struct padding, etc..
\ * Descriptors must be 2 32-bit words. Each descriptor has the following
\ * format:
\ *         dword prd_buffer     DMA buffer address, low 2 bits must be 0.
\ *         dword prd_length     Top 3 bits are EOT, EOP, LOOP, low 16 are buf len.
\ *
\ * EOT:  "End of Table" (last PRD elem)
\ * EOP:  "End of Page" (interrupt when finished processing this page (block))
\ * LOOP: "prd_buffer" contains physical address of next PRD element to process.\
\
\ * 3 channels, "stereo play" (output), "stereo record" (input),and "mono play".
\ * These correspond to hardwired PCI audio function hardware. 
\ * Channel buffers are allocated in "geode_pchan_init()". The buffering
\ * scheme for each channel is:
\ *
\ *   base----> [ Blk 0 ]
\ *             [ Blk 1 ]
\ * 
\ *  There are 3 PRD entries for each channel, with the following format:
\ *
\ *   addr:[prd_buffer ]-------> [Blk 0]
\ *    ^   [EOP, len   ]
\ *    |   [prd_buffer ]-------> [Blk 1]
\ *    |   [EOP, len   ]
\ *    \-<-[ addr      ]
\ *        [JMP        ]
\ *
\ * The addresses in the PRD entries are physical memory addresss.
\ *
\ * Although each channel (struct sc_chinfo) has a pointer (ch_dtbl)
\ * to its first PRD entry (that is, a pointer to its "addr:" in the
\ * above diagram), all PRDs for all channels are allocated successively
\ * from a single array, ("sc_info.sc_dtbl"). This array is allocated
\ * at the start of "geode_init()
\ * 
\ * Private channel info. One of these structures exists for each channel
\ * supported by the device, that is, each play or record data stream
\ * that can be doing "DMA" concurrently (each "PCI bus master", or "DMA engine").
\ *
\ * The driver allocates a single buffer containing a "block array"
\ * for each channel. Blocks are the "sub-buffers" that correspond
\ * to a single DMA operation. Each channel currently gets two
\ * contiguous blocks, thus supporting ping-pong double buffering.
\ * Blocks are numbered 0 origin (0,1). A large part of this driver's
\ * operation consists of synchronizing activation and completion of
\ * block DMA with requests to the PCM driver to process a block
\ * (either write a full buffer out to application level (receive, 
\ * record), or fill a buffer (send, play)).
\ *
begin-structure sc_chinfo
    field: ch_num        \ Channel number, matches DMA engine number.
    field: ch_run		\ 1=DMA in progress.
    field: ch_was_running \ DMA was aborted when last suspend occured.
    field: ch_speed      \ Channel speed.
    field: ch_fmt        \ Channel format.
    field: ch_sc;        \ Backptr to our driver's private device info.
    field: ch_channel	\ PCM channel that generated buffer.
    field: ch_snd_dbuf	\ PCM Control struct for buffer to be DMAed.
\ DMA blocks are in 1 buf attached to snd_dbuf
	field: ch_num_blks	\ Number of DMA blocks in buffer.
    field: ch_blk_size	\ Size of each block. An interrupt is triggered.
                        \ at the end of each block.
	field: ch_dtbl		\ Start PRD descriptor for this channel.
	field: ch_cur_dma_blk \ Num of blk just DMAed (or being DMAed).

    \ ----- Below are bus master specific for this channel.
    field: ch_last_prd_dma_adr \ Last PRD DMA address of operational DMA. 
    field: ch_dma_cmd	\ Offset of DMA_CMD PCI F3BAR audio register. 
    field: ch_dma_status	\         " DMA STATUS register. 
    field: ch_dma_prd_adr \        " address of DMA PRD register, contains.
                        \  physcial adr of next PRD to process.
	field: ch_dir		\ DMA_INPUT,DMA_OUTPUT
end-structure

\ ch_dir
2 constant  CH_INPUT  		\ Channel is an input (record) channel. 
2 constant  CH_OUTPUT       \ Channel is an output (play) channel.  


\ * 
\ * Geode audio device private data. There is one of these for each instance of the
\ * device (codec). In theory, there could be more than one codec. This driver currently does
\ * not support multiple codecs. This structure contains 3 sc_chinfo structures that control
\ * the 3 possible concurrent DMA operations of a single codec.
\ 
begin-structure sc_info
	field: sc_dev		\ Backpointer to our device.
	field: sc_type		\ Device id.
	field: sc_mmregs_base	\ Base virt adr of memory-mapped PCI audio regs.
	field: sc_parent_dmat	\ Our "parent" TAG for allocating DMAable memory.
							\ Phys mem requirements can be further restricted
							\ by creating child tags, if need be. 
	field: sc_parent_dtmap	\ Default DMAable mem MAP (requirements).
    field: sc_prd_dt_map		\ PRD descriptor table map.
	field: sc_reg
	field: sc_irq_res		\ Standard driver vars.
	field: sc_regtype
	field: sc_regid
	field: sc_irqid
	field: sc_ih
	field: sc_lock		\ Lock device for SMP.
	field: sc_buf_size	\ Total size of single buf attached to a channel.
                        \ This includes all the ping-pong DMA blocks.
    field: sc_ac97_codec \ An AC97 Codec KOBJ allocated by AC97_CREATE().
    field: sc_ch_num		\ Initialization counter, indexes sc_ch[].
    3 cells
    +fields sc_ch		\ Priv Channel info for PLAY, REC, and MONO REC channels.
    field: sc_dtbl		\ Single PRD descriptor table, divided for all channels.
						\ (this is the virtual address, not physical.)
end-structure

\ Globals.
sc_info allocate value sc


\ F3BAR access routines.

: audio_port_write_32 ( val reg -- )  f3bar# + ! ;

: audio_port_write_16 ( val reg -- ) f3bar# + w! ;

: audio_port_write_8 ( val reg -- ) f3bar# + c! ;

: audio_port_read_32 ( reg - u  ) f3bar# + @ ;

: audio_port_read_8 ( reg -- val ) f3bar# + c@ ;

\  Codec access routines.
\  Spin awaiting last command to be shifted out serially.
: await_codec_ready ( -- )
\     int   timeout;
\    u_int32_t status;
    d# 2000 0 do
		CODEC_CMD_REG audio_port_read_32		\ Read the CMD (not status) reg.
        CODEC_CMD_VALID over and if drop unloop exit then	\ If CMD_VALID, last cmd still
     loop
   \  is transmitting.
   CODEC_CMD_VALID and if ." codec cmd not ready." then 
 ;

\ Read the 32-bit F3BAR codec status register when it is valid.
\ This "status" is always one of the internal codec AC'97 registers. 

: get_codec_status ( -- val )
   500 udelay
   await_codec_ready
   d# 300 0 do
		CODEC_STATUS_REG audio_port_read_32
        CODEC_STATUS_VALID over and if leave then	\ Data (reg value) has been received.
   loop
   ." codec underflow? "
;

\ Write the codec command register. Must spin waiting for
\ any current codec command register serial transfer to complete. 

: set_codec_control (val -- )
    500  udelay
    await_codec_ready
    CODEC_CMD_MASK and
    CODEC_CMD_REG swap audio_port_write_32
	30 udelay
    await_codec_ready
;

\ Read a codec register ("codec_port") over the serial link, with the codec 
\ register specified as a 0-origin index. Returns the 8-bit AC97 reg from
\ the codec (as 16 bits). 

: codec_direct_reg_read ( codec_port -- w_val )
    dup >r d# 24 lshift 	\ High cmd byte is register index
    h# 80000000 or 	\ High cmd byte bit set is codec*
						\ register read cmd.
	begin
	 dup set_codec_control	\ Issue read command.
     get_codec_status		\ Read 32-bit F3BAR codec status reg result.
    \ Repeat if result is for wrong reg (ugly serial protocol )
         r@ over d# 24 rshift and h# 0ff and 0= while
    repeat 
    r> drop nip 	\ return AC97 codec reg value.
;

\ Write value to codec register specified by index. */
: codec_reg_write ( val reg -- )
    lshift d# 24 or		\ Hit bit of 0 means _write_
	dup  set_codec_control	\ Write specified codec register.
    get_codec_status drop

    h# 80000000 or set_codec_control		\ Read it back... 
    get_codec_status drop
;

\ Initialize codec to enable setting AD and DA rate (speeds), and set 
\ default codec AD and DA rate (22050 Hz).
  
: set_codec_AD_rate( -- )
\    u_int16_t b2
\ Enable Variable Rate Audio (VRA).*/
    h# 2a codec_direct_reg_read
    01 or 						\ VRA on (can now change speed).
    h# 2a codec_reg_write
\ Set default, 16-bit uint.
    h# 5622 h# 2C codec_reg_write	\ 22050 Hz D/A rate.
    h# 5622 h# 32 codec_reg_write	\ 22050 Hz A/D rate.
;

\ Polling and DMA routines.


USE_TIMER_POLL [if]
\ * Poll to see if any active DMA has completed, and if so fake an interrupt
\ * by calling the driver's interrupt routine. Polling for DMA completion is
\ * done by examining the PCI memory-mapped "DMA PRD address" register for
\ * each bus master. This register is updated to point to the "next PRD" 
\ * element upon completion of each DMA transfer.
\ *
\ * This routine is either called from "timeout()" or custom code in "hardclock()".

\
:static void 
do_audio_poll( void *foo ) {
    struct sc_chinfo    *ch;
    u_int32_t        cur_dma_adr;
    int          i;

    /* ." \n\n *** do_audio_poll!!!\n\n" ); */
    if( !geode_dma_poll ) goto end;

    for (i=0;i<3;i++) {         /* See if any channel DMA has completed.*/
        ch = &sc->sc_ch[i];

        cur_dma_adr =  audio_port_read_32( ch->ch_dma_prd_adr );  /* DMA PRD adr value.*/

        if( -1 == ch->ch_last_prd_dma_adr ) {
        ch->ch_last_prd_dma_adr = cur_dma_adr;
        } else {
        if( ch->ch_last_prd_dma_adr != cur_dma_adr ) {
            geode_intr( sc );       /* If completed, go handle it.*/
        }
        }
    }

end:;


/*
 * Start F3BAR audio PCI "DMA engines" (bus masters). Each channel has a 
 * dedicated bus master.
 *
 * A bus master is controlled by 3 mem-mapped regs in the PCI audio function. 
 * These are a PRD table address register, a command register, and a status register. 
 *
 * The DMA status register (one for each bus master) is read-to-clear, so the first
 * time bit 0x01 (End of Page) is read, it is cleared.
 *
 *---
 *
 * DMA Start. We always start at the 1st PRD descriptor, after which DMA
 * loops forever. ch_cur_dma_blk tracks the block being DMAed, so on a 
 * completion trigger it points to the "finished" block. 
 */
/*-----------------------------------------------------------------------*/
static void 
start_geode_dma( struct sc_chinfo *ch ) {
        u_int32_t       prd_phy_adr;
        u_int8_t        status;   

    #if USE_TIMER_POLL
    #if DMA_TRACE
    /* ." \n !!start DMA, geode_dma_poll=%x\n", geode_dma_poll ); */
    #endif
    #endif

    status = audio_port_read_8( ch->ch_dma_status ); /* DMA Status, Paranoid Read clears EOP flgs*/

    #if USE_TIMER_POLL
    geode_dma_poll    = 1;                  /* Start polling. */
    #endif

        prd_phy_adr = (u_int32_t)vtophys( ch->ch_dtbl );

        audio_port_write_32( ch->ch_dma_prd_adr, prd_phy_adr ); /* DMA PRD. Physical adr PRD tbl.*/

    ch->ch_cur_dma_blk      = 0;
    ch->ch_last_prd_dma_adr = prd_phy_adr;

    if( CH_OUTPUT == ch->ch_dir ) {
        audio_port_write_8(  ch->ch_dma_cmd, 0x01 );    /* DMA CMD. Bus Master ON. Starts write DMA.*/
    } else
    if( CH_INPUT == ch->ch_dir ) {
        audio_port_write_8(  ch->ch_dma_cmd, 0x09 );    /* DMA CMD. Bus Master ON. Starts read DMA.*/
    }

    ch->ch_run         = 1;
}


/* 
 * Stop a bus master; called on abort and at end of overall DMA operation
 * (that is, when there is no more data to be transfered).
 */
/*-----------------------------------------------------------------------*/
static void 
stop_geode_dma( struct sc_chinfo *ch  ) {
    u_int8_t status,cmd;

    status = audio_port_read_8( ch->ch_dma_status );/* DMA Status, Paranoid Read clears EOP flgs.*/
    cmd    = audio_port_read_8( ch->ch_dma_cmd );   /* DMA CMD. */

    cmd   &= h# FE;                  /* Clear enable bit. */

    audio_port_write_8(  ch->ch_dma_cmd, cmd ); /* DMA CMD. Write Clear enable bit. */
                            /* WORRY: can corrupt PCI state? */
    #if USE_TIMER_POLL
    geode_dma_poll = 0;             /* Stop polling. */
    #endif

    ch->ch_run    = 0;
}




\ * @@@ Implementation of the AC97 Interface (PCM codec support).

\ *--- Read standard AC'97 specification codec register. */
: geode_rdcd( reg -- val ) codec_direct_reg_readegno ; 


\ *--- Write standard AC'97 specification codec register. */
: geode_wrcd( val reg -- flg ) codec_reg_write  0 ;

\ @@@ Geode Audio Initializataion routines (support attach function).
\ *
\ * The attach function is "geode_pci_attach()". It is called to activate
\ * the driver, and in doing so uses the routines in this section.
\ *--- Verify the LM4548 codec's internal vendor ID. */
\ 

: verify_codec_present( --flg )

    LM4548_VENDOR_ID1 codec_direct_reg_read
    NSC_ID1 = 
    LM4548_VENDOR_ID2 codec_direct_reg_read
    NSC_ID2 = and
;

\ Reset an LM4548 codec, verify its there, and set defaults.
: reset_codec(  -- flg )
    0 LM4548_RESET codec_reg_write		\ Reset.
	LM4548_RESET   codec_direct_reg_read
    LM_RESET_VAL <> if ."  unexpected codec reset value" 0 exit then
    \  Just to be paranoid, PCM AC97 will do all this at mixer_init() call.
    h# 0808 LM4548_MASTER_VOLUME codec_reg_write
    8 LM4548_BEEP_VOLUME codec_reg_write		\  Enable PC beep.
    1
;


/* 
 * This routine is a delay after "enable_audio_irq()" until the 
 * codec registers "work".
 *
 * This is important since the ac97 PCM code uses the same technique to 
 * determine what channels exist, so we want it to find things...
 * That is, it writes 0x3F into AC'97 registers, and sees if it can 
 * read 0x3F back. If the register works, it restores the original
 * register value.
 * 
 * If this isn't done, sometimes the pcm driver will not find any channels
 * (the DELAY() settings can mask this).
 */
void
await_operational_codec_channel_regs( void ) {
    int       i;

    u_int16_t b2,b2_new;

    for (i=0;i<400;i++) {
        DELAY(250);

        b2 = codec_direct_reg_read( 0x2 );
        codec_reg_write( 0x2, h# 3F );
        b2_new = codec_direct_reg_read( 0x2 );

        /* ." \n b2=%x b2_new=%x\n", b2, b2_new ); */
        if( b2 != b2_new ) {
        codec_reg_write( 0x2, b2 );
        break;
        }
    }
}


/*  
 * Called from "geode_pci_attach()", the following is the first routine to hit 
 * the hardware, reset, verify it's there, set hardware defaults, and
 * allocate descriptor table memory that can be accessed by the controller. 
 *
 * A "TAG" can be considered a handle to requirements for DMAable memory.
 * A "MAP" is a handle to  memory allocated to the requirements of a specific
 * TAG. A MAP must be "loaded" before the memory can be accessed. (You can
 * think of a MAP as similar to a subset of page table entries).
 */
/*-----------------------------------------------------------------------*/
static int
geode_init( struct sc_info *sc ) {
    int byte_size;

    audio_port_write_32( CODEC_CMD_REG, 0 );    /* HW CMD: reset codec. */
    DELAY(100);

    if( !verify_codec_present() ) return( -1 );
    if( !reset_codec()          ) return( -1 );

    /* Get a "MAP" for hardware accessible memory as specified by a valid "TAG".
     * This MAP describes memory where the PRD table will go. This table contains 
         * PRD descriptors. Each descriptor contains 2 32-bit words, one of which is
         * a physical h/w mem addresses of a buffer to be DMAed. 
     * The PRD table is processed autonomously by the geode audio PCI hardware. 
         * The allocated MAP needs to be "loaded" before the memory can actually be used. 
     */
        if (bus_dmamem_alloc( sc->sc_parent_dmat,   /* DMA mem "tag" (rqmts) */
                  (void **)&sc->sc_dtbl,    /* Virtual address of allocated mem. */
                  BUS_DMA_NOWAIT,       /* get it now. */
                 &sc->sc_prd_dt_map)    /* Result MAP to use to reference this mem.*/
                ) return( ENOSPC );

        byte_size = (sizeof(u_int32_t) * 2)     /* Byte size of 1 PRD descritpor. */
                    * 
            (NUM_PRD_ELEMS * 3);        /* Allocate a PRD table for all */
                            /*  3 channels. */

    /* Actually "activate" physical memory buffer for device access via the TAG/MAP. */
    /* After this call, sc_dtbl can be used. */
        if (bus_dmamap_load( sc->sc_parent_dmat,    /* DMA mem TAG (rqmts) */
                 sc->sc_prd_dt_map,     /* DMA mem MAP of mem we want. */
                 sc->sc_dtbl,       /* Buffer virtual address. */
                 byte_size,         /* Buffer len. */
                 geode_setmap, NULL,    /* per buf seg callback/arg. */
                 0)             /* flags. */
               ) {
                bus_dmamem_free( sc->sc_parent_dmat,
                         (void **)&sc->sc_dtbl, 
                         sc->sc_prd_dt_map );
                return( ENOSPC );
        }

    /* ." \n\n *** dtbl=%x len=%d. padr=%x\n\n", 
       (int)sc->sc_dtbl, byte_size, vtophys( sc->sc_dtbl ) ); */

    set_codec_AD_rate();

    enable_audio_irq();

    DELAY(100);

    await_operational_codec_channel_regs(); /* enable_audio_irq() must require time... */

    return( 0 );                /* OK */
}

/* Called from geode_detach(), restore "normal" beep. */
/*-----------------------------------------------------------------------*/
static int
geode_uninit( struct sc_info *sc ) {

        codec_reg_write( LM4548_MASTER_VOLUME,    h# 0808 );
        codec_reg_write( LM4548_BEEP_VOLUME,      h# 0008 );     /* Enable PC beep. */

    return( 0 );
}




/*
 *-----------------------------------------------------------------------
 * @@@ Implementation of the Channel Interfaces.
 *-----------------------------------------------------------------------
 */


/*-----------------------------------------------------------------------*\
 * The Channel Interface Methods.
 *------------------------------
 * 
 *  The routines in this section are methods, that is, all have a pointer to a
 *  kernel kobj structure as their first argument.
 * 
 *  Communication between these methods and the PCM driver is largely via
 *  the snd_dbuf structure associated with a specific channel. Our methods must
 *  use access methods defined by the PCM "snd" class to access data from the 
 *  channel's "snd_dbuf".
 * 
 *  The "geode_pchan_init()" method returns our private channel info structure,
 *  (struct sc_chinfo *). This "opaque pointer" is then passed back to us,
 *  providing data context, when the other channel interface methods are called.
 * 
 *  The first set of methods here are used by all 3 channels - stereo play, 
 *  stereo recieve, and mono-play. Strictly speaking, each of the 3 instantiations
 *  of the channel methods could have its own set of implementations, but
 *  in practice this seems to lead to a lot of duplicate code.
 *----------------------------------------------------------------------*/


/*---- @@ Instantiation of the Play Channel. ---------------------------*/

/* 
 * Called by PCM when a "pcm_addchan()" call is made in "geode_attach()". 
 * The "devinfo" arg is the pointer passed in by "pcm_addchan()" as its last arg;
 * the "dir" arg is PCMDIR_PLAY or PCMDIR_REC. This routine is called
 * for all channels.
 *
 * This routine is called after the PCM driver has allocated a pcm_channel
 * structure. It is our responsibility to allocate the data buffer that is to
 * be used to service this channel, in conjunction with the associated snd_dbuf
 * control structure. This is done by calling "sndbuf_alloc()".
 */
/*-----------------------------------------------------------------------*/
static void *
geode_pchan_init( kobj_t obj, void *devinfo, struct snd_dbuf *b, struct pcm_channel *c, int dir )
{
        struct sc_info    *sc = devinfo;
        struct sc_chinfo  *ch;
    int        num;

    #if OP_TRACE
    ." \n[pchan]init\n" );
    #endif

    num = sc->sc_ch_num++;          /* For each channel instantiation,*/
        ch  = &sc->sc_ch[num];          /* point to our private channel data.*/

        ch->ch_sc   = sc;           /* Save backpointer to our private driver data,*/
        ch->ch_snd_dbuf = b;            /*  attach control buffer struct,*/
        ch->ch_channel  = c;            /*  and ptr to PCM channel struct.*/

    ch->ch_num      = num;
    ch->ch_run      = 0;
                        /* Get start of our PRD array.*/
    ch->ch_dtbl     = (u_int32_t *)sc->sc_dtbl + 
                (num * 
                    (NUM_PRD_ELEMS * 2 * sizeof(u_int32_t)) 
                );

    ch->ch_num_blks = 2;            /* Default is always dbl-buffering,*/
    ch->ch_blk_size = sc->sc_buf_size / 2;  /*  user can change latter. sc_buf_size */
                        /*  from pcm_get_buffersize() in "_attach()".*/
    ch->ch_cur_dma_blk = 0;         /* Current DMA target.*/    

        ch->ch_fmt      = AFMT_U16_LE;      /* Set default format and*/
        ch->ch_speed    = DSP_DEFAULT_SPEED;    /*  speed.*/

    /* Allocate a data buffer for DMA using associated DMA tag.*/
        if ( sndbuf_alloc( ch->ch_snd_dbuf,     /* For this control buffer*/
               sc->sc_parent_dmat,      /* using this TAG (mem rqmts).*/
               sc->sc_buf_size ) == -1) /* Allocate this size buffer.*/
                return( NULL );

        prd_filldtbl( ch );         /* Write default DMA descriptors.*/

    ch->ch_last_prd_dma_adr = -1;       /* No DMA adr yet.*/


    /* Channel num corresponds directly to fixed-function bus master (DMA engine). 
     * Save indices of correct PCI memory-mapped registers to control the channel's 
     * bus master.
     */
    switch( num ) {

        case 0:                     /* Stero, 16-bit*2, output */
        ch->ch_dir      = CH_OUTPUT;
        ch->ch_dma_cmd      = DMA_PLAY_CMD;     /* Offset of DMA_CMD audio reg. */
        ch->ch_dma_status   = DMA_PLAY_STATUS;      /* " DMA STATUS reg. */
        ch->ch_dma_prd_adr  = DMA_PLAY_PRD_ADR;     /* " address of DMA PRD table. */   
        break;

        case 1:                     /* Stero, 16-bit*2, input. */
        ch->ch_dir      = CH_INPUT;
        ch->ch_dma_cmd      = DMA_RECORD_CMD;       /* Offset of DMA_CMD audio reg. */
        ch->ch_dma_status   = DMA_RECORD_STATUS;    /* " DMA STATUS reg. */
        ch->ch_dma_prd_adr  = DMA_RECORD_PRD_ADR;   /* " address of DMA PRD table. */   
        break;

        case 2:                     /* Mono, 16-bit, output. */
        ch->ch_dir      = CH_OUTPUT;
        ch->ch_dma_cmd      = DMA_PLAY_MONO_CMD;    /* Offset of DMA_CMD audio reg. */
        ch->ch_dma_status   = DMA_PLAY_MONO_STATUS; /* " DMA STATUS reg. */
        ch->ch_dma_prd_adr  = DMA_PLAY_MONO_PRD_ADR;    /* " address of DMA PRD table. */   
        break;

    }

        return( ch );
}


/* 
 * Free up control buffer and DMA buffer - called after the channel 
 * is stopped. 
 */
/*-----------------------------------------------------------------------*/
static int
geode_pchan_free( kobj_t obj, void *data )
{
        struct sc_chinfo *ch = data;    /* Our channel */

    #if OP_TRACE
    ." \n[pchan]free\n" );
    #endif

        sndbuf_free( ch->ch_snd_dbuf );

        return( 0 );            /* OK */
}



/* 
 * Construct a 3-element PRD table to control bus-engine DMA. The first 2 PRD entries each
 * point to countiguous data buffers, while the last PRD entry points back to the first
 * PRD entry. The bus-engine thus runs in a loop. The EOP flag in the 2 data buffer
 * descriptors causes an interrupt when the corresponding DMA is complete. The
 * two buffers provide a double-buffering scheme in conjunction with the PCM driver.
 */
/*-----------------------------------------------------------------------*/
static  void
prd_filldtbl( struct sc_chinfo *ch ) {
    u_int32_t base;
  
        base = vtophys( sndbuf_getbuf( ch->ch_snd_dbuf ));  /* Get memory physical address */
                                /*  of DMA buffer. */
        ch->ch_num_blks = sndbuf_getsize( ch->ch_snd_dbuf ) / 
                      ch->ch_blk_size;  /* Num blks in DMA buf.*/

    if (ch->ch_num_blks != 2 ) {
        ch->ch_num_blks  = 2;
        ch->ch_blk_size  = sndbuf_getsize( ch->ch_snd_dbuf ) / ch->ch_num_blks;
    }

    /* ." \n prd_tbl: ch_blk_size=%d.\n", ch->ch_blk_size );*/
    
    ch->ch_dtbl[0] = base + (0 * ch->ch_blk_size);
    ch->ch_dtbl[1] = ( h# 40000000 | ch->ch_blk_size);   /* EOP (DMA completion interrupt)*/

    ch->ch_dtbl[2] = base + (1 * ch->ch_blk_size);
    ch->ch_dtbl[3] = ( h# 40000000 | ch->ch_blk_size);   /* EOP (DMA completion interrupt)*/

    /* 
     * Make the last entry "loop" back to the first descriptor. 
     * No interrupt or DMA buffer is associated with this entry. 
     */
    ch->ch_dtbl[4] = vtophys( ch->ch_dtbl );
    ch->ch_dtbl[5] = h# 20000000;    /* JMP (loop back to top of PRD descriptor table.)*/
                    /* NOTE: EOT and JMP bit cannot both be set,*/
                    /* if JMP, no EOT required.*/
}


/*
 * The "channel_setblocksize()" method returns the number of bytes expected
 * to be processed by a single DMA operation, that is, bytes processed
 * between DMA completion interrupts. This value will evenly divide
 * the total length of the buffer associated with the channel.
 *
 * This routine is a filter for the blocksize, which is passed in by the
 * PCM driver. If the blocksize selected and returned by this routine is
 * not the same as that passed in by PCM, this routine is responsible 
 * for calling "sndbuf_resize()" to keep the PCM driver consistent.
 *
 * This routine should return the closest possible block size less than
 * or equal to the input blocksize. This method will not be called when
 * the channel is running.
 */
/*-----------------------------------------------------------------------*/
static int
geode_pchan_setblocksize( kobj_t obj, void *data, u_int32_t blocksize )
{
        struct sc_chinfo *ch = data;
    #if OP_TRACE
    ." \n[pchan]setblocksize, blocksize=%d.\n", blocksize );
    #endif

    /* ." \n setblocksize, blocksize=%d.\n", blocksize ); */

        ch->ch_blk_size = blocksize;
        prd_filldtbl( ch );

        return( blocksize );
}


/*
 * The PCM driver, when it has output data to play or requires input
 * data, uses the channel buffers that have already been setup.
 *
 * To start playing, the PCM driver will fill the entire buffer
 * associated with the channel (that is, all play "blocks"), and
 * then will call the "channel_trigger" method with a "go" code of
 * PCMTRIG_START. The trigger method should start DMA operation,
 * with an interrupt after "ch_block_size" bytes have been DMAed
 * (that is, after one block in the buffer has been DMAed). The
 * first block to be DMAed (and interrupt at completion) will 
 * thus be block 0.
 *
 * On PCMTRIG_START, the DMA transfer to be activated is specified 
 * as block "sndbuf_getbuf()" with size "sndbuf_getsize()".
 */
/*-----------------------------------------------------------------------*/
static int
geode_pchan_trigger( kobj_t obj, void *data, int go )
{
        struct sc_chinfo *ch = data;

        /* struct sc_info    *sc = ch->ch_sc; */
    #if OP_TRACE
    ." \n[pchan]trigger, go=%x\n", go );
    #endif


        switch(go) {
            case PCMTRIG_START:          /* start at beginning of buffer */
            start_geode_dma( ch );
            break;

            case PCMTRIG_STOP:
            case PCMTRIG_ABORT:         /* stop operation */
             stop_geode_dma( ch );
             break;

            default:
            break;
        }

        return( 0 );                /* OK */
}


/* 
 * Get an OFFSET (not a pointer despite the name) to the next channel
 * block that the PCM driver is to fill (after DMA completion on play
 * channels), or copy (after DMA completion on record channels).
 *
 * The "channel_getptr()" method is called by the PCM driver's
 * "chn_intr()" routine, which is called by this driver's interrupt
 * routine, "geode_intr()". The "channel_getptr()" routine should
 * return the offset of the data block (within the overall channel
 * buffer) that the PCM driver is to process _next_.  For a
 * double-bufferd channel, this routine first returns the offset
 * of buffer 0, then 1, then 0, etc. When a play channel is started,
 * all blocks are filled before the first DMA is started.
 */
/*-----------------------------------------------------------------------*/
static int
geode_pchan_getptr(kobj_t obj, void *data)
{
        struct sc_chinfo *ch;
    int       pos;

        /* struct sc_info    *sc = ch->ch_sc; */
    #if OP_TRACE
    ." \n[pchan]getptr\n" );
    #endif

    ch  = data;
    pos = ch->ch_cur_dma_blk * ch->ch_blk_size;

    return( pos );      /* return current byte offset of channel.*/
}

/* 
 * Set the channel format (mono/stereo, 8-bit, 16-bit, etc.) from the
 * channel getcaps list. This method is not called if the channel running. The
 * Geode audio channels are "fixed-format". 
 * $$$ Shouldn't this routine check what it's being asked to do and return
 * an error code if it doens't match the fixed setting?
 */
/*-----------------------------------------------------------------------*/
static int
geode_pchan_setformat( kobj_t obj, void *data, u_int32_t format )
{
    #if OP_TRACE
    ." \n[pchan]setformat\n" );
    #endif

        /* return 0 - format _must_ be settable if in list */
        return( 0 );
}


/* 
 * Set the codec speed (rate) for a given channel. This method determines
 * which register is to be written (ADC_RATE or DAC_RATE) and then
 * calls the PCM driver's ac97_setrate() routine to do the work.
 */
/*-----------------------------------------------------------------------*/
static int
geode_pchan_setspeed( kobj_t obj, void *data, u_int32_t speed )
{
        struct sc_chinfo  *ch;
        struct sc_info    *sc;
    int                reg;

    #if OP_TRACE
    ." \n[pchan]setspeed\n" );
    #endif

        ch = data;
        sc = ch->ch_sc;
    reg = LM4548_PCM_FRONT_DAC_RATE;

    if( CH_INPUT == ch->ch_dir ) {
        reg = LM4548_PCM_ADC_RATE;
    } else
    if( CH_OUTPUT == ch->ch_dir ) {
        reg = LM4548_PCM_FRONT_DAC_RATE;
    }

    /* ac97_create() writes the codec speed using the specified reg. */

    ch->ch_speed = (u_int)ac97_setrate( sc->sc_ac97_codec, reg, speed );
    
        return( ch->ch_speed );
}

/*-----------------------------------------------------------------------*\
 * @@@ The interrupt handler. 
 *---------------------------
 *
 * Interrupt notification is by device (PCU audio function), not
 * channel, so the interrupt routine needs to figure out what channel
 * caused the interrupt. When the interrupt routine runs, multiple
 * channels may be ready for additional work.
 * 
 * This routine is responsible for updating the channel's "current"
 * DMA buffer indicator (the "ch_cur_dma_blk" index). The PCM driver
 * determines which DMA buffer it should read or write by calling
 * the "channel_getptr()" method when the interrupt routine calls
 * "chn_intr()". Thus, buffer bookkeeping should happen after the
 * "chn_intr()" call in the following interrupt code.
 * 
 * The PRD DMA descriptor table that corresponds to the data buffers
 * never needs to change (the DMA engine just keeps processing it
 * "in a circle"). The interrupt routine uses the change in the
 * "processed PRD address" contained in the memory-mapped audio
 * function registers of the appropriate bus-master to detect the
 * end of a "DMA" operation.
 * 
 * The EOP bit needs to be read after DMA completion, or the bus
 * master (and associated transfer) will halted on "double EOP
 * write". The EOP apparently is not to be trusted due to a hardware
 * bug (it cannot be used to detect end of the DMA for the respective
 * bus-master).
 */
/*-----------------------------------------------------------------------*/
static void
geode_intr( void *p ) {
    struct sc_info   *sc;
    struct sc_chinfo *ch;
    u_int32_t     cur_dma_adr;
    u_int8_t          status;
    int               i;

    sc = (struct sc_info *)p;

    /* ." \n GINT!\n" ); */

    for (i=0; i<3; i++) {
        ch = &sc->sc_ch[i];

        if( ch->ch_run ) {      /* If DMA is running on channel...*/

        cur_dma_adr = audio_port_read_32( ch->ch_dma_prd_adr ); /* Current DMA PRD.*/
        if( cur_dma_adr != ch->ch_last_prd_dma_adr ) {

            ch->ch_last_prd_dma_adr = cur_dma_adr;

            status = audio_port_read_8( ch->ch_dma_status );    /* Clear EOP.*/

            chn_intr( ch->ch_channel );             /* PCM generic int routine.*/

            /* Interrupt has been processed, update pointer to next DMA buffer.*/
            ch->ch_cur_dma_blk++;
            if (ch->ch_cur_dma_blk >= ch->ch_num_blks ) {
            ch->ch_cur_dma_blk = 0;
            }

        } /* Endif new DMA PRD adr (DMA blk completion).*/
        }   /* Endif channel running DMA.*/
    } /* End for all 3 channels.*/

}




/* 
 * The driver's attach function is called if this driver was the
 * winner of all device probes for a particular device.
 * 
 * A private sc_info structure is allocated to manage the device
 * (these structures are called a "softc" (software context) in
 * FreeBSD terminology), resources are set up to do direct hardware
 * access, and the interrupt routine is enabled. A call is made to
 * instantiate the AC97 interface used by the parent PCM driver to
 * interact with the codec (mixer). Calls are made to instantiate
 * a PCM Channel interface for each supported audio channel (these
 * interact with the PCI PCM DMA engine hardware).
 * 
 * FreeBSD uses virtualizaton functions to access I/O related
 * hardware resources. Hardware such as an I/O port or memory-mapped
 * register is specified via a "virtual resource" consisting of a
 * (type, index) pair.
 *
 * PCI function header registers are accessed by index number. PCI
 * function headers have a standard format with the Command register
 * at index 2 and the Status register at index 3.
 * 
 * The device structure passed in is known the "pcm" device, and
 * its parent device is "pci" (usually pci0).
*/
/*-----------------------------------------------------------------------*/
static int geode_pci_attach( device_t dev ) {
    u_int32_t         data;
    int               nplay;        /* Number of play (output) DMA channels.*/
    int               nrec;      /* Number of record channels.*/
    char              status[ SND_STATUSLEN ];
    #if !USE_TIMER_POLL
    int               err;
    #endif

    /* device_printf( dev, "\n geode_pci_attach()\n" );*/

    #if OP_TRACE
    ." \n\n**** geode Attach!\n\n" );
    #endif

    #if USE_TIMER_POLL
    geode_dma_poll  = 0;
    #endif

    #if USE_TIMER_POLL
    #if HARDCLOCK_POLL
        geode_audio_poll =  do_audio_poll;
    #else
    timeout( do_audio_poll, (void *)0, 1 ); /* Every tick.*/
    #endif
    #endif

    /* Allocate and zero this device's sc_info (private softc).*/
    if((sc = malloc( sizeof(*sc), M_DEVBUF, M_WAITOK|M_ZERO)) == NULL ) {
        device_printf( dev, "cannot allocate softc\n" );
            return( ENXIO );
    }

    sc->sc_ac97_codec  = 0; /* Should be zero, but...*/
        sc->sc_reg         = 0;
        sc->sc_ih          = 0;
        sc->sc_irq_res     = 0;
        sc->sc_parent_dmat = 0;
        sc->sc_lock        = 0;
    sc->sc_ch_num      = 0;

    /*-- Lock device, store dev backpointer and PCI hdr ID.*/

    sc->sc_lock = snd_mtxcreate( device_get_nameunit(dev), "geode sound softc" );
    sc->sc_dev  = dev;
    sc->sc_type = pci_get_devid( dev ); 
    /*------------------- Setup interrupt ------------------*/
#if !USE_TIMER_POLL
    /*--- Setup interrupt handler.*/
    #if 0
    ." \n dev name =<%s>\n", device_get_nameunit(dev) );
    ." \n parent dev name =<%s>\n", device_get_nameunit( device_get_parent(dev) ) ); 
    #endif
    /* Allocate an IRQ 5 interrupt resource so we can hang our handler off the IRQ 5 list.*/
    /* Put our "geode_intr()" interrupt handler on the chain of IRQ 5 handlers.
     * The "snd_setup_intr()" routine is a wrapper for "bus_setup_intr()".
     */
    err = snd_setup_intr( dev, sc->sc_irq_res, INTR_MPSAFE, geode_intr, sc, &sc->sc_ih );
    if( err ) {
        device_printf( dev, "unable to setup interrupt: %d.\n", err );
        goto bad;
    }
#endif
    /* 
     * The following sets the "device buffersize" and stores it in "sc_buf_size".
     * The device buffersize is the overall buffer size that the PCM driver 
     * creates when any channel associated with the device "dev" is created. All 
     * other DMA buffers that this driver works with are calculated based on sc_buf_size. 
     *
     * The "pcm_getbuffersize()" arguments are (dev, <hint<min,default,max>>).
     * The actual size is set to "default", unless an IVAR resource named 
     * "buffersize" exists for the "dev" (the FreeBSD driver system manages
     * a "space" of dynamic variables named by "index" integers, these are 
     * sometimes called IVARS). 
     */

        sc->sc_buf_size = pcm_getbuffersize(dev, 4096, GEODE_DEFAULT_BUFSZ, GEODE_MAX_BUFSZ);

    /* ." \n sc_buf_size=%d.\n", sc->sc_buf_size  ); */

    /*
     * Setup to allocate DMAable memory. A "tag" is a handle to memory that meets DMA
     * hardware restrictions. Tags can inherit restrictions from parent tags. 
     * Arguments are confusing. 
     *  low-addr - for bus_dmamem_alloc, allocate below this address. 
     *  low-addr/highadr - for bus_dmamap_create, 
     *                    pages below low-addr can be mapped, and pages above highadr
     *                    can be mapped. If a filter func is present, pages between
     *                    low and high adr might be mappable, if the filter func returns 1.
     */
                              
    if ( bus_dma_tag_create( NULL,            /* Parent. */
                 8,           /* Alignment in bytes (thus, 64-bit long)*/
                 64 * 1024,       /* Span bndary. Can't DMA across 64K adr*/
                              /* low/high addrs that CANT be accessed: */
                 BUS_SPACE_MAXADDR_32BIT, /* lowaddr (dont alloc above this)*/
                 BUS_SPACE_MAXADDR,       /* highaddr(cant access above this)*/
                 NULL, NULL,          /* filter, filterarg (per page Y/N)*/
                 sc->sc_buf_size,     /* Maxsize in single alloc.*/
                 1, h# 0000FFFF,       /* Num scat/gath segs, Max such seg size.*/
                 0,           /* flags*/
                &sc->sc_parent_dmat )    /* Returned TAG for DMA mem ops.*/
         != 0
       )  {
        device_printf( dev, "unable to create dma tag\n" );
        goto bad;
    }


    /* Do detailed hardware initialization. Among other things, this 
     * allocates PRD (DMA descriptor) buffers for all channels. 
     */

    if (geode_init( sc ) == -1 ) {
        device_printf( dev, "unable to init audio." );
        goto bad;
    }

    /*--------  Now bring up the glue to the generic PCM audio driver. -------*/


    /*--- Instantiate a PCM AC97 interface for standard mixer operations.*/

    sc->sc_ac97_codec = AC97_CREATE( dev, sc, geode_ac97 );
    if (NULL == sc->sc_ac97_codec ) goto bad;

    /* Use the AC97 interface to do codec (mixer) initialization. The PCM "mixer_init()"
     * routine uses our AC97 interface routines to init the LM4548 codec. 
     */
    mixer_init( dev, ac97_getmixerclass(), sc->sc_ac97_codec ); /* Calls geode_initcd()*/

    nplay = 1;                          /* Number of channels: */
    nrec  = 2;

    /*
     * Finally, link the device, our softc, and the number of channels we will
     * allocate all together with "pcm_register()". The 2nd argument is the 
     * "opaque ptr" passed back to our channel methods (our own softc). 
     */

    if( pcm_register( dev, sc, nplay, nrec ) ) goto bad;

    /* 
     * Now allocate all the PCM channels we will use and attach them to "dev".
     * The following 3 calls to "pcm_addchan()" result in 3 "geode_pchan_init()"
     * calls in the same order, corresponding to the PLAY, REC, and MONO REC channels.
     * The correct instantiated channel interface and our softc are connected to
     * each channel created by PCM.
     */
    pcm_addchan( dev, PCMDIR_PLAY, &geode_pchan_class, sc );    /* Calls geode_pchan_init()*/
    pcm_addchan( dev, PCMDIR_REC,  &geode_rchan_class, sc );    /* Calls geode_pchan_init()*/
    pcm_addchan( dev, PCMDIR_REC,  &geode_rmono_chan_class, sc );   /* Calls geode_pchan_init()*/



        /*----------------------------------------------------------------
     * Update sndstat driver text.
     *
     * The following snprintf() supports the "dev/sound/pcm/sndstat.c"
         * driver. This is a small non-loadable driver for a virtual device
     * that simply contains a text buffer describing the operational
     * audio hardware. The "snprintf()" adds formatted text to this 
     * buffer. The user can display the buffer via ">cat /dev/sndstat".
     *----------------------------------------------------------------
         */

    #if USE_TIMER_POLL
    snprintf( status, SND_STATUSLEN, " at memory 0x%lx ",
           rman_get_start(sc->sc_reg) );
    #else
    snprintf( status, SND_STATUSLEN, " at memory 0x%lx irq %ld",
           rman_get_start(sc->sc_reg), rman_get_start(sc->sc_irq_res) );
    #endif
    pcm_setstatus( dev, status );

    #if 0
    dmp_codec_regs();
    #endif
    
    return( 0 );

bad:;
        if( sc->sc_ac97_codec )
            ac97_destroy( sc->sc_ac97_codec );

    geode_free_all( dev, sc );

    return( ENXIO );
}


\ *---------------------------------------------------------------
\ * @@@ Debug Routines.
\ *---------------------------------------------------------------
\ * Display the PCI Function 3 Header registers. These registers largely
\ * follow the format specified for any PCI device, that is, the standard
\ * PCI Header. Register 0x10 in the Header is the memory address at which
\ * the device specific registers appear on the PCI bus.
\ *-----------------------------------------------------------------------*/

: dmp_audio_pci_hdr( pcidev --  )
\ hex  10 0 12 3 pic-dev     is the audio pci index
  ." \n  **** AUDIO PCI HDR ***" cr
\ 00-01h. Vendo ID. 
   0 over pci-w@  ." -->Vendor ID =" .  cr
\ 02-03h. Dev ID. 
   2 over pci-w@  ." -->Dev ID =" . cr 
\ 04-05h. PCI CMD.
   4 over pci-w@  ." -->PCI cmd =" . cr 
\ 06-07h. PCI Status.
   6 over pci-w@  ." -->PCI status =" . cr 
\ 08h.    Dev revision ID. 
   8 over pic-c@  ." -->Dev revision =" . cr 
\ 09-0Bh. PCI Class. 
   8 over pci@   ." -->PCI class =" . cr 
\ 0Ch.    PCI cache line size.
\ 0dh.    PCI latency.
   h# 0d over pci-c@ ." -->PCI latency =" . cr
\ 0Eh.    PCI Header type.
   h# 0e over pci-c@ ." -->PCI header type =" . cr
\ 0Fh.    PCI BIST.
   h# 0f over pci-c@ ." -->BIST =" . cr
\ 10-13h. BASE REGISTER --> Geode Audio Regs are mem mapped at this loc.
   h# 10 swap pci@  ." --> Register Base address =" . cr
; 


\ * Display the memory-mapped PCI Function 3 registers. These registers in
\ * the PCI audio function appear in memory at the (fixed) location specified
\ * by the BAR register in the PCI Function 3 header. The LM4548 codec (mixer)
\ * is accessed via a synchronous serial line as controlled by the codec
\ * command and status registers at offsets 0x0C and 0x08.
\ *-----------------------------------------------------------------------*/

: dmp_audio_mem_regs( audio_regs_adr --  )    \ FBAR + memory offset
   cr ." **** MEM-MAPPED AUDIO REGS at " . cr
   dup @ ." --> CODEC GPIO status =" . cr
   4 over + @  ." --> CODEC GPIO control =" . cr
   8 over + @  ." --> CODEC status =" . cr 
   h# 0c over + @  ." --> CODEC command =" . cr
   h# 10 over + w@  ." --> Audio SMI src =" . cr
   h# 14 over + @  ." --> I/O trap SMI =" . cr
   h# 18 over + w@ ." --> I/O trap SMI enable =" . cr
   h# 1a over + w@ ." --> Internal IRQ enable=" . cr
   h# 1c over + @  ." --> Internal IRQ control=" .cr 
   h# 20 over + c@ ." --> DMA 0 cmd =" . cr
   h# 21 over + c@ ." --> DMA 0 SMI status =" . cr
   h# 24 over + @  ." --> DMA 0 PRD table adr =" . cr
;


\ *--- Display the registers internal to the LM4548 codec.
: dmp_codec_regs( -- )
  cr ." **** CODEC REGS ***" cr
  LM4548_RESET           codec_direct_reg_read ." Reset          =" .
  LM4548_MASTER_VOLUME   codec_direct_reg_read ." master Volume  =" .
  LM4548_LINE_OUT_VOLUME codec_direct_reg_read ." Line Out Volume=" .
  LM4548_MONO_VOLUME     codec_direct_reg_read ." Mono Volume    =" .
  LM4548_BEEP_VOLUME     codec_direct_reg_read ." Beep Volume    =" .
  LM4548_PHONE_VOLUME    codec_direct_reg_read ." Phone Volume   =" .
  LM4548_MIC_VOLUME      codec_direct_reg_read ." Mic Volume     =" .
  LM4548_LINE_IN_VOLUME  codec_direct_reg_read ." Line in Volume =" . 
  LM4548_CD_VOLUME       codec_direct_reg_read ." Line CD Volume =" . 
  LM4548_VIDEO_VOLUME    codec_direct_reg_read ." Video Volume   =" .
  LM4548_AUX_VOLUME      codec_direct_reg_read ." Aux Volume     =" .
  LM4548_PCM_OUT_VOLUME  codec_direct_reg_read ." PCM out volume =" .
  LM4548_RECORD_SELECT   codec_direct_reg_read ." record select  =" .
  LM4548_RECORD_GAIN     codec_direct_reg_read   ." record gain    =" .
  LM4548_GENERAL_PURPOSE codec_direct_reg_read ." general purpose=" .
  LM4548_3D              codec_direct_reg_read ." National 3D    =" . 
  LM4548_POWERDOWN       codec_direct_reg_read ." Powerdown      =" . 
  LM4548_EXTENDED_AUDIO  codec_direct_reg_read ." Extended audio =" .
  LM4548_EXTENDED_AUDIO_STATUS
                         codec_direct_reg_read ." Extended audio status=" .
  LM4548_PCM_FRONT_DAC_RATE
                         codec_direct_reg_read ." PCM DAC rate   =" .
  LM4548_PCM_ADC_RATE    codec_direct_reg_read ." PCM ADC rate   =" . 
  LM4548_VENDOR_ID1      codec_direct_reg_read ." Vendor ID1     =" .
  LM4548_VENDOR_ID2      codec_direct_reg_read ." Vendor ID2     =" .

;


