/*
 * <LIC_AMD_STD>
 * Copyright (c) 2004 Advanced Micro Devices, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 * The full GNU General Public License is included in this distribution in the
 * file called COPYING
 * </LIC_AMD_STD>
 * <CTL_AMD_STD>
 * </CTL_AMD_STD>
 * <DOC_AMD_STD>
 * File Contents: This file contains the function definitions to access the  
 *                CS5530  I/O compaion.
 *
 * SubModule:     Display Data Channel support.
 * </DOC_AMD_STD>
 */


#include "ddc_5530.h"
#include "gfx_regs.h"

/*----------------------------------------------------------------------------
 * init_5530:
 *
 * Description	  : This function is called to configure the DDC Serial Data
 *                    line to probe the monitor.
 *
 * Parameters     : None
 *
 * Returns		  : None
 *
 * Comments       : This function  should be invoked before readbyte or write
 *                   byte routines.
*----------------------------------------------------------------------------
*/
void init_5530()
{
	RETAILMSG( 0,(TEXT("setup\n")));
	SetSDA_5530();    /* this function connfigure data line */
	WaitSCLHigh();    /* Waiting for Serial Clock Line go high*/
	ClearSCL_5530();  /*  Clearing the clock line */
	WaitSCLHigh();
	ClearSCL_5530();
	WaitSCLHigh();
	requestData_5530(); /* sending the data request */
	ReadByte_5530(0);   /* Read the monitor data */
	StopBit_5530();     /* Setting the stop bit */
}

/*----------------------------------------------------------------------------
 * ReadByte_5530
 *
 * Description	  : This function reads the data bit by bit and setting ack
 *                  after the one byte read.
 *
 * Parameters.
 *       ack      : Gets the one byte data.
 * Returns		  : Returning one byte information after read bit.
 *
 * Comments       : This function  should be invoked before readbyte or write
 *                  byte routines.
*----------------------------------------------------------------------------
*/
unsigned char ReadByte_5530(int ack)
{
	unsigned char val = 0;
	int i;
	RETAILMSG( 0,(TEXT("getByte\n")));
	for (i = 0; i < 8; i++) {
		val <<= 1;
		val |= ReadBit();
	}

	Ack_5530(ack);
	return val;
}

/*---------------------------------------------------------------------------
 * StartBit_5530
 *
 * Description	  : This function sending out the start bit of serial data
 *
 *
 * Parameters     : None
 *
 * Returns		  : None
 *
 * Comments       : This function indicates the begining ddc seiral data.
 *
*----------------------------------------------------------------------------
*/
void StartBit_5530()
{
	RETAILMSG( 0,(TEXT("startBit\n")));
	wait(DEFAULTDELAY);
	SetSDA_5530();
	wait(DEFAULTDELAY);
	WaitSCLHigh();
	wait(DEFAULTDELAY);
	ClearSDA_5530();
	wait(DEFAULTDELAY);
}

/*---------------------------------------------------------------------------
 * StopBit_5530.
 *
 * Description	  : This function sending out the stop bit of serial data
 *
 *
 * Parameters     : None
 *
 * Returns		  : None
 *
 * Comments       : This function indicates the end ddc seiral data.
 *
*----------------------------------------------------------------------------
*/
void StopBit_5530()
{
	RETAILMSG( 0,(TEXT("stopBit\n")));
	wait(DEFAULTDELAY);
	WaitSCLHigh();
	wait(DEFAULTDELAY);
	SetSDA_5530();
	wait(2);
}

/*---------------------------------------------------------------------------
 * Ack_5530:
 *
 * Description	  : This sends out the ack data
 *
 * Parameters     :
 *        flag    : Value read in ReaByte routine is passed
 * Returns		  : None
 *
 * Comments       : None
*----------------------------------------------------------------------------
*/
void Ack_5530(int flag)
{
	RETAILMSG( 0,(TEXT("ACK\n")));
	wait(DEFAULTDELAY);
	ClearSCL_5530();
	wait(DEFAULTDELAY);
	if (flag) {
		ClearSDA_5530();
	} else {
		SetSDA_5530();
	}
	WaitSCLHigh();
	ClearSCL_5530();
	wait(DEFAULTDELAY);
	SetSDA_5530();
	RETAILMSG( 0,(TEXT("ACK Exit\n")));
}

/*---------------------------------------------------------------------------
 * waitForAck:
 *
 * Description	  : This routine waits the ack SDL data.
 *
 * Parameters     : none
 *
 * Returns		  : None
 *
 * Comments       : None
*----------------------------------------------------------------------------
*/
int waitForAck_5530()
{
	unsigned long target=0;
	int i = 0;

	RETAILMSG( 0,(TEXT("waitForACK\n")));
	wait(DEFAULTDELAY);
	ClearSCL_5530();
	wait(DEFAULTDELAY);
	SetSDA_5530();

#ifndef _WIN32_WCE
	/* Do this for two times (2 uSec) */
	for (i=0; i<2; i++) {
		wait(DEFAULTDELAY);
		if (GetSDA_5530() == 0) {
			wait(DEFAULTDELAY);
			WaitSCLHigh();
			return 1;
		} 
	}
	/* Time Out */
	wait(DEFAULTDELAY);
	WaitSCLHigh();
	return 0;
#else
	target = GetTickCount() + 2;
	for (;;) {
		wait(DEFAULTDELAY);
		if (GetSDA_5530() == 0) 
		{
			wait(DEFAULTDELAY);
			WaitSCLHigh();
			return 1;
	    /* } else if(endtime > target) { */
		} else if(GetTickCount() > target) 
		{
			wait(DEFAULTDELAY);
			WaitSCLHigh();
			return 0;
		}
	}
#endif
}

/*---------------------------------------------------------------------------
 * requestData.
 *
 * Description		: This routine request the data and configure SDA to Rx mode.
 *
 * Parameters		: None   
 *
 * Returns			: None
 *
 * Comments         : None
*----------------------------------------------------------------------------
*/

void requestData_5530() 
{
	RETAILMSG( 0,(TEXT("requestData\n")));
	StartBit_5530();
	SendByte(0xa0);
	SendByte(0);
	StopBit_5530();

	StartBit_5530();
	SendByte(0xa1);
}

/***************************************************************
* H/W DEPENDENT LOW LAYER ROUTINES
***************************************************************/
/*---------------------------------------------------------------------------
 * startXfer_5530.
 *
 * Description	  : This routine is called to sets SDA to high state when the
 *                    transfer starts.
 * Parameters     : None
 * Returns		  : None
 *
 * Comments       : SCL should be high when toggling.
*----------------------------------------------------------------------------
*/
void startXfer_5530()
{
	unsigned long data = READ_VID32(CS5530_DISPLAY_CONFIG);
	RETAILMSG( 0,(TEXT("startXfer\n")));
	data &= ~CS5530_DCFG_DDC_SCL;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
	data &= ~CS5530_DCFG_DDC_SDA;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
	data |= CS5530_DCFG_DDC_SCL;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
	data |= CS5530_DCFG_DDC_SDA;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
}
/*---------------------------------------------------------------------------
 * endXfer_5530.
 *
 * Description	  : This routine is called to sets SDA to low state when the
 *                  transfer ends.
 *
 * Parameters     : None
 * Returns		  : None
 *
 * Comments       : SCL should be high when toggling.
*----------------------------------------------------------------------------
*/

void endXfer_5530() 
{
	unsigned long data = READ_VID32(CS5530_DISPLAY_CONFIG);
	RETAILMSG( 0,(TEXT("endXfer\n")));
	data &= ~CS5530_DCFG_DDC_SCL;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
	data |= CS5530_DCFG_DDC_SDA;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
	data |= CS5530_DCFG_DDC_SCL;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
	data &= ~CS5530_DCFG_DDC_SDA;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
}

/*---------------------------------------------------------------------------
 * SetSCL_5530.
 *
 * Description	  : This routine sets the serial clock line to high.
 *
 *
 * Parameters     : None
 * Returns		  : None
 *
 * Comments       : none
*----------------------------------------------------------------------------
*/
void SetSCL_5530() 
{
	unsigned long data = READ_VID32(CS5530_DISPLAY_CONFIG);
	RETAILMSG( 0,(TEXT("SetSCL\n")));
	data |= CS5530_DCFG_DDC_SCL;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
}

/*---------------------------------------------------------------------------
 * ClearSCL_5530.
 *
 * Description	  : This routine sets the serial clock line to low.
 *
 *
 * Parameters     : None
 * Returns		  : None
 *
 * Comments       : none
*----------------------------------------------------------------------------
*/
void ClearSCL_5530() 
{
	unsigned long data = READ_VID32(CS5530_DISPLAY_CONFIG);
	data &= ~CS5530_DCFG_DDC_SCL;
	RETAILMSG( 0,(TEXT("DDCclockLo %X\n"),data));
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
}

/*---------------------------------------------------------------------------
 * GetSCL_5530.
 *
 * Description	  : This routine gets the of SCL data .
 *
 *
 * Parameters     : None
 * Returns		  : returns SCL data.
 *
 * Comments       : none
*----------------------------------------------------------------------------
*/
int GetSCL_5530()
{
	unsigned long data = READ_VID32(CS5530_DISPLAY_CONFIG);
	RETAILMSG( 0,(TEXT("GetSCL %X\n"), data));
	return ((data & CS5530_DCFG_DDC_SCL) >> 22);
}

/*---------------------------------------------------------------------------
 * SetSDA_5530.
 *
 * Description	  : This routine sets the serial data line to high.
 *
 *
 * Parameters     : None
 * Returns		  : None
 *
 * Comments       : none
*----------------------------------------------------------------------------
*/
void SetSDA_5530() 
{
	unsigned long data = READ_VID32(CS5530_DISPLAY_CONFIG);
	data |= CS5530_DCFG_DDC_SDA | CS5530_DCFG_DDC_OE;
	RETAILMSG( 0,(TEXT("SetSDA %X\n"), data));
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
}

/*---------------------------------------------------------------------------
 * ClearSDA_5530.
 *
 * Description	  : This routine sets the serial data line to low.
 *
 *
 * Parameters     : None
 * Returns		  : None
 *
 * Comments       : none
*----------------------------------------------------------------------------
*/
void ClearSDA_5530()
{
	unsigned long data = READ_VID32(CS5530_DISPLAY_CONFIG);
	data &= ~CS5530_DCFG_DDC_SDA; 
	data |= CS5530_DCFG_DDC_OE;
	RETAILMSG( 0,(TEXT("ClearSDA %X\n"), data));
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
}

/*---------------------------------------------------------------------------
 * GetSDA_5530.
 *
 * Description	  : This routine gets the of SDL data .
 *
 *
 * Parameters     : None
 * Returns		  : returns SCL data.
 *
 * Comments       : none
*----------------------------------------------------------------------------
*/

int GetSDA_5530() 
{
	unsigned long data = READ_VID32(CS5530_DISPLAY_CONFIG);
	data &= ~CS5530_DCFG_DDC_OE;
	WRITE_VID32(CS5530_DISPLAY_CONFIG, data);
	wait(2); /* 10 */
	data = READ_VID32(CS5530_DISPLAY_CONFIG);
	RETAILMSG( 0,(TEXT("GetSDA 0x%8X\n"), data));
	return (data >> 31);
}
