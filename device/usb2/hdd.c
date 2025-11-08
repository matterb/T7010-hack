/*
*  license and disclaimer for the use of this source code as per statement below
*  Lizenz und Haftungsausschluss für die Verwendung dieses Sourcecodes siehe unten
*/

// WARNING: WIP! Do not use any of these functions on a real PC

#include "hdd.h"
#include "ata.h"
#include "os.h"
#include "devicemanager.h"
#include "kheap.h"
#include "serial.h"

void hdd_install(void)
{
#ifdef _ENABLE_HDD_
    uint16_t buf[256]; // TODO: Proper struct

    mutex_t* ataPrimaryChannelLock = 0;
    mutex_t* ataSecondaryChannelLock = 0;

    for (int i = ATA_PRIMARY_MASTER; i <= ATA_SECONDARY_SLAVE; ++i)
    {
        if (ata_identify((ATA_CHANNEL)i, buf) == ATA_HARDDISK)
        {
#ifdef _HDD_DIAGNOSTICS_
            serial_log(SER_LOG_HRDDSK, "[hdd_install] Disk connected in ATA-Channel %d\r\n", i);
#endif
            ata_portInfo_t* portInfo = malloc(sizeof(ata_portInfo_t), 0, "ata_portInfo_t");
            portInfo->channel = (ATA_CHANNEL)i;

            if (!(ata_getRegPorts(portInfo->channel, &portInfo->regs) &&
                    ata_getIRQ(portInfo->channel, &portInfo->irq) &&
                    ata_isSlave(portInfo->channel, &portInfo->slave)))
            {
                // This is a fatal error, so report it even when _HDD_DIAGNOSTICS_ is not set
                serial_log(SER_LOG_HRDDSK,
                           "[hdd_install] FATAL ERROR: Setup of hdd [%d] failed (this should never happen)\r\n",
                           i);
                free(portInfo);
                continue;
            }

            portInfo->supportsDma = false; // TODO
            portInfo->supportsLba48 = false; // TODO

            if (i == ATA_PRIMARY_MASTER || i == ATA_PRIMARY_SLAVE)
            {
                if (!ataPrimaryChannelLock)
                    ataPrimaryChannelLock = mutex_create();
                portInfo->rwLock = ataPrimaryChannelLock;

                outportb(ATA_REG_PRIMARY_DEVCONTROL, 0x00);
            }
            else if (i == ATA_SECONDARY_MASTER || i == ATA_SECONDARY_SLAVE)
            {
                if (!ataSecondaryChannelLock)
                    ataSecondaryChannelLock = mutex_create();
                portInfo->rwLock = ataSecondaryChannelLock;

                outportb(ATA_REG_SECONDARY_DEVCONTROL, 0x00);
            }

            portInfo->drive = malloc(sizeof(port_t), 0, "hdd-Port");

            portInfo->drive->type = &HDD;
            portInfo->drive->data = portInfo;
            portInfo->drive->insertedDisk = malloc(sizeof(disk_t), 0, "hdd-Disk");

            portInfo->drive->insertedDisk->type = &HDDDISK;
            portInfo->drive->insertedDisk->data = portInfo;
            portInfo->drive->insertedDisk->port = portInfo->drive;
            // buf[60] and buf[61] give the total size of the lba28 sectors
            portInfo->drive->insertedDisk->size = ((uint64_t)(*(uint32_t*)&buf[60])) * 512;
            portInfo->drive->insertedDisk->headCount = 0;
            portInfo->drive->insertedDisk->secPerTrack = 0;
            portInfo->drive->insertedDisk->sectorSize = 512;
            portInfo-> drive->insertedDisk->accessRemaining = 0;

            for (int j = 0; j < PARTITIONARRAYSIZE; ++j)
                portInfo->drive->insertedDisk->partition[i] = 0;

#ifdef _HDD_DIAGNOSTICS_
            serial_log(SER_LOG_HRDDSK, "[hdd_install] Size of disk at channel %d is %d\r\n",
                       i, portInfo->drive->insertedDisk->size);
#endif

            attachDisk(portInfo->drive->insertedDisk); // disk == hard disk
            attachPort(portInfo->drive);
            analyzeDisk(portInfo->drive->insertedDisk);

            ata_softReset(portInfo);
        }
    }
#endif
}

FS_ERROR hdd_writeSector(uint32_t sector, void* buf, disk_t* device)
{
    if (sector < device->size / 512)
    {
        return ata_accessSectors(sector, 1, buf, device->data, ATA_WRITE);
    }
    else return CE_INVALID_ARGUMENT;
}

FS_ERROR hdd_readSector(uint32_t sector, void* buf, disk_t* device)
{
    if (sector < device->size / 512)
    {
        return ata_accessSectors(sector, 1, buf, device->data, ATA_READ);
    }
    else return CE_INVALID_ARGUMENT;
}

/*
* Copyright (c) 2012-2013 The PrettyOS Project. All rights reserved.
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