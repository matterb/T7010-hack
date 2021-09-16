//  ===============================================
//  CamelForth for the Intel 80x86
//  Copyright (c) 1996 Bradford J. Rodriguez
// 
//  This program is free software//  you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation//  either version 3 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY//  without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//  Commercial inquiries should be directed to the author at 
//  115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
//  or via email to bj@camelforth.com
// 
//  ===============================================
//    Source code is for GNU Assembler.
//    Forth words are documented as follows:
//  x[-yy]   NAME     stack -- stack    description
//      where x=C for ANS Forth Core words,
//             =X for ANS Extensions, 
//             =Z for internal or private words.
//      where yy=BL for BLock word set
//              =DO for DOble word set
//              =EX for EXception word set
//              =FA for FAcility word set
//              =FI for FIle word set
//              =FL for FLoating word set
//              =LO for LOcal word set
//              =ME for MEmory word set
//              =TO for TOols word set
//              =SE for SEarch word set
//              =ST for STring word set
//
//  Guide to stack diagrams:  
//      R: = return stack,
//       c = 8-bit character, 
//    flag = boolean (0 or -1), 
//       n = signed 32-bit, 
//       u = unsigned 32-bit,
//       d = signed 64-bit, 
//      ud = unsigned 64-bit,
//      +n = unsigned 32-bit, 
//       x = any cell value, 
//  *x j*x = any number of cell values,
//  a-addr = aligned adrs, 
//  c-addr = character adrs
//  p-addr = I/O port adrs, 
//     sys = system-specific.
//
// Direct- Threaded Forth model for Intel 80x86
// 32 bit cell, 8 bit char, 8 bit (byte) addrs unit, align 32 bit
// Dictionary layout:
// LINK(4bytes):FLAG(1byte):NAME_FIELD(1byte len,name):Code_Field(4byte):PFA
// Register Usage:
//  %esi = IP   Forth Interpreter pointer
//  %edi = UP   User pointer
//  %esp = PSP  Parameter Stack Pointer
//  %ebp = RSP  Return Stack Pointer
//  %eax = W    work register, scratch
//  %ebx = TOS  Top of Parameter Stack
//  %ecx = loop index
//  %edx = scratch
//
//  ===============================================
//  REVISION HISTORY
//  v2011.001
//  ===============================================


