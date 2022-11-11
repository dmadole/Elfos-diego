
;  Copyright 2021, David S. Madole <david@madole.net>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program.  If not, see <https://www.gnu.org/licenses/>.


          ; Include kernal API entry points

#include include/bios.inc
#include include/kernel.inc


          ; Define non-published API elements

d_ideread:  equ   0447h
d_idewrite: equ   044ah


          ; Executable program header

            org   2000h - 6
            dw    start
            dw    end-start
            dw    start

start:      br    entry


          ; Build information

            db    8+80h                 ; month
            db    12                    ; day
            dw    2021                  ; year
            dw    8                     ; build

            db    'See github.com/dmadole/Elfos-diego for more info',0


          ; Check if hook points have already been patched and do not
          ; install if so, since we don't know what it is or what the
          ; impact might be of disconnecting it.

entry:      ldi   patchtbl.1            ; Get point to table of patch points
            phi   rd
            ldi   patchtbl.0
            plo   rd

chekloop:   lda   rd                    ; a zero marks end of the table
            lbz   chekvers

            phi   rf                    ; get pointer to patch point
            lda   rd
            plo   rf

            inc   rf                    ; skip the lbr opcode

            ldn   rd                    ; if points into bios then ok
            smi   0f8h
            lbnf  cheknext

            sep   scall                 ; quit with error message
            dw    o_inmsg
            db    'ERROR: Read or write hooks already installed',13,10,0
            sep   sret

cheknext:   inc   rd                    ; skip target address in table
            inc   rd

            lbr   chekloop              ; repeat for all


          ; Check minimum needed kernel version 0.4.0 in order to have
          ; heap manager available.

chekvers:   ldi   k_ver.1               ; pointer to installed kernel version
            phi   rd
            ldi   k_ver.0
            plo   rd

            lda   rd                    ; if major is non-zero then good
            lbnz  allocmem

            lda   rd                    ; if minor is 4 or more then good
            smi   4
            lbdf  allocmem

            sep   scall                 ; quit with error message
            dw    o_inmsg
            db    'ERROR: Needs kernel version 0.4.0 or higher',13,10,0
            sep   sret


          ; Allocate a page-aligned block from the heap for storage of
          ; the persistent code module. Make it permanent so it will
          ; not get cleaned up at program exit.

allocmem:   ldi   (modend-module).1     ; length of persistent module
            phi   rc
            ldi   (modend-module).0
            plo   rc

            ldi   255                   ; page-aligned
            phi   r7
            ldi   4+64                  ; permanent and named
            plo   r7

            sep   scall                 ; request memory block
            dw    o_alloc
            lbnf  gotalloc

            sep   scall                 ; return with error
            dw    o_inmsg
            db    'ERROR: Could not allocate memeory from heap',13,10,0
            sep   sret

gotalloc:   ghi   rf                    ; Offset to adjust addresses with
            smi   module.1
            stxd

          ; Copy module code into the permanent heap block

            ldi   (modend-module).1     ; length of code to copy
            phi   rb
            ldi   (modend-module).0
            plo   rb

            ldi   module.1              ; get source address
            phi   rd
            ldi   module.0
            plo   rd

copycode:   lda   rd                    ; copy code to destination address
            str   rf
            inc   rf
            dec   rc
            dec   rb
            glo   rb
            lbnz  copycode
            ghi   rb
            lbnz  copycode

            lbr   padname

padloop:    ldi   0                     ; pad name with zeros to end of block
            str   rf
            inc   rf
            dec   rc
padname:    glo   rc
            lbnz  padloop
            ghi   rc
            lbnz  padloop


          ; Output banner message

            sep   scall
            dw    o_inmsg
            db    'Diego SD Card Driver Build 1 for Elf/OS',13,10,0


          ; Patch BIOS to point to normal DMA disk routines

intisdma:   ldi   patchtbl.1            ; Get point to table of patch points
            phi   rd
            ldi   patchtbl.0
            plo   rd


          ; Update kernel hooks to point to the copied module code

setpatch:   inc   r2                    ; point to page offset on stack

hookloop:   lda   rd                    ; a zero marks end of the table
            lbz   finished

            phi   rf                    ; get pointer to vector to hook
            lda   rd
            plo   rf

            inc   rf                    ; skip the lbr opcode

            lda   rd                    ; add offset to get copy address
            add                         ;  and update into vector
            str   rf
            inc   rf
            lda   rd
            str   rf

            lbr   hookloop              ; repeat for all


          ; All done, exit to operating system

finished:   seq                         ; idle state for q is high
            sep   sret


          ; Table giving addresses of jump vectors we need to update, along
          ; with offset from the start of the module to repoint those to.

patchtbl:   dw    d_ideread, sdread
            dw    d_idewrite, sdwrite
            db    0


            org   ($ + 255) & 0ff00h

module:   ; Start the actual module code on a new page so that it forms
          ; a block of page-relocatable code that will be copied to himem.






initreg:    plo   re

            glo   rc
            stxd
            ghi   rc
            stxd

            glo   ra
            stxd
            ghi   ra
            stxd

            glo   r9
            stxd
            ghi   r9
            stxd

            ghi   r3                    ; initialize r9 for subroutine call
            adi   1
            phi   r9
            phi   ra

            ldi   subjump
            plo   r9

            ldi   0
            phi   rc
            plo   rc

            glo   re                    ; return to caller
            adi   2
            plo   r3








sdread:     glo   r3                    ; save and initialize registers
            br    initreg

            sep   r9                    ; send init clocks then command
            db    sendblk.0             ;  init plus block command
            db    cmd17.0               ;  read block command packet
            db    1                     ;  response length
            db    readini.0

rdblock:    sep   r9                    ; receive data response token
            db    recvspi.0             ;  receive after idle
            db    1                     ;  reponse length
            db    timeout.0

            ghi   rf                    ; get pointer to data buffer
            phi   ra
            glo   rf
            plo   ra

            ldi   512.1                 ; set length to full block
            phi   rc

            sep   r9                    ; read data into bufffer
            db    recvraw.0

            sep   r9                    ; read crc and disregard
            db    recvbuf.0             ;  receive raw bytes
            db    2                     ;  reponse length

            adi   0                     ; return with df clear
            br    return



readini:    glo   r3                    ; initialize sd card
            br    initspi 

            sep   r9                    ; resend block read command
            db    sendcmd.0             ;  send command packet
            db    cmd17.0               ;  read block command packet
            db    1                     ;  response length
            db    timeout.0

            br    rdblock               ; receive data block




sdwrite:    glo   r3                    ; save and initialize registers
            br    initreg

            sep   r9                    ; send init clocks then command
            db    sendblk.0             ;  init plus block command
            db    cmd24.0               ;  write block command packet
            db    1                     ;  response length
            db    writini.0

wrblock:    sep   r9                    ; send data start token
            db    sendbuf.0             ;  send from static memory
            db    stblock.0             ;  start data block token
            db    1                     ;  token length
            db    0                     ;  no response expected

            ghi   rf                    ; get pointer to data buffer
            phi   ra
            glo   rf
            plo   ra

            ldi   512.1                 ; set length to full block
            phi   rc

            sep   r9                    ; read data into bufffer
            db    sendspi.0             ;  send raw bytes from ra
            db    0                     ;  no response expected

            ghi   ra                    ; update buffer pointer past end
            phi   rf

            sep   r9                    ; send dummy crc bytes
            db    sendbuf.0             ;  send from static memory
            db    zeroes.0              ;  dummy zero crc bytes
            db    2                     ;  crc bytes length
            db    1                     ;  reponse expected
            db    timeout.0

busy:       sep   r9                    ; receive busy flag
            db    recvbuf.0             ;  receive raw byte to memory
            db    1                     ;  length to receive

            dec   ra                    ; wait if response not zero
            ldn   ra
            bz    busy

            adi   0                     ; return with df clear
            br    return



writini:    glo   r3                    ; initialize sd card
            br    initspi 

            sep   r9                    ; resend block write command
            db    sendcmd.0             ;  send command packet
            db    cmd24.0               ;  write block command packet
            db    1                     ;  response length
            db    timeout.0

            br    wrblock               ; send data block




initspi:    str   r2                    ; save return address

            sep   r9                    ; send reset command
            db    sendcmd.0             ;  send command packet
            db    cmd0.0                ;  reset command packet
            db    1                     ;  response length
            db    timeout.0

            sep   r9                    ; send host voltage support
            db    sendcmd.0             ;  send command packet
            db    cmd8.0                ;  voltage support command packet
            db    5                     ;  response length
            db    timeout.0

iniloop:    sep   r9                    ; send application command escape
            db    sendcmd.0             ;  send command packet
            db    cmd55.0               ;  application comman packet
            db    1                     ;  response length
            db    timeout.0

            sep   r9                    ; send host capacity support
            db    sendcmd.0             ;  send command packet
            db    acmd41.0              ;  host capacity command packet
            db    1                     ;  response length
            db    timeout.0

            dec   ra                    ; wait until reponse is zero
            ldn   ra
            bnz   iniloop

            ldn   r2                    ; return to instruction after br
            adi   2
            plo   r3





            org   ($ & 0ff00h) + 253

timeout:    smi   0

return:     inc   r2                    ; this falls through into next page

            lda   r2
            phi   r9
            lda   r2
            plo   r9

            lda   r2
            phi   ra
            lda   r2
            plo   ra

            lda   r2
            phi   rc
            ldn   r2
            plo   rc

            sep   sret



sendblk:    ldi   80/2                  ; 80 pulses unrolled by factor of 2

sendinit:   req                         ; send pulse, decrement pulse count
            seq
            smi   1

            req                         ; send pulse, loop if count not zero
            seq
            bnz   sendinit

            lda   r3                    ; get address of the command packet,
            adi   4                     ;  add offset to lsb of the address
            plo   ra

            sex   ra                    ; use stxd to save a couple of instr


          ; Different types of SD cards address content differently, so we
          ; need to handle two cases here depending on what kind of card we
          ; detected during the initialization process.

            br    sdhc                  ; right now the type is static


          ; SDSC cards address content by byte and so the Elf/OS block address
          ; needs to be multiplied by 512, which is nine left bit shifts, or
          ; one byte shift plus one extra bit.

sdsc:       ldi   0                     ; lowest byte is always zero, store
            stxd                        ;  shifted left address in next three
            glo   r7
            shl
            stxd
            ghi   r7
            shlc
            stxd
            glo   r8
            shlc
            stxd

            sex   r2
            br    execcmd


          ; SDHC and SDXC cards address content in 512-byte blocks, which is
          ; the same as Elf/OS so we just store the block address into the
          ; low three bytes of the address in the command packet.

sdhc:       glo   r7                    ; we only use the low three bytes,
            stxd                        ;  the last one is always zero
            ghi   r7
            stxd
            glo   r8
            stxd
            ldi   0
            stxd

            sex   r2
            br    execcmd


sendcmd:    lda   r3
            plo   ra

execcmd:    ldi   6
            plo   rc

            ldi   255
            br    sendhigh


sendbuf:    ghi   r9
            phi   ra
            lda   r3
            plo   ra

            lda   r3
            plo   rc


          ; Send data through SPI from memory starting at RF for RC bytes.
          ; Returns RF just past sent data and RC set to zero. An input count
          ; of 0 means 65536 bytes, which is probably not useful.

sendspi:    smi   0

sendbyte:   lda   ra                    ; get byte to send, adjust count
            dec   rc

            shlc                        ;  shift in a one bit to mark end
            bnf   sendlow

sendhigh:   req                         ; send a one bit, shift data and
            seq                         ;  jump if end of byte
            shl
            bz    sendmore

sendnext:   bdf   sendhigh              ; is data bit a one or zero?

sendlow:    req                         ; send a zero bit, shift data and
            shl                         ;  jump if not end of byte
            seq
            bnz   sendnext

sendmore:   glo   rc                    ; loop if all data has not been sent
            bnz   sendbyte
            ghi   rc
            bnz   sendbyte



          ; Receive bytes through SPI into memory at RF for RC bytes. This
          ; first skips any $FF bytes which are idle time on the line before
          ; counting and receiving data. A count of 0 means 65536 bytes.

recvspi:    lda   r3
            bz    sendret

            plo   rc

            inc   r3

            ldi   buffer.0
            plo   ra

            ldi   64                    ; idle bytes to wait for data start
            plo   re

recvwait:   ldi   255                   ; start shift register with ones

recvlow:    shl                         ; shift in zero, exit loop if byte
            bnf   recvlast

recvmore:   req                         ; clock next bit, branch if zero
            seq
            bn2   recvlow

            shlc                        ; shift in one, loop if not a byte
            bdf   recvmore

recvlast:   str   ra                    ; save byte, branch if not idle
            smi   255
            bnz   recvdata

            dec   re                    ; decrement count, loop if more
            glo   re
            bnz   recvwait

            dec   r3
            lda   r3                    ; return to timeout vector
            plo   r3
            sep   r3

            lda   r3
            plo   r9                    ; duplicate subroutine entry



recvbuf:    lda   r3
            plo   rc

            ghi   r9
            phi   ra
            ldi   buffer.0
            plo   ra


          ; Receive bytes through SPI into memory at RF for RC bytes. This
          ; immediately puts bytes into memory without skipping any leading
          ; $FF bytes like recvspi does. A count of 0 means 65536 bytes.

recvraw:    ldi   255                   ; start shift register with ones

recvzero:   shl                         ; shift in zero, exit loop if a byte
            bnf   recvdone

recvnext:   req                         ; clock next bit, branch if zero
            seq
            bn2   recvzero

            shlc                        ; shift in one, loop if not a byte
            bdf   recvnext

recvdone:   str   ra                    ; save byte to buffer

recvdata:   inc   ra                    ; move past received byte

            dec   rc                    ; loop back if more to receive
            glo   rc
            bnz   recvraw
            ghi   rc
            bnz   recvraw

sendret:    sep   r3                    ; return


          ; Entry point for all subroutines. This is called via sep r9 with
          ; the address within the page passed in D, which it simply jumps
          ; to by putting the address into the lsb of the program counter.

subjump:    lda   r3
            plo   r9




cmd0:       db    40h+0,0,0,0,0,1+94h
cmd8:       db    40h+8,0,0,1,0aah,1+86h
cmd17:      db    40h+17,0,0,0,0,1
cmd24:      db    40h+24,0,0,0,0,1
cmd55:      db    40h+55,0,0,0,0,1
acmd41:     db    40h+41,40h,0,0,0,1

stblock:    db    0feh
zeroes:     db    0,0

buffer:     ds    5


            ; Module name for minfo to display

            db    0,'Diego',0

modend:   ; This is the last of what's copied to the heap.

end:      ; That's all folks!

