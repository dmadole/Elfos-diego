
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

            ghi   re                    ; save to use as transfer counter
            stxd

            glo   ra                    ; save to use as transfer pointer
            stxd
            ghi   ra
            stxd

            glo   r9                    ; save to use for subroutine call
            stxd
            ghi   r9
            stxd

            ldi   subjump               ; initialize r9 for subroutine call
            plo   r9
            ghi   r3
            adi   1
            phi   r9

            phi   ra                    ; preset page of data pointer

            glo   re                    ; return to caller
            adi   2
            plo   r3


          ; Read a block from the SD Card; the block address is passed in
          ; R8:R7 and the pointer to the data buffer is in RF. If the read
          ; is successful, DF is cleared, otherwise it is set.

sdread:     glo   r3                    ; save and initialize registers
            br    initreg

            sep   r9                    ; read the block from disk
            db    sendini,51h

            sep   r9                    ; read response token
            db    recvspi

            bz    rdblock               ; read block if response is zero,
            bnf   error                 ;  else other than timeout is error

            glo   r3                    ; if timeout, initialize sd card
            br    initspi 

            sep   r9                    ; resend block read command
            db    sendblk,51h

            sep   r9                    ; read response token
            db    recvspi

            bnz   error                 ; timeout or non-zero is error

rdblock:    sep   r9                    ; receive data response token
            db    recvspi

            xri   0feh                  ; other than 11111110 is an error,
            bnz   error                 ;  including a timeout

            ghi   rf                    ; get pointer to data buffer
            phi   ra
            glo   rf
            plo   ra

            sep   r9                    ; get first 256 data bytes to buffer
            db    recvraw,100h

            sep   r9                    ; get last 256 data bytes to buffer
            db    recvraw,100h

            ghi   ra                    ; update returned buffer pointer
            phi   rf

            ghi   r9                    ; reset page of data pointer
            phi   ra

            sep   r9                    ; read crc and disregard
            db    recvstk,2

            inc   r2
            inc   r2

            br    return                ; return with df clear


          ; Write a block to the SD Card; the block address is passed in
          ; R8:R7 and the pointer to the data buffer is in RF. If the write
          ; is successful, DF is cleared, otherwise it is set.

sdwrite:    glo   r3                    ; save and initialize registers
            br    initreg

            sep   r9                    ; send init clocks then command
            db    sendini,58h

            sep   r9
            db    recvspi               ;  response length

            bz    wrblock               ; read block if response is zero,
            bnf   error                 ;  else other than timeout is error

            glo   r3                    ; initialize sd card
            br    initspi 

            sep   r9                    ; resend block write command
            db    sendblk,58h           ;  send command packet

            sep   r9
            db    recvspi               ;  response length

            bnz   error                 ; timeout or non-zero is error

wrblock:    sep   r9                    ; send data start token
            db    sendlit,1             ;  send from static memory
            db    0feh

            sex   rf

            sep   r9                    ; read data into bufffer
            db    sendmrx,100h          ;  send raw bytes from ra

            sep   r9                    ; read data into bufffer
            db    sendmrx,100h          ;  send raw bytes from ra

            sep   r9                    ; send dummy crc bytes
            db    sendlit,2             ;  send from static memory
            db    0,0

            sep   r9
            db    recvspi               ; response length

            ani   1fh                   ; response other than xxx00101 is
            xri   05h                   ;  error, including timeout
            bnz   error

isbusy:     sep   r9                    ; receive busy flag
            db    recvstk,1             ;  receive raw byte to memory

            lda   r2
            bz    isbusy

            sep   r9                    ; send get status command
            db    sendcmd,4dh           ;  send command packet

            sep   r9                    ; receive response
            db    recvspi

            bnz   error                 ; not 0 or timeout is error

            sep   r9                    ; get second byte of r2 response
            db    recvstk,1

            lda   r2
            bnz   error

            br    return                ; return with df clear



initspi:    str   r2                    ; save return address

            sep   r9                    ; send reset command
            db    sendlit,6             ;  send command packet
            db    40h,0,0,0,0,95h       ; reset device

            sep   r9
            db    recvspi               ; response length

            xri   1                     ; response other than 1 is error
            bnz   error                 ;  including a timeout

            sep   r9                    ; send host voltage support
            db    sendlit,6             ;  send command packet
            db    48h,0,0,1,5,8fh       ; host capacity support

            sep   r9
            db    recvspi               ; response length

            xri   1                     ; response other than 1 is error
            bnz   r4error               ;  including a timeout

            sep   r9                    ; receive 4 more bytes of response
            db    recvstk,4

            inc   r2
            inc   r2
            inc   r2
            inc   r2

waitini:    sep   r9                    ; send application command escape
            db    sendcmd,77h           ;  send command packet

            sep   r9
            db    recvspi               ;  response length

            xri   1                     ; response other than 1 is error
            bnz   error                 ;  including a timeout

            sep   r9                    ; send host capacity support
            db    sendlit,6             ;  send command packet
            db    69h,40h,0,0,0,1       ; initialize device

            sep   r9
            db    recvspi               ;  response length

            shr                         ; if not 0 or 1, then error, including
            bnz   error                 ;  if timeout

            bdf   waitini               ; if not 0, then repeat until it is

            sep   r9                    ; get ocr register
            db    sendcmd,7ah

            sep   r9                    ; first byte of response
            db    recvspi

            bnz   r4error               ; fail on error or timeout

            sep   r9                    ; 4 more bytes of response
            db    recvstk,4

            inc   r2
            inc   r2
            inc   r2

            lda   r2
            ani   40h                   ; ccs bit = device capacity support

            ldn   r2                    ; return to instruction after br
            adi   2
            plo   r3




r4error:    sep   r9                    ; 4 more bytes of response
            db    recvstk,4

            inc   r2
            inc   r2
            inc   r2
            inc   r2

error:      smi   0

return:     inc   r2                    ; this falls through into next page

            lda   r2
            phi   r9
            lda   r2
            plo   r9

            lda   r2
            phi   ra
            lda   r2
            plo   ra

            ldn   r2
            phi   re

            sep   sret


            org   ($ + 255) & 0ff00h

sendini:    ldi   80/2                  ; 80 pulses unrolled by factor of 2

loopini:    req                         ; send pulse, decrement pulse count
            seq
            smi   1

            req                         ; send pulse, loop if count not zero
            seq
            bnz   loopini

sendblk:    sex   r2
            dec   r2

            ldi   1
            stxd


          ; Different types of SD cards address content differently, so we
          ; need to handle two cases here depending on what kind of card we
          ; detected during the initialization process.

            br    sdhcblk               ; right now the type is static


          ; SDSC cards address content by byte and so the Elf/OS block address
          ; needs to be multiplied by 512, which is nine left bit shifts, or
          ; one byte shift plus one extra bit.

sdscblk:    ldi   0                     ; lowest byte is always zero, store
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

            br    execcmd


          ; SDHC and SDXC cards address content in 512-byte blocks, which is
          ; the same as Elf/OS so we just store the block address into the
          ; low three bytes of the address in the command packet.

sdhcblk:    glo   r7                    ; we only use the low three bytes,
            stxd                        ;  the last one is always zero
            ghi   r7
            stxd
            glo   r8
            stxd
            ldi   0
            stxd

            br    execcmd


sendcmd:    sex   r2
            dec   r2

            ldi   1
            stxd
            ldi   0
            stxd
            stxd
            stxd
            stxd

execcmd:    lda   r3
            str   r2

            ldi   6
            plo   re

            br    sendspi

sendlit:    lda   r3
            plo   re

            sex   r3

            br    sendspi


          ; Send data through SPI from memory starting at RF for RC bytes.
          ; Returns RF just past sent data and RC set to zero. An input count
          ; of 0 means 65536 bytes, which is probably not useful.

sendmrx:    lda   r3                    ; get length to send
            plo   re

sendspi:    smi   0                     ; set df to mark end of bits

sendbyte:   dec   re                    ; decrement byte count

            ldxa                        ; get next byte to send and shift
            shlc                        ;  in a one bit to mark end
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

sendmore:   glo   re                    ; loop if all data has not been sent
            bnz   sendbyte

            sep   r3

            lda   r3
            plo   r9



          ; Receive a single byte through SPI, first skiping any $FF bytes
          ; which are idle time on the line. Returns the byte in D. If no
          ; non-$FF value is seen within 64 bytes, then a timeout has occured,
          ; and $FF is returned in D with DF set. Otherwise, DF is cleared.
          ;
          ; Note: As this received, it records bits inverted as it's easier
          ; to detect and deal with $FF bytes that way since they are zero.
          ; The true value is returned at the end though.

recvspi:    ldi   64                    ; idle bytes to wait for data start
            plo   re

recvwait:   ldi   255                   ; start shift register with ones

recvhigh:   shl                        ; shift in zero, exit loop if byte
            bnf   recvlast

recvmore:   req                         ; clock next bit, branch if zero
            seq
            b2    recvhigh

            shlc                        ; shift in one, loop if not a byte
            bdf   recvmore

recvlast:   bnz   recvbyte

recvskip:   dec   re                    ; decrement count, loop if more
            glo   re
            bnz   recvwait

            smi   0                     ; set df to signal timeout

recvbyte:   xri   255                   ; complement and return
            sep   r3

            lda   r3                    ; subroutine entry jump vector
            plo   r9






recvstk:    ldn   r3

pushstk:    dec   r2
            smi   1
            bnz   pushstk

            glo   r2
            plo   ra
            ghi   r2
            phi   ra

            br    recvraw


          ; Receive up to 255 bytes into the scratchpad buffer by presetting
          ; RC based on an inline value and RA to the buffer address.

recvbuf:    ldi   buffer.0
            plo   ra


          ; Receive bytes through SPI into memory at RF for RC bytes. This
          ; immediately puts bytes into memory without skipping any leading
          ; $FF bytes like recvspi does. A count of 0 means 65536 bytes.

recvraw:    lda   r3                    ; get inline length to receive
            plo   re

recvloop:   dec   re                    ; decrement received bytes count,
            ldi   255                   ;  set shift register to count bits

recvzero:   shl                         ; shift in zero, exit loop if a byte
            bnf   recvdone

recvnext:   req                         ; clock next bit, branch if zero
            seq
            bn2   recvzero

            shlc                        ; shift in one, loop if not a byte
            bdf   recvnext

recvdone:   str   ra                    ; save byte to buffer

            inc   ra                    ; move past received byte

            glo   re
            bnz   recvloop

            sep   r3                    ; return with df clear


          ; Entry point for all subroutines. This is called via sep r9 with
          ; the address within the page passed in D, which it simply jumps
          ; to by putting the address into the lsb of the program counter.

subjump:    lda   r3
            plo   r9


          ; Start data token and a couple of zeroes that are send for a dummy
          ; CRC, even though the CRC could be anything at all.



          ; Buffer for receiving command responses. This will need to be
          ; reworked before this can be put into ROM.

buffer:     ds    5


          ; Module name for minfo to display

            db    0,'Diego',0


modend:   ; This is the last of what's copied to the heap.

end:      ; That's all folks!

