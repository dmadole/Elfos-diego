
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

d_idereset: equ   0444h
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

finished:   sep   scall
            dw    d_idereset

            sep   sret


          ; Table giving addresses of jump vectors we need to update, along
          ; with offset from the start of the module to repoint those to.

patchtbl:   dw    d_idereset, sdreset
            dw    d_ideread, sdread
            dw    d_idewrite, sdwrite
            db    0


            org   ($ + 255) & 0ff00h

module:   ; Start the actual module code on a new page so that it forms
          ; a block of page-relocatable code that will be copied to himem.

          ; There are three kinds of subroutine calls in use here. The main
          ; entry points of SDRESET, SDREAD, and SDWRITE are all called with
          ; standard SCRT conventions like all of Elf/OS uses. Because the
          ; driver code needs to be relocatable, we use two other methods,
          ; and also because they are much less overhead than SCRT.

          ; For calling subroutines in the same page, we use a calling 
          ; convention of GLO R3,BR SUBR; the GLO R3 is to pass the return
          ; address to the subroutine, which it increments by 2 to get the
          ; return address past the BR instruction, and saves it somewhere.
          ; To return, the address is simply stuffed into the PC with PLO R3.

          ; For calling subroutines in the other page, we use SEP R9 to 
          ; switch PC to one in the other page. In this case, we follow the
          ; SEP R9 with the address of the subroutine within the other page.
          ; R9 is initialized to point to a LDA R3,PLO R9 sequence which
          ; gets the inline address argument and jumps to it. To save having
          ; to reset R9, each subroutine return (consisting of SEP R3) is
          ; followed by this same sequence to R9 is always pointing to one
          ; or another instance of it.


          ; INITREG saves the registers we use and presets R9 for subroutine
          ; calls in the other page. RE is used for temporary storage of the
          ; return address since we are pushing to the stack here.

initreg:    adi   2                     ; save return address following br
            plo   re

            ghi   re                    ; save to use as transfer counter
            stxd

            glo   r9                    ; save to use for subroutine call
            stxd
            ghi   r9
            stxd

            ghi   r3                    ; initialize r9 for subroutine call
            adi   1
            phi   r9
            ldi   subjump
            plo   r9

            glo   re                    ; return to caller
            plo   r3


          ; SDRESET first sets Q high so it's in the correct idle state, then
          ; it initializes the SD Card, if one is present. This should be 
          ; called before the first read or write operations as the
          ; initialization process is also where the card capacity is read
          ; which is needed to know how to send the block addresses.

sdreset:    seq                         ; set clock line to idle state

            glo   r3                    ; save and intialize registers
            br    initreg

            sep   r9                    ; send initial clock pulses
            db    sendini

            glo   r3                    ; send initialization commands
            br    initspi

            br    return                ; done, return


          ; SDREAD reads a block from the SD Card; the block address is passed
          ; in R8:R7 and the pointer to the data buffer is in RF. If the read
          ; is successful, DF is cleared, otherwise it is set. If the initial
          ; read command times out, it means that the card is not in SPI
          ; mode, and was probably just inserted, so the card is initialized.

sdread:     glo   r3                    ; save and initialize registers
            br    initreg

            sep   r9                    ; send initial clock pulses
            db    sendini

            sep   r9                    ; read the block from disk
            db    sendblk,51h

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

rdblock:    sep   r9                    ; receive data block start token
            db    recvspi

            xri   0feh                  ; other than 11111110 is an error,
            bnz   error                 ;  including a timeout

            sep   r9                    ; get first 256 data bytes to buffer
            db    recvbuf,100h

            sep   r9                    ; get last 256 data bytes to buffer
            db    recvbuf,100h

            sep   r9                    ; read crc of data
            db    recvstk,2

            inc   r2                    ; discard the crc
            inc   r2

            br    return                ; return with df clear


          ; SDWRITE writes a block to the SD Card; the block address is passed 
          ; in R8:R7 and the pointer to the data buffer is in RF. If the write
          ; is successful, DF is cleared, otherwise it is set. Unlike in
          ; SDREAD, we do not intialize the card if the write command times
          ; out. This is because a write as the first operation to a newly
          ; inserted card is almost certainly wrong and will corrput data,
          ; probably it happened as a result of a card that was swapped while
          ; a file was open. So, it seems safer to let these fail.
 
sdwrite:    glo   r3                    ; save and initialize registers
            br    initreg

            sep   r9                    ; send initialization clocks
            db    sendini

            sep   r9                    ; send block write command
            db    sendblk,58h
  
            sep   r9                    ; get response token
            db    recvspi

            bnz   error                 ; not zero or timeout is an error

            sep   r9                    ; send data block start token
            db    sendlit,1             ;  of 11111110
            db    0feh

            sex   rf                    ; rf points to the buffer

            sep   r9                    ; send first 256 bytes from buffer
            db    sendmrx,100h

            sep   r9                    ; send next 256 bytes from buffer
            db    sendmrx,100h

            sep   r9                    ; send dummy crc bytes, in spi
            db    sendlit,2             ;  mode crcs are not used by default
            db    0,0

            sep   r9                    ; get data response token
            db    recvspi

            ani   1fh                   ; response other than xxx00101 is
            xri   05h                   ;  error, including a timeout
            bnz   error

isbusy:     sep   r9                    ; receive busy token
            db    recvstk,1

            lda   r2                    ; wait until token is not zero
            bz    isbusy

            sep   r9                    ; send get device status command
            db    sendcmd,4dh

            sep   r9                    ; receive response token
            db    recvspi

            bnz   r1error               ; not 0 or timeout is error

            sep   r9                    ; get second byte of r2 response
            db    recvstk,1

            lda   r2                    ; not zero is an error
            bnz   error

            br    return                ; return with df clear


          ; INITSPI initializes an SD Card into SPI mode by sending the
          ; prescribed sequence of commands. One important part of this is
          ; discovery of whether the card is high capacity (HC or XC types)
          ; because this determines whether the data on the card is addressed
          ; by byte or by block. So this information is saved during initial-
          ; ization for later reference in sending read and write commands.

initspi:    adi   2                     ; save return address following br
            str   r2

            sep   r9                    ; send reset command
            db    sendlit,6
            db    40h,0,0,0,0,95h

            sep   r9                    ; get response token
            db    recvspi

            xri   1                     ; timeout or other than 1 is error
            bnz   error

            sep   r9                    ; send host voltage support
            db    sendlit,6
            db    48h,0,0,1,5,8fh

            sep   r9                    ; get response token
            db    recvspi

            xri   1                     ; timeout or other than 1 is error
            bnz   r4error

            sep   r9                    ; receive 4 more bytes of response
            db    recvstk,4

            inc   r2                    ; discard response
            inc   r2
            inc   r2
            inc   r2

waitini:    sep   r9                    ; send application command escape
            db    sendcmd,77h

            sep   r9
            db    recvspi               ; get response token

            xri   1                     ; timeout or other than 1 is error
            bnz   error

            sep   r9                    ; send host capacity support
            db    sendlit,6
            db    69h,40h,0,0,0,1

            sep   r9                    ; get reponse token
            db    recvspi

            shr                         ; if not 0 or 1, then error
            bnz   error

            bdf   waitini               ; if not 0, then repeat until so

            sep   r9                    ; get ocr register
            db    sendcmd,7ah

            sep   r9                    ; get first byte of response
            db    recvspi

            bnz   r4error               ; fail on error or timeout

            sep   r9                    ; 4 more bytes of response
            db    recvstk,4

            inc   r2                    ; discard last three bytes
            inc   r2
            inc   r2

            ghi   r9                    ; pointer to byte to store ocr
            phi   re
            ldi   ocrreg
            plo   re

            lda   r2                    ; save first byte of response
            str   re

            ldn   r2                    ; return
            plo   r3


          ; R4ERROR and R1ERROR return from an error condition, first
          ; reading any outstanding bytes that the card needs to send.

r4error:    sep   r9                    ; receive and discard 4 bytes
            db    recvstk,3

            inc   r2
            inc   r2
            inc   r2

r1error:    sep   r9                    ; receive and discard 1 byte
            db    recvstk,1

            inc   r2


          ; Make sure DF is set on return from error condition.

error:      smi   0

return:     inc   r2                    ; this falls through into next page

            lda   r2
            phi   r9
            lda   r2
            plo   r9

            ldn   r2
            phi   re

            sep   sret


            org   ($ + 255) & 0ff00h


          ; After power-on an SD Card may require up to 74 clock pulses
          ; to be able to initialize, so we send extra pulses before each
          ; initial block command in case the card was freshly inserted.
          ; 80 pulses are sent so it does not disrupt byte syncronization.

sendini:    ldi   80/2                  ; 80 pulses unrolled by factor of 2

loopini:    req                         ; send pulse, decrement pulse count
            seq
            smi   1

            req                         ; send pulse, loop if count not zero
            seq
            bnz   loopini

            sep   r3                    ; return

            lda   r3                    ; subroutine call re-entry point
            plo   r9


          ; Send a block read or write command by building the command packet
          ; on the stack including the appropriate address from R8:R7 and
          ; then sending it. The OCR register byte that is retreived during
          ; card initialization is referenced to see if the card is high
          ; capacity or not so we know what address format to use. The first
          ; byte specifying the read or write command is passed inline.

sendblk:    sex   r2  
            dec   r2

            ldi   1
            stxd


          ; Different types of SD cards address content differently, so we
          ; need to handle two cases here depending on what kind of card we
          ; detected during the initialization process.

            ghi   r9                    ; get saved ccs flag from card init
            phi   re
            ldi   ocrreg
            plo   re

            ldn   re                    ; if set, card is high-capacity
            ani   40h
            bnz   sdhcblk


          ; SDSC cards address content by byte and so the Elf/OS block address
          ; needs to be multiplied by 512, which is nine left bit shifts, or
          ; one byte shift plus one extra bit.

            ldi   0                     ; lowest byte is always zero, store
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


          ; Build a command packet with an all-zeroes payload on the stack
          ; and send to the card. The first byte is passed inline and the 
          ; CRC is sent as zero since most commands don't need it in SPI mode.

sendcmd:    sex   r2                    ; build downward on stack
            dec   r2

            ldi   1                     ; store crc and stuff bits
            stxd
            ldi   0
            stxd
            stxd
            stxd
            stxd

execcmd:    lda   r3                    ; store first command byte
            str   r2

            ldi   6                     ; length of the packet to send
            plo   re

            br    sendspi               ; send via spi output routine


          ; Send literal bytes inline with the subroutine call. This is used
          ; to send command packets with payloads that the prior cases cannot
          ; handle, and for commands where the CRC needs to be correct, as
          ; well as for data tokens and CRCs.

sendlit:    lda   r3
            plo   re

            sex   r3

            br    sendspi


          ; Send data through SPI from memory starting at RX for RE.0 bytes.
          ; Leaves RX just past sent data and RE.0 set to zero. An input count
          ; of 0 means 256 bytes; to send more than that, call more than once.

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

            sep   r3                    ; return


          ; Entry point of subroutine calls. This is called via SEP R9 with
          ; individual subroutine address within the page passed in D, which
          ; it simply jumps to by storing to the lsb of the program counter.
          ; This is duplicated after each subroutine return so that the R9
          ; register does not need to be reset for each call.

subjump:    lda   r3
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


          ; Receive bytes through SPI, pushing them onto the stack, for
          ; RE.0 bytes. This receives immediately without skipping any $FF
          ; idle bytes.

recvstk:    lda   r3                    ; get inline length to receive
            plo   re

rstkloop:   dec   re                    ; decrement received bytes count,
            ldi   255                   ;  set shift register to count bits

rstkzero:   shl                         ; shift in zero, exit loop if a byte
            bnf   rstkdone

rstknext:   req                         ; clock next bit, branch if zero
            seq
            bn2   rstkzero

            shlc                        ; shift in one, loop if not a byte
            bdf   rstknext

rstkdone:   dec   r2                    ; save byte to buffer
            str   r2

            glo   re
            bnz   rstkloop

            sep   r3                    ; return with df clear

            lda   r3
            plo   r9


          ; Receive bytes through SPI into memory at RF for RE.0 bytes. This
          ; immediately puts bytes into memory without skipping any leading
          ; $FF bytes like recvspi does. A count of 0 means 256 bytes.

recvbuf:    lda   r3                    ; get inline length to receive
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

recvdone:   str   rf                    ; save byte to buffer
            inc   rf                    ; move past received byte

            glo   re
            bnz   recvloop

            sep   r3                    ; return with df clear

            lda   r3
            plo   r9


          ; Stores card capacity flag.

ocrreg:     db    0


          ; Module name for minfo to display

            db    0,'Diego',0


modend:   ; This is the last of what's copied to the heap.

end:      ; That's all folks!

