; **************************************************************** ;
; Fifty1Ford/NormalLuser 
; Ben Eater Breadboard Bad Apple! Demo 
; for the Ben Eater 6502 and Worlds Worst Video Card Breadboard kits
;
; 1 Byte encode with Run Length, Differential and Tri Pixel Encoding
; Zero page self modifying RLE
; 56.77 Frames a second average decode without vsync/beep
; 20.86 : 1 compression ratio with python encoder
; Automatic 30 FPS Vsync with catch-up
;
; Hardware: start with stock Ben Eater 6502+Worlds Worst Video Card kits
; You will need 2 jumper wires Extra bypass capacitors may help with stability
;
; ***************** NOTE *****************************************
;
; A wire connecting the VGA Vsync signal to the NMI pin on the CPU
; is used for a Vsync IRQ 
; If this is not connected the system will hang unless you 
; turn off VSync in the Debug Options
;
; Nothing else should be connected to the NMI pin but the Vsync from the VGA
;
; ****************************************************************
;
; First disconnect the LCD and any buttons from the VIA 
; Next clock your system to 5 mhz 
; Use first counter output of VGA for 5mhz CPU clock instead of the 1 Mhz clock
; Set to Run in both hsync and vsync (1 pixels on left side will have a noise line at 5mhz)
; This gives you 14Mhz effective CPU speed
; Now connect the Vsync from the VGA output to the NMI pin of the CPU
; SD Card input is PA0 MISO to make the serial code fast nothing else is connected
; USE CA2 as clock to to SCLK save more cycles!!!
; Others are pins on VIA Port B.
; Did you connect the Vsync to the NMI?
; SD CARD BOOT AND READ ROUTINE BASED ON GFOOT SD CARD 6502 GUIDE AND HELP!
;
; Extra bypass capacitors may help with stability Use several per power rail
;
; ***************** NOTE *****************************************
; Boot ROM must have a NMI rouine with :
;  DEC $0C
;  For V-Sync to work
;  NMI Music routine needed in boot rom for music to play
; ****************************************************************

; Constants:
MusicStart = $8A00
BeepFileBottom = $8A;$89;>MusicStart;$0A ; This is the music file for Bad Apple
BeepFileTop    = $C6;$C5;>MusicEnd;$1D   ; 60 beeps a second ~15KB
MusicEnd = $C6000
; Music and Woz in ROM
; Add Music Data Page aligned to start
   .org MusicStart
;MusicStart:
    incbin "C:\Projects\6502\MusicSFXIntroEmptyOutro.bin"
 .align 8 ; Mark the end of the music file
    .byte 'ZUN'

Programstart    = $8000;$300                  ; $F700 ; Page align!
RLERenderstart  = Programstart + 512    ; 768;512  ; Page align!
Arraystart      = RLERenderstart + 256  ; Page align! Where code is moved to RAM on startup
;Woz            = Byte align to end of this code for now.
; *********************** DEBUG OPTIONS ****************************
VsyncOn = 1   ; Automatic Screen pointer Roll-Over/IRQ Vsync.  
BeepOn  = 0   ; Load music from a range of pages to the IRQ Beep buffer
Debug   = 0   ; Put a Red Pixel in top left corner if we don't wait for Vsync. 
VSZero  = 225 ; 'Zero Point' for the Vsync counter
; ******************************************************************

NumberOfBlocks = 2  ; 512 bytes is 2 blocks. For SD card CRC count.

BeepBuffer   = $1E00     ; IRQ Beep Buffer Location ** don't move without changing IRQ code
; I/O:
Display      = $2000     ; start of memory mapped display 100x64 mapped to 128x64
ACIA        = $5000 
ACIA_DAT    = ACIA
ACIA_SR     = ACIA+1
ACIA_CMD    = ACIA+2
ACIA_CTRL   = ACIA+3
VIA              = $6000
VIA_PORTB        = VIA
VIA_PORTA        = VIA+1 ;$6001
VIA_DDRB         = VIA+2 ;$6002
VIA_DDRA         = VIA+3 ;$6003
VIA_T1CL         = VIA+4 ;$6004
VIA_T1CH         = VIA+5 ;$6005
VIA_T1LL         = VIA+6 ;$6006
VIA_T1LH         = VIA+7 ;$6007
VIA_T2CL         = VIA+8 ;$6008
VIA_T2CH         = VIA+9 ;$6009
VIA_SHIFT        = VIA+10;$600A
VIA_AUX          = VIA+11;$600B ;Set to 0 to stop BEEP ALSO ACR
VIA_PCR          = VIA+12;$600C
VIA_IFR          = VIA+13;$600F
VIA_IER          = VIA+14;$600E
VIA_IORA         = VIA+15;$600F
RelocateLocation = $100-RelocateLength ; Zero Page Baby! Put the routine just before Stack   
TossBitsLocation = $100  ; Bottom of Stack!
ZP               = $1    ; Start ZP at $1 to save the $0 spot for good luck! :)

; Move all the ZP to start so I can use top of ZP and bottom of stack for code
;Vsync Beep for Bad Apple!
BeepFile          = ZP + 0        ; Location of audio in memory
BeepFileH         = ZP + 1        ;
Block_Counter     = ZP + 2        ; ZP counter for 512 Block CRC bytes
BlockTwo          = ZP + 3       ; Counting to 256 twice
; Bad Apple!!
RLECount          = ZP + 4   
ReadByteTemp      = ZP + 5 
BackColor         = ZP + 6        ; Background/Color command color
PlotColor         = ZP + 7        ; Color for plot function 
Screen            = ZP + 8        ; GFX screen location
ScreenH           = ZP + 9        ; to draw TO
VidCounter        = ZP + 10       ;***** IF NMI hooked up to vsync below will DEC ***** 
VGAClock          = ZP + 11       ; $C is for Clock!    
;VGASync          =               ; Unused ***** IF NMI hooked up to vsync this will be 1 after sync(s), you set to zero yourself ***** 
zp_sd_cmd_address = ZP + 12
BeepEnable        = ZP + 13   ; ZP ** don't move without changing IRQ code
BeepCount         = ZP + 14   ; ZP ** don't move without changing IRQ code
BeepRead          = ZP + 15   ; ZP ** don't move without changing IRQ code
BeepWrite         = ZP + 16   ; ZP ** don't move without changing IRQ code


; WozMon with NormalLuser Fast Binary Load
IN          = $1F00 ;$0200;*Input buffer Moved to between Audio Buffer and Screen. In case of overdraw, no issue.
XAML        = ZP + 0             ;*Index pointers
XAMH        = ZP + 1 
STL         = ZP + 2 
STH         = ZP + 3 
L           = ZP + 4 
H           = ZP + 5 
YSAV        = ZP + 6 
MODE        = ZP + 7 
MSGL        = ZP + 8 
MSGH        = ZP + 9 
COUNTER     = ZP + 10 
CRC         = ZP + 11 

CRCCHECK    = ZP + 17

NMI_TX      = ZP + 18       ; Saves 1 cycle by sty/ldy instead of PHY/PLY 6 vrs 7
NMI_TY      = ZP + 19       ; Saves 1 cycle by sty/ldy instead of PHX/PLX 6 vrs 7
NMI_TA      = ZP + 20       ; Saves 1 cycle by sty/ldy instead of PHA/PLA 6 vrs 7

RTI_TX      = ZP + 21       ; Saves 1 cycle by sty/ldy instead of PHY/PLY 6 vrs 7
RTI_TY      = ZP + 22       ; Saves 1 cycle by sty/ldy instead of PHX/PLX 6 vrs 7
RTI_TA      = ZP + 23       ; Saves 1 cycle by sty/ldy instead of PHA/PLA 6 vrs 7

TempY       = ZP + 24       ; Saves 1 cycle by sty/ldy instead of PHA/PLA 6 vrs 7
LastZP      = ZP + 25




  .org Programstart ; Page align this program in RAM/ROM

;reset: ; Setup SD card and Vars, Copy main decode loop with self modifying code to ZeroPage
BadAppleStart:
  jmp BootUp ; Returns to FrameLoop below jmp keeps program load and start address the same

; FrameLoop is the Entry Point. Moved to ZP
sRstTop:     ; Frame Roll-Over routine 
;****************************************************************************
;***************************** FRAME SYNC *********************************** 
;****************************************************************************
  ;lda #$20   ;New Frame starts at $2000   
  ;sta ScreenH;Reset screen pointer to first pixel
  lsr ScreenH ;Same 5 cycles as above, but less bytes
; Since we just rolled-over the screen buffer that means that we are done with the frame
; Do Vsync routine below:
; This is easier to understand without 'if' statements:
; .org Vsync routine without Debug
; Vsync: 
;   ldx VGAClock ;IRQ does a DEC on Vsync
;   cpx #250     ;Using 250 as 'zero' sync
;   bcs EGVsync ; = > 250 either wait or sync
; Synced:; It is less than 250, Can't wait Try to catch up
;   inc VGAClock ;Add two for 30 Frames a Second
;   inc VGAClock ;VGA is 60 Frames a Second 
;   ;jsr GetBeep ;Put two notes per frame in music buffer
;   ;jsr GetBeep ;60 vsync a sec on VGA with 30 FPS so 2 notes
;   jmp FrameLoop;** start next frame decode **
; EGVsync: ; = > 250 It is a wait or a sync
;   beq Synced  ; = 250 Perfect Vsync Done waiting
;   jmp Vsync   ; > 250 Not Time yet Wait for Vsync


Vsync: ; Debug version I use assembler ' if ' statments here for 
        ; debug with no Vsync and a  'slow' pixel
 if VsyncOn=1
    ldx VGAClock ;IRQ does a DEC on Vsync
    cpx #VSZero ; Using this as 'zero' sync
    bcs EGVsync ; = > VSZero wait or sync   
    
    if Debug=1 ; Show Red if we don't wait for Vsync. 
     lda #$20 ; Red Color. Frame may be 'just in time' and show Red.
     sta $2002; Set Top Left Pixel Red. But we are prob late if we do this.
     jmp NextSync; Less than VSZero no wait Try to catch up
    endif

Synced:
    if Debug=1
     lda #$0  ; Black
     sta $2002; Set Top Left Pixel Black
    endif

NextSync:
    ;Two inc's is 10 cycles, the same as ADC routine with clc, but 3 less bytes
     inc VGAClock ;Add two for 30 Frames a Second
     inc VGAClock ;VGA is 60 Frames a Second 
     jmp FrameLoop;** start next frame decode **

EGVsync: ; = > 250 It is a wait or a sync
    beq Synced  ; = 250 ;Good Vsync Done waiting
    jmp Vsync   ; > 250 ;Wait for Vsync 
 endif; if VsyncOn=1
 
 if VsyncOn=0
   jmp FrameLoop;** start next frame decode NOW **
 endif

 
;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;****************************************************************************
;************************** ZP RELOCATED CODE *******************************
;****************************************************************************
RelocateLength=144       ; Size of routine without ' jmp TossBits' at the end. 
RelocateCode:            ; All for the RLE 34 byte function in ZP to shave a couple of cycles
 .rorg RelocateLocation  ; ZeroPage location

; Chose to draw 3 pixels every time even if only 1 pixel changes
; I think this is a faster decode loop.
TriPixel:      ; 3 pixel run 70 cycles including SD read
  lda VIA_PORTA ; Load 6 bits for up to 64 values for each array
  asl           ; IE there are 64 possible 3 pixel combinations of
    ora VIA_PORTA;the 4 colors used   
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    
  tax           ; Color/index to x
  lda Array1,x ; Load 1 of 4 colors B/DG/LG/W for Bad Apple!
  sta (Screen),y; Draw it!
  iny           ; Next Pixel
  lda Array2,x ; Load 1 of 4 colors 
  sta (Screen),y; Draw it!
  iny           ; Next Pixel
  lda Array3,x ; Load 1 of 4 colors 
  sta (Screen),y; Draw it! 
  sta PlotColor ; Store last color used for Repeats
TriDone:
  jmp FrameLoop ; Decode another byte

SkipRun: ;Just add this amount to the screen pointer
; Load 7 bits for a skip value up to 127
 lda VIA_PORTA
  asl
  ora VIA_PORTA 
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  
  sta RLECount  ; Store because of adc of y below
 ;clc           ; Not needed because of shifts above! Save 2 cycles each skip!
  tya 
  adc RLECount  ; Need to add RLE count to Y  
  tay           ; Transfer back to Y
  lda ScreenH   ; increment the top byte of the screen pointer
  adc #$00      ; Add any carry leftover from ADC above
  sta ScreenH   ; Since we always encode skips on the edge of the screen we only need to check
  cmp #$40      ; for the screen roll-over at $4000 in the Skip routine
  bne FrameLoop ; FrameLoop is more likley than Reset Top. 
  jmp sRstTop  ;  Saves 2 cycles more often than not over the reverse branch order. Now that I'm in ZP I need the jmp


;****************************************************************************
;************************* MAIN DECODE LOOP ********************************* 
;*************************    - start -     ********************************* 
;****************************************************************************
FrameLoop:
  dec Block_Counter ; must count every 512 bytes read from SD card
  beq BLOCK        ; 256 roll-over go to BLOCK routine
;************************** >>>ENTRY POINT<<< *******************************  
FrameLoopstart:    ; jmp here to start and for SD card BLOCK return
; Load Control Bit 1 SD outputs Bit 8 first, bit 1 last
  lda VIA_PORTA; Read bit 8 of byte from SD card 
  bne SkipRun ; If 1 Skip Pixels
; Load Control Bit 2  
  lda VIA_PORTA ; Else check bit 7
  beq TriPixel ; If 0 TriPixel
 ; Save 3 cycles per line Can fall through to RLE now that Decode is in ZeroPage
 ;jmp RLE      ; Else 1 New RLE self modifies, needs to be in RAM

; Whole reason all of this code is moved to ZP and the stack is to save just a few cycles
; on the RLE routine below!
RLE: ; Self Modify Run Length Draw Function
; Load 6 bits for a repeat value of up to 64
  lda VIA_PORTA
  asl
  ora VIA_PORTA 
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
  asl
  ora VIA_PORTA
 ; I could Bit shift and ADC then subtract from the length of the 
 ; routine instead of using a table, but that would take more cycles
  tax            ; RLE count to x
  lda RLEArray,x ; Load jmp location, faster than x3
  sta RLEJump+1  ; Store low byte location, high byte is unchanged
  lda PlotColor  ; Last pixel color used in TriPixel
RLEJump:         ; Self modify that code!  
  jmp RLERender  ; Jump to RLE count in RLE routine

BLOCK:              ; Block_Counter rolled over -256 count
  dec BlockTwo      ; We need to count to 512 so we will do this twice
  bne FrameLoopstart; only 256, keep going
 ; jmp TossBits      ; It's been 512 bytes, toss the bits!
  ; Fall through to TossBits at bottom of Stack!
  ; The stack only uses a few bytes during Bootup and is set to FF at startup
  ; It only would save 3 cycles per block. 29 cycles a frame is not much help...
  ; But a cycle saved is a cycle faster!
 rend            ;End Relocation
 
;****************************************************************************
;************************* STACK RELOCATED CODE *****************************
;****************************************************************************
 ; TossBits only uses a jmp at the end, no need to rorg
 ; rorg TossBitsLocation   ; Stack after above in ZP
TossBits:            ; 320 CYCLE, 249 BYTE Routine. Hard to place for easy branch. 
TossBitsLen = 249    ; jmp return used
 lda #NumberOfBlocks ; Reset top Byte Counter, we know bottom rolled over to 0, 2 blocks
 sta BlockTwo        ; Set once and Count down and save Two cmp#? 
; Must throw away 10 bytes every block read from the SD card No choice 
 ;Read Byte 1
 bit VIA_PORTA 
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read Byte 2
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read Byte 3
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read Byte 4
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read Byte 5
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read Byte 6
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read Byte 7
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read Byte 8
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read Byte 9
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read Byte 10
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
; Done tossing bytes
 jmp FrameLoopstart ; Back to decoding entry
; rend            ;End Relocation
;****************************************************************************
;************************** END RELOCATED CODE ******************************
;****************************************************************************
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
RelocateEnd:

;RLERender must start on page!
  .org RLERenderstart
RLERender:  ; Unrolled Line draw/Run Length Routine
; This is 8.2 cycles a pixel vrs 13.3 for a loop
;More than 60% faster Nice!
  sta (Screen),y; Draw it! 64
  iny ; Next pixel
 sta (Screen),y; Draw it! 63
  iny ; Next pixel
  sta (Screen),y; Draw it! 62
  iny ; Next pixel
  sta (Screen),y; Draw it! 61
  iny ; Next pixel
  sta (Screen),y; Draw it! 60
  iny ; Next pixel
  sta (Screen),y; Draw it! 59
  iny ; Next pixel 
  sta (Screen),y; Draw it! 58
  iny ; Next pixel
  sta (Screen),y; Draw it! 57
  iny ; Next pixel
  sta (Screen),y; Draw it! 56
  iny ; Next pixel
  sta (Screen),y; Draw it! 55
  iny ; Next pixel
  sta (Screen),y; Draw it! 54
  iny ; Next pixel
  sta (Screen),y; Draw it! 53
  iny ; Next pixel
  sta (Screen),y; Draw it! 52
  iny ; Next pixel
  sta (Screen),y; Draw it! 51
  iny ; Next pixel
  sta (Screen),y; Draw it! 50
  iny ; Next pixel
  sta (Screen),y; Draw it! 49
  iny ; Next pixel
 
 sta (Screen),y; Draw it! 48
  iny ; Next pixel
  sta (Screen),y; Draw it! 47
  iny ; Next pixel
  sta (Screen),y; Draw it! 46
  iny ; Next pixel
  sta (Screen),y; Draw it! 45
  iny ; Next pixel
  sta (Screen),y; Draw it! 44
  iny ; Next pixel
  sta (Screen),y; Draw it! 43
  iny ; Next pixel
  sta (Screen),y; Draw it! 42
  iny ; Next pixel
  sta (Screen),y; Draw it! 41
  iny ; Next pixel
  sta (Screen),y; Draw it! 40
  iny ; Next pixel
  sta (Screen),y; Draw it! 39
  iny ; Next pixel
  sta (Screen),y; Draw it! 38
  iny ; Next pixel
  sta (Screen),y; Draw it! 37
  iny ; Next pixel
  sta (Screen),y; Draw it! 36
  iny ; Next pixel
  sta (Screen),y; Draw it! 35
  iny ; Next pixel
  sta (Screen),y; Draw it! 34
  iny ; Next pixel
  sta (Screen),y; Draw it! 33
  iny ; Next pixel
  
 sta (Screen),y; Draw it! 32
  iny ; Next pixel
  sta (Screen),y; Draw it! 31
  iny ; Next pixel
  sta (Screen),y; Draw it! 30
  iny ; Next pixel
  sta (Screen),y; Draw it! 29
  iny ; Next pixel
  sta (Screen),y; Draw it! 28
  iny ; Next pixel
  sta (Screen),y; Draw it! 27
  iny ; Next pixel
  sta (Screen),y; Draw it! 26
  iny ; Next pixel
  sta (Screen),y; Draw it! 25
  iny ; Next pixel
  sta (Screen),y; Draw it! 24
  iny ; Next pixel
  sta (Screen),y; Draw it! 23
  iny ; Next pixel
  sta (Screen),y; Draw it! 22
  iny ; Next pixel
  sta (Screen),y; Draw it! 21
  iny ; Next pixel
  sta (Screen),y; Draw it! 20
  iny ; Next pixel
  sta (Screen),y; Draw it! 19
  iny ; Next pixel
  sta (Screen),y; Draw it! 18
  iny ; Next pixel
  sta (Screen),y; Draw it! 17
  iny ; Next pixel
 
 sta (Screen),y; Draw it! 16
  iny ; Next pixel
  sta (Screen),y; Draw it! 15
  iny ; Next pixel
  sta (Screen),y; Draw it! 14
  iny ; Next pixel
  sta (Screen),y; Draw it! 13
  iny ; Next pixel
  sta (Screen),y; Draw it! 12
  iny ; Next pixel
  sta (Screen),y; Draw it! 11
  iny ; Next pixel
  sta (Screen),y; Draw it! 10
  iny ; Next pixel
  sta (Screen),y; Draw it! 9
  iny ; Next pixel
  sta (Screen),y; Draw it! 8
  iny ; Next pixel
  sta (Screen),y; Draw it! 7
  iny ; Next pixel
  sta (Screen),y; Draw it! 6
  iny ; Next pixel
  sta (Screen),y; Draw it! 5
  iny ; Next pixel
  sta (Screen),y; Draw it! 4
  iny ; Next pixel
  sta (Screen),y; Draw it! 3
  iny ; Next pixel
  sta (Screen),y; Draw it! 2
  iny ; Next pixel
  sta (Screen),y; Draw it! 1
  iny ; Next pixel
 jmp FrameLoop ; Decode another byte

; Moved here because there was space.
cmd0_bytes 
  byte $40, $00, $00, $00, $00, $95
cmd8_bytes
  byte $48, $00, $00, $01, $aa, $87
cmd55_bytes
  byte $77, $00, $00, $00, $00, $01
cmd41_bytes
  byte $69, $40, $00, $00, $00, $01
MSGBadApple: byte "NormalLuser BenEater Bad Apple!! 2024"
 
  .org Arraystart;Page align
  .align 8
RLEArray: ;Decending Array of 3 byte skips I don't think I actually use zero? 
 ; I should use 0 for ' 1 ' but just dup first entry for now Do I have RLE's of 3 or less?
    BYTE 189,189,186,183,180,177,174,171,168,165,162 ; 11
        BYTE 159,156,153,150,147,144,141,138,135,132 ; 10
        BYTE 129,126,123,120,117,114,111,108,105,102 ; 10
        BYTE  99, 96, 93, 90, 87, 84, 81, 78, 75, 72 ; 10
        BYTE  69, 66, 63, 60, 57, 54, 51, 48, 45, 42 ; 10
        BYTE  39, 36, 33, 30, 27, 24, 21, 18, 15, 12 ; 10
    BYTE       9,  6,  3; , 0,  0, 0 ;extra zeros just in case
                  ; First few bytes of next array are same as last of RLEArray
 ;.org Arraystart ; We can save some space and mash these arrays into the same page
Array1:          ; Page align below for fast code without using zero page
    byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63
    byte 63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21
    byte 42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
Array2: ; Keeps it the same 4 cycles as zero page (Array1),x 
    byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63
    byte 0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21
    byte 42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
Array3: ; This is 192 bytes of all possible 3 pixel Greyscale values
    byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0
    byte 21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21
    byte 42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63


;****************************************************************************
;********************************* BOOT *************************************
;****************************************************************************

BootUp: ;SD card and system boot routines
        ;These only run once at startup and are not optimized
            cld             ; Clear decimal arithmetic mode
            cli             ; Turn on IRQ
            ldx #$FF        ; Lets start at the top of the stack
            txs             ; Nice and clean,  with room on the bottom if we need it
           

      stz  NMI_TA       ; Changed to ZP to save 1 cycle over PH/PL
      stz  NMI_TX       ; Also, leaves the stack alone (Aside from NMI RTI address and Processor Status flags.)
      stz  NMI_TY 

; SD card setup:
; Need to read 512 bytes Then throw away 10 CRC bytes
; Unrolling might help a tiny bit but it seems silly to
; double up the entire decoder just to do that?
    stz Block_Counter; So a 16 bit counter it is
    lda #NumberOfBlocks; 2 Blocks
    sta BlockTwo  ;We'll cound down 
    stz VIA_PORTA ;Port A empty Just in case
    lda #254         ; ??? #$ff-SD_BIT_MISO 
    sta VIA_DDRA     ; Don't remember why I do this??
    
    lda #$00
    sta Screen ; Ben Eater's Worlds Worst Video card 
    lda #$20   ; uses the upper 8Kb of system RAM
    sta ScreenH; This starts at location $2000
    
;  if BeepOn=1
; ; Setup some commands for music    
    stz BeepRead
    stz BeepWrite
    stz BeepCount
    stz BeepEnable
    
    
    ldy #0  
    ldx #0
    lda #255
    sta  VIA_DDRB;            MAKE EVERYTHING OUTPUT! $6002   
    lda  VIA_AUX;  $600b      This would be a retangle wave instead of a square wave, but no real matter    
                            ; Set the two high bits in the ACR to get the
    ORA  #$C0   ;  1100-0000; square-wave/RECTANGLE output on PB7  (Don't enable the
    sta  VIA_AUX;  $600b    ; T1 interrupt in the IER though)  
    
    lda #$0a ; Make CA2 pulse each time port A is read tnx gfoot!
    sta VIA_PCR

    ; Do screwy stuff to get SD card started up in SPI mode 
    jsr sd_init ;Thanks George Foot!

    ; Setup SD card to read forever
    lda #SD_MOSI
    sta  VIA_PORTB 
    lda #$52 ; CMD18 - READ_MULTI_BLOCK
    ;WOW, that easy! Change $51 to $52 and I can stream
    ;bytes off the SD card forrever! Yea!
    ;Just have to remember to throw away the 10 byte
    ;CRC every 512 bytes Other than that, the bits stream forever Yea!
    jsr sd_writebyte
    lda #$00           ; sector 24:31
    jsr sd_writebyte
    lda #$00           ; sector 16:23
    jsr sd_writebyte
    lda #$00           ; sector 8:15
    jsr sd_writebyte
    lda #$00           ; sector 0:7
    jsr sd_writebyte
    lda #77;RLECount   ; crc (not checked, random data sent)
    jsr sd_writebyte

    jsr sd_waitresult
    cmp #$00
    beq readsuccess
    jmp BadAppleStart;reset ;This change makes it pretty reliable after a reboot

readsuccess
  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  beq SdBooted
  ;Retry until it works
  jmp BadAppleStart;reset
SdBooted
  ;SD card booted, setup some values

  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta VIA_PORTB 
  ; Port A is in Clock pulse mode
  ; A read will pulse the clock on pin CA2 of the 6522 
  ; We will stream forever after that    
              
              
  jsr NMIBeepInit

;********************************* Zero Page Copy *************************************
  ; Done with SD setup. Also done with the Stack..
  ; Stack? Did you say Stack? 
  ; We won't need any Stack where we're going!! 
    ldx #$FF
    txs
; Need to copy the RLE self modify code to Zeropage RAM. 
; Most of the code comes along for the ride. 
    ldx #0
RelocateLoop ;routine to move code to ZP
    lda RelocateCode,x
    sta RelocateLocation,x 
    inx
    cpx #RelocateLength
    bne RelocateLoop
;********************************* End Zero Page Copy *********************************
;*********************************     Stack  Copy    *********************************    
; I need to put TossBits in the stack to fall through from BLOCK in the main loop.
    ldx #0
RelocateLoop2 ;routine to move code Stack
    lda TossBits,x
    sta $100,x 
    inx
    cpx #TossBitsLen
    bne RelocateLoop2
  ldx #0
;********************************* End Stack Copy *************************************    




;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;*******************************************************************************
;****************************** XX DECODE XX  **********************************
;******************************  -  START -   **********************************
;*******************************************************************************
  lda VIA_PORTA ; toggle the clock once at the start to prime the SD card
  ;     -WE ARE STREAMING BITS FROM THE SD CARD NOW IN PORT A PA0!!
  lda #VSZero+2;#252    ; 250 is our Vsync 'Zero' and we want 2 Vsync per frame (30fps)
  sta VGAClock; start Vsync just before starting decode

     lda #$00 ;Make it easy, start on a page
     sta BeepFile  ;Beep Rom data $A000 while testing Change to ROM 8400
     lda #BeepFileBottom ;#$84 
     sta BeepFileH

    lda #1 ; start the music now 
    sta BeepEnable
;********************* XX Jump to video stream decode now! XX ******************
  jmp FrameLoopstart ;Decoder entry point Toss some frames on that screen!
;*******************************************************************************
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\




;****************************************************************************
;******************************* SD INIT ************************************
;**************************************************************************** 
SD_CS   = $20 ;%00100000
SD_MOSI = $8  ;%00001000
sd_init:
;  Thanks George Foot! https://githubcom/gfoot/sdcard6502 and for all your help on
;  6502.org forums and https://wwwredditcom/r/beneater/ !!
;
; Since the SD boot only runs at the start there is no real need to optimize this if it works
; I use Kingston Canvas Select Plus 32GB micro SD cards
; 2 pack with SD adapter for less than $8 at normal online places
; This code does a good job of booting them up I've used 4 so far without any issues
  ; Let the SD card boot up, by pumping the clock with SD CS disabled
  ; We need to apply around 80 clock pulses with CS and MOSI high
  ; Normally MOSI doesn't matter when CS is high, but the card is
  ; not yet is SPI mode, and in this non-SPI state it does care
  lda #SD_CS | SD_MOSI
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
preinitloop:
  sta  VIA_PORTB
  ldy VIA_PORTA 
  dex
  bne preinitloop
  
cmd0 ; GO_IDLE_staTE - resets card to idle state, and SPI mode
  lda #<cmd0_bytes
  sta zp_sd_cmd_address
  lda #>cmd0_bytes
  sta zp_sd_cmd_address+1
  jsr sd_sendcommand
  sta VIA
  ; Expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;initfailed

cmd8 ; SEND_IF_COND - tell the card how we want it to operate (33V, etc)
  lda #<cmd8_bytes
  sta zp_sd_cmd_address
  lda #>cmd8_bytes
  sta zp_sd_cmd_address+1
  jsr sd_sendcommand
  ; Expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;initfailed
  ; Read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte

cmd55 ; APP_CMD - required prefix for ACMD commands
  lda #<cmd55_bytes
  sta zp_sd_cmd_address
  lda #>cmd55_bytes
  sta zp_sd_cmd_address+1
  jsr sd_sendcommand
  ; Expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;initfailed

cmd41 ; APP_SEND_OP_COND - send operating conditions, initialize card
  lda #<cmd41_bytes
  sta zp_sd_cmd_address
  lda #>cmd41_bytes
  sta zp_sd_cmd_address+1
  jsr sd_sendcommand
  ; status response $00 means initialised
  cmp #$00
  beq initialized
  ; Otherwise expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;initfailed
  ; Not initialized yet, so wait a while then try again
  ; This retry is important, to give the card time to initialize
  ;jsr delay NOT THAT IMPORTANT? Works with just a nop removed delay
  nop ;nop instead of a delay
  bra cmd55
initialized
  rts

; Moved into some empty space above
; cmd0_bytes
;   byte $40, $00, $00, $00, $00, $95
; cmd8_bytes
;   byte $48, $00, $00, $01, $aa, $87
; cmd55_bytes
;   byte $77, $00, $00, $00, $00, $01
; cmd41_bytes
;   byte $69, $40, $00, $00, $00, $01

sd_readbyte: ;Only used by SD INIT
    lda #SD_MOSI
    sta VIA_PORTB ; set MOSI

    lda VIA_PORTA ; toggle the clock once at the start
    lda VIA_PORTA ; actual load of first bit
    asl
    ora VIA_PORTA 
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_PORTA
    asl
    ora VIA_IORA;VIA_PORTA ; read last bit without causing a clock pulse Cool!
    rts

sd_writebyte:
  ; Tick the clock 8 times with descending bits on MOSI
  ; SD communication is mostly half-duplex so we ignore anything it sends back here
  ldx #8                      ; send 8 bits
swloop:
  asl                         ; shift next bit into carry
  tay                         ; save remaining bits for later
  lda #0
  bcc sendbit                ; if carry clear, don't set MOSI for this bit
  ora #SD_MOSI
sendbit:
  sta VIA_PORTB               ; set MOSI (or not) first with SCK low
  ;eor #SD_SCK                ; Clock not needed anymore
  sta VIA_PORTB               ; raise SCK keeping MOSI the same, to send the bit
  bit VIA_PORTA               ; * New clock for handshake *
  tya                         ; restore remaining bits to send
  dex
  bne swloop                   ; loop if there are more bits to send
  rts
  
sd_waitresult:
  ; Wait for the SD card to return something other than $ff
  jsr sd_readbyte
  cmp #$ff
  beq sd_waitresult
  rts

sd_sendcommand:
  ldx #0
  lda (zp_sd_cmd_address,x)
  lda #SD_MOSI           ; pull CS low to begin command
  sta  VIA_PORTB 
  ldy #0
  lda (zp_sd_cmd_address),y    ; command byte
  jsr sd_writebyte
  ldy #1
  lda (zp_sd_cmd_address),y    ; data 1
  jsr sd_writebyte
  ldy #2
  lda (zp_sd_cmd_address),y    ; data 2
  jsr sd_writebyte
  ldy #3
  lda (zp_sd_cmd_address),y    ; data 3
  jsr sd_writebyte
  ldy #4
  lda (zp_sd_cmd_address),y    ; data 4
  jsr sd_writebyte
  ldy #5
  lda (zp_sd_cmd_address),y    ; crc
  jsr sd_writebyte
  jsr sd_waitresult
  pha
  ; End command
  lda #SD_CS | SD_MOSI   ; set CS high again
  sta  VIA_PORTB 
  pla   ; restore result code
  rts
 .byte 'NormalLuser'



 ; Big changes, no longer generic sound effect and music code.
 ; Now you point it at a start and end memory location and it just loops until you stop it. 
 ; It only loops on the high byte, but is faster. 
 ; Small savings overall since this only runs 60 times a second and the old routine to put in the
 ; ring buffer only ran 30 times a second. I over-optimized anyway.

NMI: ;Needed for Bad Apple!! music.    
    ; Bad Apple!! Beep! routine:
    sta  RTI_TA         ; Changed to ZP to save 1 cycle over PH/PL
    dec VGAClock        ; Count down each Vsync, rolls over at 256
    ;       - Not using yet. Save the 5 cycles -
    ;lda #1             ; Always just a '1' if vsync happened since reset
    ;sta VGASync        ; Reset by user program. 
    lda BeepEnable      ; BeepEnable/IRQ ;$DF
    beq NMIExitNoBeep   ; IRQ Beep/buffer off. Changed so that it does not stop 
    sty RTI_TY          ; Also, leaves the stack alone (Aside from NMI RTI address and Processor Status flags.)
    ldy #0;NMI_TY 
; if BeepOn=1           ; Debug option to turn off sound
    lda (BeepFile),y    ; Need to load the note and see if it is Zero 0
    beq NMINoBeep       ; on 0 stop Beep/PB7 timer        
    sta VIA_T1CH;$6005; ; Beep! Store the note
    lda #$C0            ; square-wave/RECTANGLE output on PB7  (Don't enable the
    sta VIA_AUX         ; Turn the Beep back on if it is off.
    bra NMIInc
NMINoBeep:
    stz VIA_AUX ;$600b  ; Beep Off, buffer might still have data
NMIInc:
    clc ;10/14 cycle 16 bit inc is fastest I found Needs clc I think?
    inc BeepFile
    bne GBNoTop1
    inc BeepFileH
GBNoTop1: ;Don't inc high byte
    lda BeepFileH ; See if we are rolling over
    cmp #BeepFileTop ;#$D3
    beq GBeepFileRstTop1 ; Reset top byte
    bra GBeepDone1 ; Don't reset, continue
GBeepFileRstTop1:
    lda #BeepFileBottom;#$84;Reset To Beginning of file
    sta BeepFileH	
GBeepDone1:
; endif ;If BeepOn

NMIExit:
      ldy  RTI_TY        ; Changed to ZP to save 1 cycle over PH/PL
NMIExitNoBeep:
      lda  RTI_TA        ; Also, leaves the stack alone (Aside from NMI RTI address and Processor Status flags.)
      RTI

NMIBeepInit:
      lda VIA_AUX;$600b
      ora #$C0   ;1100-0000; square-wave/RECTANGLE output on PB7  (Don't enable the
      sta VIA_AUX;$600b    ; T1 interrupt in the IER though)
      lda #255             ; Set the T1 timeout period 
      sta VIA_T1CL;$6004   ; I wonder if I really need this?
      rts



;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;*******************************************************************************
;******************************  XX  WOZ  XX  **********************************
;******************************  -  START -   **********************************
;*******************************************************************************
 ;.org Woz

;MSG do not need to be byte aligned. Put before Woz since there is room.
MSG1        .byte "Woz FL: BadApple!! New Beep 8000 R ",0
MSG2        .byte "Load Data Start |  Data End",0
MSG3        .byte "        ",0
MSG4        .byte " -Start Binary File Transfer-",0
MSG5        .byte " All Bytes Imported -Done-",0
MSG6        .byte "-Timeout- NormalLuser Fast Binary Load",0
 align  8    ; Page align Woz
WozStart:    ; WozMon with L command for load,wider display, Rockwell ACIA routine
RESET
            CLD             ;Clear decimal arithmetic mode.
            CLI
            LDX #$FF        ;Lets start at the top of the stack.
            TXS             ;Nice
            

            LDA #$1F        ;* Init ACIA to 19200 Baud.
            STA ACIA_CTRL
            LDA #$0B        ;* No Parity. No IRQ
            STA ACIA_CMD
            
            JSR NewLine
            LDA #<MSG1
            LDX #>MSG1
            JSR SHWMSG      ;* Show Welcome.
            JSR NewLine
            JSR NewLine

;EDIT
            STZ BeepEnable
            STZ VIA_AUX
;END EDIT

            
SOFTRESET   LDA #$9B        ;* Auto escape.
NOTCR       CMP #$88        ;"<-"? * Note this was chaged to $88 which is the back space key.
            BEQ BACKSPACE   ;Yes.
            CMP #$9B        ;ESC?
            BEQ ESCAPE      ;Yes.
            INY             ;Advance text index.
            BPL NEXTCHAR    ;Auto ESC if >127.
ESCAPE      LDA #$DC        ;"\"
            JSR ECHO        ;Output it.
GETLINE     LDA #$8D        ;CR.
            JSR ECHO        ;Output it.
            LDA #$0A
            JSR ECHO
            LDY #$01        ;Initiallize text index.
BACKSPACE   DEY             ;Backup text index.
            BMI GETLINE     ;Beyond start of line, reinitialize.
            LDA #$A0        ;*Space, overwrite the backspaced char.
            JSR ECHO
            LDA #$88        ;*Backspace again to get to correct pos.
            JSR ECHO

NEXTCHAR    ;LDA ACIA_SR     ;*See if we got an incoming char
            ;AND #$08        ;*Test bit 3
            ;LDA io_getc    ; For Kowalski simulator use
            ;BEQ NEXTCHAR    ;*Wait for character
            ;LDA ACIA_DAT    ;*Load char
            JSR GETCHAR
            CMP #$60        ;*Is it Lower case
            BMI CONVERT     ;*Nope, just convert it
            AND #$5F        ;*If lower case, convert to Upper case
CONVERT     ORA #$80        ;*Convert it to "ASCII Keyboard" Input
            STA IN,Y        ;Add to text buffer.
            JSR ECHO        ;Display character.
            CMP #$8D        ;CR?
            BNE NOTCR       ;No.
            LDY #$FF        ;Reset text index.
            LDA #$00        ;For XAM mode.
            TAX             ;0->X.
SETSTOR     ASL             ;Leaves $7B if setting STOR mode.
SETMODE     STA MODE        ;$00 = XAM, $7B = STOR, $AE = BLOK XAM.
BLSKIP      INY             ;Advance text index.
NEXTITEM    LDA IN,Y        ;Get character.
            CMP #$8D        ;CR?
            BEQ GETLINE     ;Yes, done this line.
            CMP #$AE        ;"."?
            BCC BLSKIP      ;Skip delimiter.
            BEQ SETMODE     ;Set BLOCK XAM mode.
            CMP #$BA        ;":"?
            BEQ SETSTOR     ;Yes, set STOR mode.
            CMP #$D2        ;"R"?
            BEQ RUN         ;Yes, run user program.
            CMP #$CC        ;* "L"? LOAD Command check
            BEQ SETMODE     ;* Yes, set LOAD mode. NormalLuser addition
            ; CMP #$D0      ;* "P" test?
            ; BEQ PURGE     ;* Yes, Purge Memory.
            STX L           ;$00->L.
            STX H           ; and H.
            STY YSAV        ;Save Y for comparison.
NEXTHEX     LDA IN,Y        ;Get character for hex test.
            EOR #$B0        ;Map digits to $0-9.
            CMP #$0A        ;Digit?
            BCC DIG         ;Yes.
            ADC #$88        ;Map letter "A"-"F" to $FA-FF.
            CMP #$FA        ;Hex letter?
            BCC NOTHEX      ;No, character not hex.
DIG         ASL
            ASL             ;Hex digit to MSD of A.
            ASL
            ASL
            LDX #$04        ;Shift count.
HEXSHIFT    ASL             ;Hex digit left MSB to carry.
            ROL L           ;Rotate into LSD.
            ROL H           ;Rotate into MSD's.
            DEX             ;Done 4 shifts?
            BNE HEXSHIFT    ;No, loop.
            INY             ;Advance text index.
            BNE NEXTHEX     ;Always taken. Check next character for hex.
NOTHEX      CPY YSAV        ;Check if L, H empty (no hex digits).
            BNE NOESCAPE    ;* Branch out of range, had to improvise...
            JMP ESCAPE      ;Yes, generate ESC sequence.

RUN         JSR ACTRUN      ;* JSR to the Address we want to run.
            JMP SOFTRESET   ;* When returned for the program, reset EWOZ.
ACTRUN      JMP (XAML)      ;Run at current XAM index.
;---------------------NormalLuser edit--------------------------
NOESCAPE    LDA #$CC        ; NormalLuser Edit
            CMP MODE        ; Adding a 'L' Load mode.
            BEQ LOADBINARY  ; Match, LOAD a Binary file
;---------------------------------------------------------------              
            ;Back to Woz!
	        BIT MODE        ;Test MODE byte.
            BVC NOTSTOR     ;B6=0 for STOR, 1 for XAM and BLOCK XAM
            LDA L           ;LSD's of hex data.
            STA (STL, X)    ;Store at current "store index".
            INC STL         ;Increment store index.
            BNE NEXTITEM    ;Get next item. (no carry).
            INC STH         ;Add carry to 'store index' high order.
TONEXTITEM  JMP NEXTITEM    ;Get next command item.
NOTSTOR     BMI XAMNEXT     ;B7=0 for XAM, 1 for BLOCK XAM.
            LDX #$02        ;Byte count.
SETADR      LDA L-1,X       ;Copy hex data to
            STA STL-1,X     ;"store index".
            STA XAML-1,X    ;And to "XAM index'.
            DEX             ;Next of 2 bytes.
            BNE SETADR      ;Loop unless X = 0.
NXTPRNT     BNE PRDATA      ;NE means no address to print.
            LDA #$8D        ;CR.
            JSR ECHO        ;Output it.
            LDA #$0A
            JSR ECHO
            LDA XAMH        ;'Examine index' high-order byte.
            JSR PRBYTE      ;Output it in hex format.
            LDA XAML        ;Low-order "examine index" byte.
            JSR PRBYTE      ;Output it in hex format.
            LDA #$BA        ;":".
            JSR ECHO        ;Output it.
PRDATA      LDA #$A0        ;Blank.
            JSR ECHO        ;Output it.
            LDA (XAML,X)    ;Get data byte at 'examine index".
            JSR PRBYTE      ;Output it in hex format.
XAMNEXT     STX MODE        ;0-> MODE (XAM mode).
            LDA XAML
            CMP L           ;Compare 'examine index" to hex data.
            LDA XAMH
            SBC H
            BCS TONEXTITEM  ;Not less, so no more data to output.
            INC XAML
            BNE MOD8CHK     ;Increment 'examine index".
            INC XAMH
MOD8CHK     LDA XAML        ;Check low-order 'exainine index' byte
            ;AND #$0F       ;For MOD 8=0 ** changed to $0F to get 16 values per row **
            AND #$1F        ; For MOD 8=0 ** changed to $0F to get 32 values per row **
            BPL NXTPRNT     ;Always taken.
PRBYTE      PHA             ;Save A for LSD.
            LSR
            LSR
            LSR             ;MSD to LSD position.
            LSR
            JSR PRHEX       ;Output hex digit.
            PLA             ;Restore A.
PRHEX       AND #$0F        ;Mask LSD for hex print.
            ORA #$B0        ;Add "0".
            CMP #$BA        ;Digit?
            BCC ECHO        ;Yes, output it.
            ADC #$06        ;Add offset for letter.
; Rockwell/Non bugged WDC ACIA code 
; ** DO NOT USE with Ben Eater WDC based Serial Kit **
ECHO        PHA             ;*Save A
.WAIT       LDA ACIA_SR     ;*Load status register for ACIA
            AND #$10        ;*Mask bit 4.
            BEQ    .WAIT    ;*ACIA not done yet, wait.
            PLA
            PHA
            AND #$7F        ;*Change to "standard ASCII"
            STA ACIA_DAT    ;*Send it.
            ;STA io_putc    ;For Kowalski simulator use
            PLA             ;*Restore A
            RTS             ;*Done, over and out...

LOADBINARY:
; NormalLuser Fast Binary load. With Timeout.
; Quickly Load an program in Binary Format to memory.
; Usage: 2000 L 4000
; Will start load at location $2000 hex and stop at $4000 hex
; 0L9 -or 1 L 200 -or 100.200,L200 all work also with WOZ parsing
; Space can be saved on the messages. 
; Without any messages routine is under 70 bytes.
;
; STH and STL from Woz parser is Start address 
; H and L from Woz parser is End address 
;   
            PHP ; LETS TRY A GRACEFUL RETURN TO WOZ??
            PHA ; Kitchen sink.
            PHY ; Just push/pull everything?
            PHX ; 
            SEI             ; Turn off IRQ's, don't want/need.
            ;LDA #$1A        ; 8-N-1, 2400 baud
            ;LDA #$1C        ; 8-N-1, 4800 baud
            ;LDA #$1E        ; 8-N-1, 9600 baud
            ;LDA #$1F        ; 8-N-1, 19200 baud
            LDA #$1F         ;* Init ACIA to 19200 Baud.
            STA ACIA_CTRL
            LDA #$0B        ;* No Parity. No IRQ
            STA ACIA_CMD
            ;Below is just to display messages.
            JSR NewLine
            LDA #<MSG2
            LDX #>MSG2
            JSR SHWMSG      ; Hello Message.
            JSR NewLine
            LDA #<MSG3      ; Show address start/end for load
            LDX #>MSG3
            JSR SHWMSG      ; Space
            LDA #'$'
            JSR ECHO
            LDA STH
            JSR PRBYTE
            LDA STL
            JSR PRBYTE
            LDA #<MSG3
            LDX #>MSG3
            JSR SHWMSG      ;Space
            LDA #'$'
            JSR ECHO   
            LDA H
            JSR PRBYTE
            LDA L
            JSR PRBYTE
            
            JSR NewLine
            LDA #<MSG4
            LDX #>MSG4
            JSR SHWMSG      ;Start Data Transfer MSG.
            JSR NewLine
            ; Done with messages
            ; Load Address from WOZ
            LDA STL
            STA YSAV;TAY ;Y +  start address 0
            STZ STL
            LDY YSAV             ; Low byte in Y 
            JSR GETCHAR          ; Wait to grab first Byte from ACIA. No Timeout.
            JMP BINARY_LOOP_START; Store and do normal loop.
BINARY_LOOP: ; Could  copy GETCHAR here to save cycles.
            JSR GETCHARTO    ; Grab Byte from ACIA. With Timeout
            BCC BINARYTIMEOUT; It timed out. Exit.
BINARY_LOOP_START:           ; Got data. Did not timeout.
            STA (STL),Y      ; Store it at our memory location
            ; Comment out everything down to the to INY if you don't want status
            ; IF YOU WANT JUST STATUS USE:
            ;  LDA #'X' 
            ;  STA ACIA_DAT ;DON'T CARE IF IT GETS DROPPED JUST SEND
            
            ; Below translates to last HEX char for a nice ASCII output
            ; Not at all needed. It just looks neat.
;PRHEX      ; Move inline and just send only last char of Hex      
            AND #$0F        ;Mask LSD for hex print.
            ORA #$B0        ;Add "0".
            CMP #$BA        ;Digit?
            BCC HECHO        ;Yes, output it.
            ADC #$06        ;Add offset for letter.
HECHO:    
            AND #$7F               ;*Change to "standard ASCII"
            STA     ACIA_DAT;DATA  ; Output character.No wait or check so we don't slow down anything. May show garbage.
            ;sta io_putc           ; For Kowalski simulator use:
            ;Check memory pointer for max and INC
            LDX STH         ; Load our high byte
            CPX H           ; Does it match our max?
            BNE NO_HMATCH   ; Nope, just normal inc
            CPY L           ; Does the low byte match our max?
            BNE NO_HMATCH   ; Nope, just normal inc
            JMP BINARY_DONE ; MATCH! We are done!
NO_HMATCH:
            INY             ; Inc low byte
            BNE BINARY_LOOP ; jump if not a roll-over
            INC STH         ; Roll-over. Inc the high byte.
            JMP BINARY_LOOP ; Get more bytes
 
BINARY_DONE:; Data transfer Done
            JSR NewLine     ;New line.
            LDA #<MSG5
            LDX #>MSG5
            JSR SHWMSG      ;Show Finished msg

BINARYEXTRA:; Care about garbage data at end?
            ; *For Streaming Test,jmp back to top 
            ; JMP LOOPMEMORY -If you want to stream, like to the screen buffer.
            ; Could RTS here, but we could overwrite data.
            ; Could  copy GETCHARTO here to save cycles.
            JSR GETCHARTO    ; Grab Byte from ACIA. With Timeout
            BCC BINARYTIMEOUT; Time out
            LDA #'X'         ; 'Garbage' data past end
            JSR ECHO        
            JMP BINARYEXTRA
 
BINARYTIMEOUT: ; Must be finished sending.
            JSR NewLine     ;New line.
            LDA #<MSG6
            LDX #>MSG6
            JSR SHWMSG      ;Show Finished msg
            JSR NewLine     ;New line.
            ; Restore everything
            CLI
            PLX
            PLY
            PLA
            PLP
            ;RTS ;? ISSUES WITH THIS FOR SOME REASON
            JMP RESET       ;Restart Woz. Works well enough.


GETCHAR:    ; Will wait forever for a char, use for first char of timeout load
            LDA ACIA_SR ;STATUS     ; See if we got an incoming char
            AND #$08        ; Test bit 3
            ;LDA io_getc    ; For Kowalski simulator use
            BEQ GETCHAR     ; Wait for character
            LDA ACIA_DAT;DATA    ; Load char
            RTS

GETCHARTO:  ; Get data with Timeout
  phy ; Need to keep Y
;shell_rx_receive_with_timeout:; Started with mike42.me Xmodem timout routine
  ldy #$0 ; Thanks Mike!
y_loop:
  ldx #$0
x_loop:
  lda ACIA_SR;STATUS              ; Check ACIA status in inner loop
  and #$08                     ; Mask rx buffer status flag
  bne rx_got_char
  dex
  bne x_loop
  dey
  bne y_loop
  ply                          ; Need to keep Y
  clc                          ; No byte received in time. Clear Carry as flag
  rts
rx_got_char:
  lda ACIA_DAT ;DATA                ; Get byte from ACIA data port
  ply                          ; Need to keep Y
  sec                          ; Set Carry bit as flag
  rts


NewLine
            PHA
            LDA #$0D
            JSR ECHO        ;* New line.
            LDA #$0A
            JSR ECHO
            PLA
            RTS
 
SHWMSG      ; Changed msg routine to save some bytes
            ; Loads MSG Low byte A High byte X
            ;LDA #<MSG2
            ;LDX #>MSG2
            STA MSGL
            STX MSGH
            ;PHA ; I only msg when A and Y are unused.
            ;PHY ; Save the bytes
            ;jsr NewLine ;Always do a New Line anyway?
            LDY #$0
.PRINT      LDA (MSGL),Y
            BEQ .DONE
            JSR ECHO
            INY 
            BNE .PRINT
.DONE       ;PLY
            ;PLA
            RTS 

; Moved above into empty space
; ;MSG1        .byte "Woz FL: EhBasic E54E - BadApple F700",0
; MSG1        .byte "Woz FL: BadApple!! 8000 R ",0
; MSG2        .byte "Load Data Start |  Data End",0
; MSG3        .byte "        ",0
; MSG4        .byte " -Start Binary File Transfer-",0
; MSG5        .byte " All Bytes Imported -Done-",0
; MSG6        .byte "-Timeout- NormalLuser Fast Binary Load",0
WozEnd:


IRQ:
      RTI

   .org $FFFA
   word NMI
   word RESET
   word IRQ