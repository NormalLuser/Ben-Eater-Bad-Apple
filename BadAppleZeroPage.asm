; Fifty1Ford/NormalLuser Ben Eater Breadboard 6502 Bad Apple! Demo
;
; 1 Byte encode with Run Length, Differential and Tri Pixel Encoding
; Zero page self modifying RLE.
; 53.4 Frames a second average decode without vsync/beep
; 20:1 compression ratio python encoder
; Automatic 30 FPS Vsync with catch-up
;
; Hardware: Start with stock Ben Eater 6502+Worlds Worst Video Card kits
; You will need 2 jumper wires. Extra bypass capacitors may help with stability.
;
; ***************** NOTE *****************************************
;
; A wire connecting the VGA Vsync signal to the NMI pin on the CPU
; is used for a Vsync IRQ. 
; If this is not connected the system will hang.
;
; ****************************************************************
;
; Disconnect LCD and clock your system to 5 mhz. 
; Use first counter output of VGA for 5mhz CPU clock instead of the 1 Mhz clock.
; Run in both hsync and vsync (1 pixels on left side will have a noise line on top at 5mhz)
; This gives you 1.4Mhz effective CPU speed.
; I disconnect the little LCD and any buttons from the VIA while working with this.
; Connect the Vsync from the VGA output to the NMI pin of the CPU
; SD Card input is PA0 MISO
; to make the serial code FAST.
; I also have pulse mode on reads
; USE CA2 as clock to save more cycles!!!
; IE FAST..
; Others are pins on VIA Port B 
; Leave Port A empty except for MISO.
; Did you connect the Vsync to the NMI?
; SD CARD ROUTINE BASED ON GFOOT SD CARD 6502 GUIDE AND HELP
;
; Extra bypass capacitors may help with stability.
;
; ***************** NOTE *****************************************
;
; Boot ROM must have a NMI rouine with :
;  DEC $E2
;  Music routine needed in boot rom for music to play.
; ****************************************************************

ProgramStart    = $300 ; Page align!
RLERenderStart  = ProgramStart + 768  ; Page align!
RLEArrayStart   = RLERenderStart + 256; Page align!
RelocateLocation= $0 ;Zero Page Baby!


; *********************** DEBUG OPTIONS **************************
VsyncOn=1
Debug=1
BeepOn=1
; ****************************************************************

VIA              = $6000
VIA_PORTB        = VIA
VIA_PORTA        = VIA+1;$6001
VIA_DDRB         = VIA+2;$6002
VIA_DDRA         = VIA+3;$6003
VIA_AUX          = VIA+11;$600B ;Set to 0 to stop BEEP ALSO ACR
VIA_PCR          = VIA+12;$600C
VIA_IORA         = VIA+15;$600F
SD_CS   = $20 ;%00100000
SD_MOSI = $8  ;%00001000
; SD_SCK  = $10 ;%00010000;Clock moved to CA2
; Display      = $2000     ;Start of memory mapped display. 100x64 mapped to 128x64

zp_sd_cmd_address = $F0
; ZP addresses hold over from availible EhBasic ZP 
; Vsync Beep for Bad Apple!
; VGASync           = $E1 ;ZP ***** IF NMI hooked up to vsync this will be 1 after sync(s), you set to zero yourself.
VGAClock          = $E2   ;ZP ***** IF NMI hooked up to vsync this will DEC *****
; Beep/Music related:
BeepEnable        = $DC  ;** don't move without changing IRQ code
BeepCount         = $DD  ;ZP ** don't move without changing IRQ code
BeepRead          = $DE  ;ZP** don't move without changing IRQ code
BeepWrite         = $DF  ;ZP** don't move without changing IRQ code
BeepBuffer        = $1E00 ;** don't move without changing IRQ code

BeepFile          = $E5       ; Location of audio in memory
BeepFileH         = $E6       ;

RLECount          = $E3  
PlotColor         = $EC       ; Color for plot function 
Screen            = $ED       ; GFX screen location
ScreenH           = $EE       ; to draw TO
Block_Counter     = $E0       ; ZP counter for 512 Block CRC bytes
BlockTwo          = $EF       ; Counting to 256 twice






  .ORG ProgramStart;$300; Page align this program in RAM/ROM.
.reset:
  jmp .BootUp ;Returns to FrameLoop below. Keeps program load and start address the same.

.Array1: ; Putting this here page aligns below for fast code without using zero page.
    .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63,63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21,42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
.Array2: ; Keeps it the same 4 cycles as zero page (Array1),x 
    .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63,0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21,42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
.Array3: ; This is 192 bytes of all possible 3 Greyscale values.
    .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0,21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21,42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63


;****************************************************************************
;************************* MAIN DECODE LOOP ********************************* 
;****************************************************************************

.FrameLoop:
  dec Block_Counter ; must count 512 bytes
  beq .BLOCK        ; 256 roll-over go to BLOCK routine
.FrameLoopStart:     ; JMP here to start and for SD card BLOCK return

; Load Control Bit 1
  lda VIA_PORTA; Read bit 8 of byte from SD card. 
  bne .SkipRun ; If 1 Skip Pixels
; Load Control Bit 2  
  lda VIA_PORTA ; Else check bit 7
  beq .TriPixel ; If 0 TriPixel
  jmp .RLE      ; Else 1. New RLE self modifies, needs to be in RAM.

.TriPixel:      ; 3 pixel run
  lda VIA_PORTA ; Load 6 bits for up to 64 values for each array
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
    
  tax           ; Color/index to x
  lda .Array1,x ; Load 1 of 4 colors B/DG/LG/W for Bad Apple!
  sta (Screen),y; Draw it!
  iny           ; Next color
  lda .Array2,x ; Load 1 of 4 colors 
  sta (Screen),y; Draw it!
  iny           ; Next color
  lda .Array3,x ; Load 1 of 4 colors 
  sta (Screen),y; Draw it! 
  sta PlotColor ; Store last color used for Repeats
.TriDone:
  jmp .FrameLoop ; Decode another byte

.BLOCK:        ; Block_Counter rolled over -256 count
  inc BlockTwo ; We need to count to 512
  lda BlockTwo ; So we will do this twice
  cmp #2       ; 
  beq .TossBits; It's been 512 bytes, toss the bits
  jmp .FrameLoopStart; only 256, keep going

.SkipRun: ;Just add this amount to the screen pointer
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
  sta RLECount;Store because of adc of y below
  clc 
  tya 
  adc RLECount; Need to add RLE count to Y  
  tay         ; 
  lda ScreenH ; increment the top byte of the screen pointer
  adc #$00    ; 
  sta ScreenH ; Since we always encode skips on the edge of the screen we only need to check
  cmp #$40    ; for the screen roll-over at $4000 in the Skip routine.   
  beq .sRstTop; We never roll-over while in the draw routines becuse we are always on-screen. 
  jmp .FrameLoop;Decode next Byte
.sRstTop:
;****************************************************************************
;***************************** FRAME SYNC *********************************** 
;****************************************************************************
  lda #$20   ;New Frame starts at $2000   
  sta ScreenH;Reset screen pointer to first pixel
; Since we just rolled-over the screen buffer that means that we are done with the frame.
; Do Vsync routine below. 

; .Vsync: ;Org Vsync routine without Debug.
;   ldx VGAClock ;IRQ does a DEC on Vsync
;   cpx #250     ;Using this as 'zero' sync
;   bcs .EGVsync ; = > 250 wait or sync
; .Synced:; Less than 250 no wait. Try to catch up.
;   inc VGAClock ;Add two for 30 Frames a Second
;   inc VGAClock ;VGA is 60 Frames a Second 
;   jsr .GetBeep ;Put two notes per frame in music buffer
;   jsr .GetBeep ;60 vsync a sec on VGA with 30 FPS so 2 notes
;   jmp .FrameLoop;** Start next frame decode **
; .EGVsync: ; = > 250 It is a wait or a sync
;   beq .Synced  ; = 250 ;Good Vsync. Done waiting.
;   jmp .Vsync   ; > 250 ;Wait for Vsync

.Vsync: ; Debug version. I use assembler ' if ' statments here for 
        ; debug with no Vsync and a  'slow' pixel
 .if VsyncOn=1
    ldx VGAClock ;IRQ does a DEC on Vsync
    cpx #250     ;Using this as 'zero' sync
    bcs .EGVsync ; = > 250 wait or sync   
    .if Debug=1
     LDA #$20 ; Red
     STA $2002; Top Left
     JMP .NextSync; Less than 250 no wait. Try to catch up.
    .endif
.Synced:
    .if Debug=1
     LDA #$0  ; Black
     STA $2002; Top Left
    .endif
.NextSync
    inc VGAClock ;Add two for 30 Frames a Second
    inc VGAClock ;VGA is 60 Frames a Second 
    ;This only runs once per frame, so I won't unroll yet.
    .if BeepOn=1
        jsr .GetBeep ;Put two notes per frame in music buffer
        jsr .GetBeep ;60 vsync a sec on VGA with 30 FPS so 2 notes
    .endif
    jmp .FrameLoop;** Start next frame decode **
.EGVsync: ; = > 250 It is a wait or a sync
    beq .Synced  ; = 250 ;Good Vsync. Done waiting.
    jmp .Vsync   ; > 250 ;Wait for Vsync 
  .endif; if VsyncOn=1
  .if VsyncOn=0
   jmp .FrameLoop;** Start next frame decode **
  .endif

.TossBits:;320 CYCLES This routine is hard to place in a spot I can easily branch to 
  stz BlockTwo  ;     Reset top Byte Counter, we know bottom rolled over to 0
; Must throw away 10 bytes every block read from the SD card. No choice. 
  bit VIA_PORTA 
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA

  ;Read a Byte
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
  bit VIA_PORTA
; Done tossing bytes
  jmp .FrameLoopStart ; Back to decoding


 .if BeepOn=1 ; Debug option to turn off sound
.GetBeep: ;Beep is only run twice per frame, no real need to optimize/unroll, but I could clean it up some...
  PHY     
  PHP
  LDY #0
  LDA (BeepFile),y
  JSR .Beep
  CLC  
  LDA BeepFile
  ADC #$01
  STA BeepFile
; inc memory high
; Use carry from prior
  LDA BeepFileH
  ADC #$00
  STA BeepFileH
  CMP #$D3
  BEQ .BeepFileRstTop
  BRA .BeepDone
.BeepFileRstTop
  LDA #$A0
  STA BeepFileH	
.BeepDone:
  PLP
  PLY
  rts

.Beep:
    ; Actual Beep moved to system IRQ. Right now I will assume that
    ; the decoder bootup will send some number of Audio packets before 
    ; starting to show frames. This will fill the buffer.
      PHA ;?? Do I really need this?
      PHX
      
      LDX BeepWrite
      STA BeepBuffer,x
      
      INC BeepWrite  

      ;Last thing
      INC BeepCount
      ;BEQ .StartBeep ;Full Buffer
      
.StartBeep
      LDA #1
      STA BeepEnable
.BeepExit
      PLX
      PLA
      RTS
 .endif ;If BeepOn

;****************************************************************************
;************************** ZP RELOCATED CODE *******************************
;****************************************************************************
RelocateLength=40       ; Try 6 extra bytes?
.RelocateCode:          ; 34 byte function in ZP.
 .rorg RelocateLocation ; ZeroPage location
.RLE:                   ; Run Length Draw Function
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

  tax ; RLE count
  LDA .RLEArray,x ; Load JMP location
  STA .RLEJump+1  ; Store low byte location
  lda PlotColor   ; Last pixel color used
.RLEJump:         ; ZP Self modify!  
  JMP .RLERender  ; Jump to count in RLE routine.
.RLEend: ;Pad after relocate code for testing.
 .byte 1,2,3,4,5,6,7,8,9,0,1,1,1,1,2,2,2,2,3,3,3;Just some padding
 .rend ;End Relocation
;****************************************************************************

  .ORG RLERenderStart;RLERender must start on page!
.RLERender: ;This is around 8.2 cycles a pixel vrs 13.3 for a loop
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
 jmp .FrameLoop ; Decode another byte
  
  .ORG RLEArrayStart;Page align
 ; I could Bit shift and ADC then subtract from the length of hte routine
 ; instead of using this table, but that would take more cycles than this.
.RLEArray: ;Decending Array of 3 byte skips. I don't think I actually use zero? 
; I should for ' 1 ' but just dup first entry for now.
    .BYTE 189,189,186,183,180,177,174,171,168,165,162
        .BYTE 159,156,153,150,147,144,141,138,135,132
        .BYTE 129,126,123,120,117,114,111,108,105,102
        .BYTE  99, 96, 93, 90, 87, 84, 81, 78, 75, 72
        .BYTE  69, 66, 63, 60, 57, 54, 51, 48, 45, 42
        .BYTE  39, 36, 33, 30, 27, 24, 21, 18, 15, 12
    .BYTE   9,  6,  3,  0,  0 ;extra zero just in case.


;****************************************************************************
;********************************* BOOT *************************************
;****************************************************************************

.BootUp ;SD card and system boot routines
        ;These only run once at startup and are not optimized.

;Need to copy the RLE self modify to ram
 ldx #0
.RelocateLoop ;routine to move code
 LDA .RelocateCode,x
 STA RelocateLocation,x ;$0,x
 inx
 cpx #RelocateLength
 bne .RelocateLoop


; Some SD card setup:
; Need to read 512 bytes. Then throw away 10 CRC bytes
    stz Block_Counter; So a 16 bit counter it is..
    stz BlockTwo     ; Upper byte of counter.
    stz VIA_PORTA
    lda #254;??? #$ff-SD_BIT_MISO 
    sta VIA_DDRA
    LDA #$00
    
    STA Screen ; Ben Eater's Worlds Worst Video card 
    LDA #$20   ; uses the upper 8Kb of system RAM
    STA ScreenH; This starts at location $2000
    
 if BeepOn=1
; Setup some commands for music    
    STZ BeepRead
    STZ BeepWrite
    STZ BeepCount
    STZ BeepEnable
      ;Beep Rom data $A000
    LDA #$00
    STA BeepFile
    LDA #$A0
    STA BeepFileH
;Add some empty audio to beginning?
; ;This is to cover the SD card boot time and such.
;     LDX #5
;     lda #0
; .FillBufferNoBeep
;     ;JSR .GetBeep
;     jsr .Beep
;     DEX 
;     BNE .FillBufferNoBeep
.FillBuffer:
    LDX #250 
.FillBufferLoop:;Fill up NMI Music buffer with 250 bytes
    JSR .GetBeep
    DEX 
    BNE .FillBufferLoop
    STZ BeepRead ;Reset read to start of buffer
 endif ; if BeepOn

    ldy #0  
    ldx #0
    LDA #255
    STA  VIA_DDRB;MAKE EVERYTHING OUTPUT! $6002   
    LDA  VIA_AUX;$600b This would be a retangle wave instead of a square wave, but no real matter.    
                        ; Set the two high bits in the ACR to get the
    ORA  #$C0   ;1100-0000; square-wave/RECTANGLE output on PB7.  (Don't enable the
    STA  VIA_AUX;$600b    ; T1 interrupt in the IER though.)  
    
    lda #$0a ; Make CA2 pulse each time port A is read. tnx gfoot!
    STA VIA_PCR

    ;Do screwy stuff to get SD card
    ;started up in SPI mode.
    jsr sd_init

    ; Setup SD card to read forever
    lda #SD_MOSI
    sta  VIA_PORTB 
    lda #$52 ; CMD18 - READ_MULTI_BLOCK
    ;WOW, that easy, change $51 to $52 and I can stream
    ;bytes off the SD card forrever! Yea!
    ;Just have to remember to throw away the 10 byte
    ;CRC every 512 bytes. Other than that the bits stream forever. Yea!
    jsr sd_writebyte
    lda #$00           ; sector 24:31
    jsr sd_writebyte
    lda #$00           ; sector 16:23
    jsr sd_writebyte
    lda #$00           ; sector 8:15
    jsr sd_writebyte
    lda #$00           ; sector 0:7
    jsr sd_writebyte
    lda #RLECount           ; crc (not checked)
    jsr sd_writebyte

    jsr sd_waitresult
    cmp #$00
    beq .readsuccess
    jmp .reset ;This change makes it pretty reliable after a reboot.

.readsuccess
  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  beq .SdBooted
  ;Retry until it works...
  jmp .reset
.SdBooted
  ;SD card booted, setup some values


  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  ; Port A is in Clock pulse mode.
  ; A read will pulse the clock. 
  ; We will stream forever after that.
    

;****************************************************************************
;******************************** DECODE ************************************
;**************************************************************************** 

  lda VIA_PORTA ; toggle the clock once at the start to prime the SD card
  ;     -WE ARE STREAMING BITS FROM THE SD CARD NOW IN PORT A PA0-

  lda #252    ; 250 is our Vsync 'Zero' and we want 2 Vsync per frame (30fps)
  sta VGAClock; Start Vsync just before starting decode

;********************* Jump to video stream decode now! *********************
  jmp .FrameLoopStart ;Decoder entry point. Toss some frames on that screen!

;****************************************************************************


;****************************************************************************
;******************************* SD INIT ************************************
;**************************************************************************** 

sd_init:
;  Thanks George Foot! https://github.com/gfoot/sdcard6502 and for all your help on
;  6502.org forums and https://www.reddit.com/r/beneater/ !!
;
  ; Let the SD card boot up, by pumping the clock with SD CS disabled
  ; We need to apply around 80 clock pulses with CS and MOSI high.
  ; Normally MOSI doesn't matter when CS is high, but the card is
  ; not yet is SPI mode, and in this non-SPI state it does care.
;
; Since the SD boot only runs at the start there is no real need to optimize this if it works.
; I use Kingston Canvas Select Plus 32GB micro SD cards.
; 2 pack with SD adapter for $8 at normal online places.
; This code does a good job of booting them up. I've used 3 so far without issue.
  lda #SD_CS | SD_MOSI
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
.preinitloop:
  ;eor #SD_SCK
  sta  VIA_PORTB
  ldy VIA_PORTA 
  dex
  bne .preinitloop
  ;LDA #'P'
  ;STA ACIA
.cmd0 ; GO_IDLE_STATE - resets card to idle state, and SPI mode
  lda #<cmd0_bytes
  sta zp_sd_cmd_address
  lda #>cmd0_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand
  sta VIA
  ; Expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;.initfailed

.cmd8 ; SEND_IF_COND - tell the card how we want it to operate (3.3V, etc)
  lda #<cmd8_bytes
  sta zp_sd_cmd_address
  lda #>cmd8_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;.initfailed

  ; Read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte

.cmd55 ; APP_CMD - required prefix for ACMD commands
  lda #<cmd55_bytes
  sta zp_sd_cmd_address
  lda #>cmd55_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;.initfailed

.cmd41 ; APP_SEND_OP_COND - send operating conditions, initialize card
  lda #<cmd41_bytes
  sta zp_sd_cmd_address
  lda #>cmd41_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Status response $00 means initialised
  cmp #$00
  beq .initialized

  ; Otherwise expect status response $01 (not initialized)
  cmp #$01
  bne sd_init;.initfailed
  ; Not initialized yet, so wait a while then try again.
  ; This retry is important, to give the card time to initialize.
  ;jsr .delay.. NOT THAT IMPORTANT? Works with just a nop.. removed delay
  nop ;nop instead of a delay
  bra .cmd55

.initialized
  rts

cmd0_bytes
  .byte $40, $00, $00, $00, $00, $95
cmd8_bytes
  .byte $48, $00, $00, $01, $aa, $87
cmd55_bytes
  .byte $77, $00, $00, $00, $00, $01
cmd41_bytes
  .byte $69, $40, $00, $00, $00, $01



sd_readbyte:
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
    ora VIA_IORA;VIA_PORTA ; read last bit without causing a clock pulse. Cool!
    rts

sd_writebyte:
  ; Tick the clock 8 times with descending bits on MOSI
  ; SD communication is mostly half-duplex so we ignore anything it sends back here
  ldx #8                      ; send 8 bits
.swloop:
  asl                         ; shift next bit into carry
  tay                         ; save remaining bits for later
  lda #0
  bcc .sendbit                ; if carry clear, don't set MOSI for this bit
  ora #SD_MOSI
.sendbit:
  sta VIA_PORTB               ; set MOSI (or not) first with SCK low
  ;eor #SD_SCK                ; Clock not needed anymore
  sta VIA_PORTB               ; raise SCK keeping MOSI the same, to send the bit
  bit VIA_PORTA               ; * New clock for handshake *
  tya                         ; restore remaining bits to send
  dex
  bne .swloop                   ; loop if there are more bits to send
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

; Turns out I just don't need any delays? 
; The SD boot works without delay.. :) 
; so I'll just comment out for now?
;.delay:
;   ldx #0
;   ldy #0
; .dloop
;   dey
;   bne .dloop
;   dex
;   bne .dloop
;   rts

