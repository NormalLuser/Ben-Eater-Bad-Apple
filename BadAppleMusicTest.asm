; Fifty1Ford/NormalLuser Ben Eater Breadboard 6502 Bad Apple! Demo
; Music Test
; ***************** NOTE *****************************************
;
; Boot ROM must have a NMI rouine with :
;  DEC $E2
;  Music routine needed in boot rom for music to play.
; ****************************************************************

ProgramStart    = $1500 ; Page align!
; RLERenderStart  = ProgramStart + 768  ; Page align!
; RLEArrayStart   = RLERenderStart + 256; Page align!
; RelocateLocation= $0 ;Zero Page Baby!


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

LastBeep=$99





  .ORG ProgramStart;$300; Page align this program in RAM/ROM.
.reset:
  jmp .BootUp ;Returns to FrameLoop below. Keeps program load and start address the same.

; .Array1: ; Putting this here page aligns below for fast code without using zero page.
;     .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63,63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21,42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
; .Array2: ; Keeps it the same 4 cycles as zero page (Array1),x 
;     .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63,0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21,42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
; .Array3: ; This is 192 bytes of all possible 3 Greyscale values.
;     .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0,21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21,42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63


;****************************************************************************
;************************* MAIN DECODE LOOP ********************************* 
;****************************************************************************

.FrameLoop:
 ; dec Block_Counter ; must count 512 bytes
 ; beq .BLOCK        ; 256 roll-over go to BLOCK routine
.FrameLoopStart:     ; JMP here to start and for SD card BLOCK return




;****************************************************************************
;***************************** FRAME SYNC *********************************** 
;****************************************************************************
;   lda #$20   ;New Frame starts at $2000   
;   sta ScreenH;Reset screen pointer to first pixel
; ; Since we just rolled-over the screen buffer that means that we are done with the frame.
; ; Do Vsync routine below. 

.Vsync: ;Org Vsync routine without Debug.
  
  ldx VGAClock ;IRQ does a DEC on Vsync
  cpx #250     ;Using this as 'zero' sync
  bcs .EGVsync ; = > 250 wait or sync
.Synced:; Less than 250 no wait. Try to catch up.
  inc VGAClock ;Add two for 30 Frames a Second
  ; inc VGAClock ;VGA is 60 Frames a Second 
  ldy #0
  lda LastBeep
  sta (BeepFile),y
  jsr .GetBeep ;Put two notes per frame in music buffer
  ;jsr .GetBeep ;60 vsync a sec on VGA with 30 FPS so 2 notes
  ldy #0
  lda (BeepFile),y
  sta LastBeep
  lda #48
  sta (BeepFile),y

  ; ldy #0
  ; lda LastBeep
  ; sta (BeepFile),y
  ; jsr .GetBeep ;Put two notes per frame in music buffer
  ; ;jsr .GetBeep ;60 vsync a sec on VGA with 30 FPS so 2 notes
  ; ldy #0
  ; lda (BeepFile),y
  ; sta LastBeep
  ; lda #48
  ; sta (BeepFile),y


  jmp .FrameLoop;** Start next frame decode **
.EGVsync: ; = > 250 It is a wait or a sync
  beq .Synced  ; = 250 ;Good Vsync. Done waiting.
  jmp .Vsync   ; > 250 ;Wait for Vsync


.GetBeep: ;Beep is only run twice per frame, no real need to optimize/unroll, but I could clean it up some...
  PHY     
  PHP
  ; LDY #0
  ; LDA (BeepFile),y
  ; STA LastBeep
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
  CMP #$40
  BEQ .BeepFileRstTop
  BRA .BeepDone
.BeepFileRstTop
  LDA #$20;#$A0
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
 
;****************************************************************************
;********************************* BOOT *************************************
;****************************************************************************

.BootUp ;SD card and system boot routines
        ;These only run once at startup and are not optimized.

; Setup some commands for music    
    STZ BeepRead
    STZ BeepWrite
    STZ BeepCount
    STZ BeepEnable
      ;Beep Rom data $A000
    LDA #$00
    STA BeepFile
    LDA #$20;#$A0
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
    LDX #4 ;#250 
.FillBufferLoop:;Fill up NMI Music buffer with 250 bytes
    JSR .GetBeep
    DEX 
    BNE .FillBufferLoop
    STZ BeepRead ;Reset read to start of buffer
 

    ldy #0  
    ldx #0
  

  lda #252    ; 250 is our Vsync 'Zero' and we want 2 Vsync per frame (30fps)
  sta VGAClock; Start Vsync just before starting decode

;********************* Jump to video stream decode now! *********************
  jmp .FrameLoopStart ;Decoder entry point. Toss some frames on that screen!

;****************************************************************************

