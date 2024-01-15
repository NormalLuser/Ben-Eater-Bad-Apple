;Fifty1Ford/NormalLuser Ben Eater Breadboard 6502 Bad Apple! Demo
;Debug Meter, Beep routine, Tri Pixel, Skip, Diff, RLE.


VIA              = $6000
VIA_PORTB        = VIA
VIA_PORTA        = VIA+1;$6001
VIA_DDRB         = VIA+2;$6002
VIA_DDRA         = VIA+3;$6003
VIA_AUX          = VIA+11;$600B ;Set to 0 to stop BEEP ALSO ACR
VIA_PCR          = VIA+12;$600C
VIA_IORA         = VIA+15;$600F
E  = $80;%10000000 ;
RW = $40;%01000000
SD_CS   = $20 ;%00100000
SD_MOSI = $8  ;%00001000
; SD_SCK  = $10 ;%00010000;Clock moved to CA2
zp_sd_cmd_address = $40
; ;Vsync Beep for Bad Apple!
BeepBuffer   = $1E00;$1800 ;$1F00 ;$1A ;$0200  ;
; Display      = $2000     ;Start of memory mapped display. 100x64 mapped to 128x64
BeepEnable        = $DC  ;** don't move without changing IRQ code
BeepCount         = $DD  ;ZP
BeepRead          = $DE  ;ZP
BeepWrite         = $DF  ;ZP
Block_Counter     = $E0  ;ZP counter for 512 (2 bytes/2) Block CRC bytes
VGASync           = $E1;***** IF NMI hooked up to vsync this will be 1 after sync(s), you set to zero yourself *****
VGAClock          = $E2;***** IF NMI hooked up to vsync this will DEC *****
RLECount          = $E3  
ReadByteTemp      = $E4
BeepFile          = $E5       ; Location of audio in memory
BeepFileH         = $E6       ;
LastClock         = $E7       ;ZP
PlotColor         = $EC       ; Color for plot function 
Screen            = $ED       ; GFX screen location
ScreenH           = $EE       ; to draw TO

BlockTwo = $02

  .ORG $300; Page align
.reset:
  JMP .BootUp ;Returns below readloop:
;  JMP .readloop
.Array1: ; putting this here page aligns below for fast code without using zero page.
    .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63,63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21,42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
.Array2: ; Keeps it the same 4 cycles as zero page (Array1),x 
    .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63,0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21,42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
.Array3: ; This is 192 bytes
    .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0,21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21,42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63
        
.TriPixel: ;Up here so I don't need a JMP 
    TAX ;color/index to x
    LDA .Array1,x ;Load 1 of 4 colors
    sta (Screen),y
    INY
    LDA .Array2,x
    sta (Screen),y
    INY
    LDA .Array3,x
   STA (Screen),y ;Skip this store, RLE below will get it
    STA PlotColor ;unused PlotColor. Perserve A and Y
.TriDone:
  JMP .readloop 

.Repeate:
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

   TAX 
  ;LDX RLECount
  LDA PlotColor
.RLETop:    
  STA (Screen),y
  INY
  DEX
  BNE .RLETop

.RLEDone
  JMP .readloop 


.BLOCK:;jmp to the SD Block routine.
  jmp .TheBLOCK

;*** MAIN DECODE LOOP ***
.readloop:
  dec Block_Counter
  BEQ .BLOCK
.readloopStart:
 ;Read 2 Bytes:


;Control byte
  lda VIA_PORTA 
  BNE .SkipRun ;If 1 skip 
  
  lda VIA_PORTA 
  BNE .Repeate ;If Repeate
  ; Else TriPixel
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
    
 ;sta PlotColor ;A register kept, PlotColor never loaded.

  jmp .TriPixel

.SkipRun: ;Just add this amount to the screen pointer
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
    sta RLECount

  clc  
  TYA
  adc RLECount
  TAY
  lda ScreenH
  adc #$00
  sta ScreenH ;Since we always encode skips on the edge of the screen we only need to check
  CMP #$40    ;for the screen roll-over at $4000 in the Skip routine.   
  BEQ .sRstTop;We never roll-over while in the draw routines becuse we are always on-screen.
  BRA .readloop 

.sRstTop:
  LDA #$20   ;New Frame starts at $2000   
  STA ScreenH;Reset screen pointer to first pixel

.Vsync: 
  LDX VGAClock
  CPX #250 
  BCS .EGVsync ; = > 250
; Less than 250 no wait. Try to catch up.
  ;INC VGAClock ;Extra clock to help with sync?
.Synced:
; this is 10 cycles
  INC VGAClock ;Add two for 30 Frames a Second
  INC VGAClock ;VGA is 60 FPS 
  JMP .readloop;** Start next frame **

.EGVsync: ; = > 250
  BEQ .Synced  ; = 250 ;Good Vsync. Done waiting.
  BRA .Vsync   ; > 250 ;Wait for Vsync



.TheBLOCK: ; 320 CYCLES
  inc BlockTwo
  lda BlockTwo
  cmp #2
  beq .TossBits
  JMP .readloopStart

.TossBits:
 stz BlockTwo
;Must throw away 10 bytes every block read from the SD card. No choice.
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
;Done tossing bytes
 ; JMP .BlockReturn ;Back to decoding
  JMP .readloopStart



;****************************************************************************
;****************************************************************************
;****************************************************************************
.BootUp ;SD card and system boot routines
    stz BlockTwo
    stz VIA_PORTA
    lda #254;??? #$ff-SD_BIT_MISO 
    sta VIA_DDRA

    LDA #255
    STA  VIA_DDRB;;MAKE EVERYTHING OUTPUT! $6002   ; Set the high bit in DDRB, to make PB7 an output.
    LDA  VIA_AUX;$600b This would be a retangle wave instead of a square wave, but no real matter.    
                        ; Set the two high bits in the ACR to get the
    ORA  #$C0   ;1100-0000; square-wave/RECTANGLE output on PB7.  (Don't enable the
    STA  VIA_AUX;$600b    ; T1 interrupt in the IER though.)  
    ;Make CA2 pulse each time port A is read tnx gfoot.
    lda #$0a 
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
    ;CRC every 512 bytes. Otherwise the bits stream forever.
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
    ;This change makes it pretty reliable after a reboot.
    jmp .reset 

.readsuccess
  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  beq .SdBooted
  ;Retry until it works...
  jmp .reset
.SdBooted
  ;SD card booted, setup some values
  STZ BeepRead
  STZ BeepWrite
  STZ BeepCount

  LDA #$00
  STA Screen
  LDA #$20
  STA ScreenH
  ldy #0  
  ldx #0
.readgotdata
  ; Need to read 512 bytes. Then throw away 10 CRC bytes
  ; Read two at a time, 256 times.
  STZ Block_Counter;$E0 

  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  ; Port A is in Clock pulse mode.
  ; A read will pulse the clock. 
  ; We will stream forever after that.
    
  
  ;**************************
  lda VIA_PORTA ; toggle the clock once at the start to prime
  ;-WE ARE STREAMING BITS FROM THE SD CARD NOW IN PORT A PA0-
 
 ;Start Vsync
  LDA #252 
  STA VGAClock
 
  ;*** Jump to video stream decode now! ***
  ;JMP .readloop
  JMP .readloopStart
  ;***********************


sd_init:
  ; Let the SD card boot up, by pumping the clock with SD CS disabled
  ; We need to apply around 80 clock pulses with CS and MOSI high.
  ; Normally MOSI doesn't matter when CS is high, but the card is
  ; not yet is SPI mode, and in this non-SPI state it does care.
;The code below still mostly works with old setup as well.
;Since the SD boot only runs at the start there is no real need to optimize this if it works.
;I use Kingston Canvas Select Plus 32GB micro SD cards.
;2 pack with SD adapter for $8 at normal online places.
;This code does a good job of booting them up. I've used 3 so far without issue.
  lda #SD_CS | SD_MOSI
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
.preinitloop:
  ;eor #SD_SCK
  sta  VIA_PORTB
  LDY VIA_PORTA 
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
  STA VIA
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
  ;jsr delay.. NOT THAT IMPORTANT? Works with just a nop.. removed delay
  nop
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
    asl
    ora VIA_IORA;VIA_PORTANH ; read last bit without causing a clock pulse. Cool!
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
  sta VIA_PORTB                   ; set MOSI (or not) first with SCK low
  ;eor #SD_SCK
  sta VIA_PORTB                   ; raise SCK keeping MOSI the same, to send the bit
  BIT VIA_PORTA ;clock for handshake
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

; delay
;   ldx #0
;   ldy #0
; .dloop
;   dey
;   bne .dloop
;   dex
;   bne .dloop
;   rts

