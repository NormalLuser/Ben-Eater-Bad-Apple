;BASED ON GFOOT SD CARD 6502 GUIDE

;Currently input is PA7 so I can use ROL 
;to make the serial code tighter.
;I also have pulse mode on, next step is to see if I can use 
;CA2 as clock to save more cycles??

;This version will use a lookup dictionary
;so we can alway do 3 pixels with 2 bytes min.
;if the last byte can be a RLE that will still work.


PORTB = $6000
PORTA = $6000
DDRB = $6002
DDRA = $6002


VIA              = $6000
VIA_PORTB        = VIA
VIA_PORTA        = VIA+1;$6001
VIA_DDRB         = VIA+2;$6002
VIA_DDRA         = VIA+3;$6003
VIA_T1CL         = VIA+4;$6004
VIA_T1CH         = VIA+5;$6005
VIA_T1LL         = VIA+6;$6006
VIA_T1LH         = VIA+7;$6007
VIA_T2CL         = VIA+8;$6008
VIA_T2CH         = VIA+9;$6009
VIA_SHIFT        = VIA+10;$600A
VIA_AUX          = VIA+11;$600B ;Set to 0 to stop BEEP ALSO ACR
VIA_PCR          = VIA+12;$600C
VIA_IFR          = VIA+13;$600F
VIA_IER          = VIA+14;$600E
VIA_IORA         = VIA+15;$600F



E  = $80;%10000000 ;
RW = $40;%01000000
;RS = %00100000

SD_CS   = $20 ;%00100000
SD_SCK  = $10 ;%00010000
SD_MOSI = $8  ;%00001000
;SD_MISO = $4 ;%00000100 
SD_MISO = $80 ;%10000000 ;Moved to other port top bit


PORTA_OUTPUTPINS = E | RW | SD_CS | SD_SCK | SD_MOSI

zp_sd_cmd_address = $40

Display           = $2000     ;Start of memory mapped display. 100x64 mapped to 128x64
Screen            = $ED       ; GFX screen location
ScreenH           = $EE       ; to draw TO



Color = $03
RLECount = $01
ReadByteTemp = $07

 ; .org $1C00
  .ORG $1800

.reset:

 
  LDA #$00
  STA Screen
  LDA #$20
  STA ScreenH
  lda #PORTA_OUTPUTPINS   ; Set various pins on port A to output
  sta DDRA

  ;Do screwy stuff to get SD card
  ;started up in SPI mode.
  jsr sd_init

  ;Make CA2 pulse each time port A is read
  ;lets see if I can use this as the SD clock??
  ;gfoot says it can be done. I still need to figure it out

  lda #$0a 
  STA VIA_PCR


  ; Read a sector
  lda #SD_MOSI
  sta  VIA_PORTB 

  ; Command 17, arg is sector number, crc not checked
  lda #$52 ; 51           ; CMD17 - READ_SINGLE_BLOCK TRY CMD18
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

  ;jmp loop
  jmp .reset
  LDX #$00 
  ldy #$00
  
.readsuccess
  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  beq .readgotdata

  ;jmp loop
  jmp .reset




.readgotdata
  ; Need to read 512 bytes.  Read two at a time, 256 times.
  lda #0
  sta $00 ; counter HERE IT IS!!!
.readloop:
 ;UNROLL

;READ THE TWO BYTES THAT MAKE UP A PACKET
.R1loop:;Read Byte
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 

  lda VIA_PORTA                   ; read next bit
  ROL
  ROL RLECount
 ;.R2loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 

  lda VIA_PORTA                   ; read next bit
  ROL
  ROL RLECount
 ;.R3loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 

  lda VIA_PORTA                   ; read next bit
  ROL
  ROL RLECount
 ;.R4loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 

  lda VIA_PORTA                   ; read next bit
  ROL
  ROL RLECount
 ;.R5loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 

  lda VIA_PORTA                   ; read next bit
  ROL
  ROL RLECount
 ;.R6loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 

  lda VIA_PORTA                   ; read next bit
  ROL
  ROL RLECount
 ;.R7loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 

  lda VIA_PORTA                   ; read next bit
  ROL
  ROL RLECount
 ;.R8loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 

  lda VIA_PORTA                   ; read next bit
  ROL
;StoreRLECount
  ROL RLECount
.RR1loop:;ReadByte
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 
  lda VIA_PORTA                   ; read next bit
  ROL
  ROL Color
 ;.RR2loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 
  lda VIA_PORTA                   ; read next bit
  ROL
  ROL Color
 ;.RR3loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 
  lda VIA_PORTA                   ; read next bit
  ROL
  ROL Color
 ;.RR4loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 
  lda VIA_PORTA                   ; read next bit
  ROL
  ROL Color
 ;.RR5loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 
  lda VIA_PORTA                   ; read next bit
  ROL
  ROL Color
 ;.RR6loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 
  lda VIA_PORTA                   ; read next bit
  ROL
  ROL Color
 ;.RR7loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 
  lda VIA_PORTA                   ; read next bit
  ROL
  ROL Color
 ;.RR8loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 
  lda VIA_PORTA                   ; read next bit
  ROL
;Store Color
  ROL Color

;OK, now I need to see if it is a 'skip'
  LDA Color
  CMP #64
  BEQ .SkipRun ;it is 64, want to skip these.
  BCS .TriPixel ;BCS GREATER THAN 64
.TriDone:
  LDX #$00 
  ldy #$00
  LDA Color

;Tried speeding up, not much improvment,
;and it messes up the top two lines for some reason.
;8:29 runtime vrs 8:40. Skip if for now, revisit later
;-> --Commented Code-- <-
    ; .RLETop:
    ; ; DEC RLECount
    ; ;  LDA Color
    ; LDY #1
    ; .RLELoop:
    ; STA (Screen),y
    ; CPY RLECount
    ; BEQ .RLEDone
    ; INY
    ; BRA .RLELoop	
    ; ; CPX RLECount
    ; ; BEQ .RLEDone
    ; ; BRA .RLETop
    
    ; .RLEDone:

    ; clc  
    ; LDA Screen
    ; adc RLECount
    ; sta Screen
    ; lda ScreenH
    ; adc #$00
    ; sta ScreenH
    ; CMP #$40
    ; BEQ .rleRstTop
    
    ; dec $00 ; counter
    ; BEQ .BLOCK
    ; JMP .readloop 
    ; .rleRstTop:
    ; LDA #$20
    ; STA ScreenH
    
    ; dec $00 ; counter
    ; BEQ .BLOCK
    ; JMP .readloop 

.RLETop:
  DEC RLECount
  LDA Color
  
  sta (Screen),y
  ;JSR $AE6A
  ;I don't like the screen inc code....
  ;seems like room for improvement?
  LDA Screen
  CMP #$FF
  BEQ .P1IncTop
  INC Screen
  BRA .P1DONE
.P1IncTop:
  INC ScreenH
  LDA #$00
  STA Screen
  LDA ScreenH
  CMP #$40
  BEQ .P1RstTop
  BRA .P1DONE
.P1RstTop:
  LDA #$20
  STA ScreenH
.P1DONE:
  CPX RLECount
  BEQ .RLEDone
  BRA .RLETop
.RLEDone:
  dec $00 ; counter ;HOW IS THIS WORKING? i DONT INIT $00? MUST BE EH BASIC?
  BEQ .BLOCK
  JMP .readloop 



.TriPixel:
    TAX ;color/index to x
    LDA Array1-65,x
    LDY #0
    sta (Screen),y
    INY
    LDA Array2-65,x
    sta (Screen),y
    INY
    LDA Array3-65,x
    sta (Screen),y
    STA Color
    ;------------
    clc  
    LDA Screen
    ;confuses me, why 2 seems to work?
    adc #2;tRY2? ;3 pixels+1 for next? ;RLECount
    sta Screen
    lda ScreenH
    adc #$00
    sta ScreenH
    CMP #$40
    BEQ .TRstTop
    JMP .TriDone
.TRstTop:
    LDA #$20
    STA ScreenH
    ;JMP .TriDone
    JMP .TriDone



.SkipRun:
  clc  
  LDA Screen
  adc RLECount
  sta Screen
  lda ScreenH
  adc #$00
  sta ScreenH
  CMP #$40
  BEQ .sRstTop
  
  dec $00 ; counter
  BEQ .BLOCK
  JMP .readloop 
.sRstTop:
  LDA #$20
  STA ScreenH
  
  dec $00 ; counter
  BEQ .BLOCK
  JMP .readloop 


.BLOCK: ; 644 CYCLES
  ;must throw away 10 bytes every block.
  ;HRM... I could speed this up with a macro .
  ;should figure out how to do that sometime....
  ;I'll just code it.
;  ldx #80                      ; we'll read 80 bits
 ; ldx #10                      ; we'll read 10 BYTES
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  ldy #SD_MOSI | SD_SCK       ; toggle the clock high
; loop:; 8 CLOCK CYCLES FOR 8 BITS
;OK, JUST UNROLL, NEED TO FIGURE OUT MACROS SO IT DOES NOT LOOK LIKE THIS
;Regardless, this is the fastest way I could think of to throw away 10 bytes by
;just using a and y to store what bits we need to cycle the clock.
; 644 CYCLES

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 

  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 
  sta  VIA_PORTB 
  sty  VIA_PORTB 



;Unrolled, comment below.
 ; dex                         ; decrement counter
 ; bne .loop                   ; loop if we need to read more bits

  ;rts

; readloopJMP:
  ;bra .readloop ;HOW ABOUT WE LOOP FOREVER?
  JMP .readloop ;HOW ABOUT WE LOOP FOREVER?



;   ; loop forever
; loop:
;      nop
;      RTS
;    jmp loop





sd_init:
  ; Let the SD card boot up, by pumping the clock with SD CS disabled

  ; We need to apply around 80 clock pulses with CS and MOSI high.
  ; Normally MOSI doesn't matter when CS is high, but the card is
  ; not yet is SPI mode, and in this non-SPI state it does care.

  lda #SD_CS | SD_MOSI
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
.preinitloop:
  eor #SD_SCK
  sta  VIA_PORTB 
  dex
  bne .preinitloop
  

.cmd0 ; GO_IDLE_STATE - resets card to idle state, and SPI mode
  lda #<cmd0_bytes
  sta zp_sd_cmd_address
  lda #>cmd0_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

.cmd8 ; SEND_IF_COND - tell the card how we want it to operate (3.3V, etc)
  lda #<cmd8_bytes
  sta zp_sd_cmd_address
  lda #>cmd8_bytes
  sta zp_sd_cmd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne .initfailed

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
  bne .initfailed

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
  bne .initfailed

  ; Not initialized yet, so wait a while then try again.
  ; This retry is important, to give the card time to initialize.
  jsr delay
  jmp .cmd55


.initialized
  lda #'Y'
 ; jsr print_char
  rts

.initfailed
  lda #'X'
  ;jsr print_char
; .loop
  jmp sd_init ;.loop


cmd0_bytes
  .byte $40, $00, $00, $00, $00, $95
cmd8_bytes
  .byte $48, $00, $00, $01, $aa, $87
cmd55_bytes
  .byte $77, $00, $00, $00, $00, $01
cmd41_bytes
  .byte $69, $40, $00, $00, $00, $01







sd_readbyte:
  ; Enable the card and tick the clock 8 times with MOSI high, 
  ; capturing bits from MISO and returning them
  ldx #8                      ; we'll read 8 bits
.loop:
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta  VIA_PORTB 
  lda VIA_PORTA                   ; read next bit
  
  ROL
  ROL ReadByteTemp
  
;   and #SD_MISO
;   clc                         ; default to clearing the bottom bit
;   beq .bitnotset              ; unless MISO was set
;   sec                         ; in which case get ready to set the bottom bit
; .bitnotset:
;   tya                         ; transfer partial result from Y
;   rol                         ; rotate carry bit into read result
;   tay                         ; save partial result back to Y
  
  dex                         ; decrement counter
  
  bne .loop                   ; loop if we need to read more bits
  lda ReadByteTemp
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
  eor #SD_SCK
  sta VIA_PORTB                   ; raise SCK keeping MOSI the same, to send the bit
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
  ; Debug print which command is being executed

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

delay
  ldx #0
  ldy #0
.dloop
  dey
  bne .dloop
  dex
  bne .dloop
  rts

Array1:
    .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63,63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21,42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
Array2:
    .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63,0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21,42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
Array3:
    .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0,21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21,42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63



