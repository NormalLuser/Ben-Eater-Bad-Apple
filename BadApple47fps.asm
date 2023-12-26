;Fifty1Ford/NormalLuser Ben Eater Breadboard 6502 Bad Apple! Demo
;Debug Meter, Beep routine, Tri Pixel, Skip, Diff, RLE.

;SD CARD ROUTINE BASED ON GFOOT SD CARD 6502 GUIDE

;Disconnect LCD and clock system to 5 mhz. (use first counter output of VGA for 5mhz clock)
;Run in both hsync and vsync (1 pixels on left side will have a noise line on top at 5mhz)
;I disconnect the little LCD while working with this.
;SD Card input is PA0 MISO
;to make the serial code FAST.
;I also have pulse mode on reads
;USE CA2 as clock to save more cycles!!!
;IE FAST..
;Others are pins on VIA Port B 
;Use what you want for pins on B but
;Leave Port A empty except for MISO.


;This version will use a lookup dictionary
;so we can alway do 3 pixels with 2 bytes min.
;if the last byte can be a RLE that will still work.


PORTB = $6000
PORTA = $6000
DDRB = $6002
DDRA = $6002
ACIA         = $5000 ;

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
;SD_MISO = $80 ;%10000000 ;Moved to other port top bit
SD_MISO = $1 ;%00000001 ;Moved to other port BOTTOM bit



PORTA_OUTPUTPINS = E | RW | SD_CS | SD_SCK | SD_MOSI
;PORTA_OUTPUTPINS = $FE ;E | RW | SD_CS | SD_SCK | SD_MOSI
zp_sd_cmd_address = $40


;Vsync Beep for Bad Apple!
BeepBuffer   = $1E00;$1800 ;$1F00 ;$1A ;$0200  ;
Display      = $2000     ;Start of memory mapped display. 100x64 mapped to 128x64

;Break zeropage for a sec, dont' want to deal..

;A wire connecting the VGA Vsync signal to the NMI is used for
;BUFV command for a Vsync Buffer switch. It can also be used
;for a 1/60th of a second timer or with user input a random seed.

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
; OldPixY           = $E7       ; MOVE Old Pixel Location y offset
; OldPixC           = $E8       ; MOVE Old Color
; OldPixL           = $E9       ; MOVE Old Pixel Location
; OldPixH           = $EA       ; MOVE memory address/ROW
BackColor         = $EB       ; Background/Color command color

PlotColor         = $EC       ; Color for plot function 
Screen            = $ED       ; GFX screen location
ScreenH           = $EE       ; to draw TO
VidCounter        = $EF





 ; .org $1C00
 ; .ORG $1800 ;Start Ben Eater EhBasic with 6,000 start memory.
;  .ORG $1400 ;Start Ben Eater EhBasic with 5,000 start memory.
  ;This leaves room to load the program. $1800 is easy to remember and type.

  .ORG $300;$1B00 ;Start Ben Eater EhBasic with 5,000 start memory.
.reset:
  STZ BeepRead
  STZ BeepWrite
  STZ BeepCount

  LDA #$00
  STA Screen
  LDA #$20
  STA ScreenH

  JMP .BootSD 
  ldy #0  
  ldx #0

.readgotdata
  ; Need to read 512 bytes. Then throw away 10 CRC bytes
  ; Read two at a time, 256 times.
  lda #0
  sta Block_Counter;$E0 ; counter HERE IT IS!!!
  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta  VIA_PORTB 
  ; Port A is in Clock pulse mode.
  ; A read will pulse the clock. 
  ; We will stream forever after that.
  lda VIA_PORTA ; toggle the clock once at the start to prime
  ;-WE ARE STREAMING BITS NOW IN PORT A-
  jmp .readloop

 ;.ORG $400 ;Think this page aligns below for fast code without zero page?
.Array1: ; I think putting this here keeps it in the same page as the calling function?
    .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63,63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21,42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
.Array2: ; If so that keeps it the same 4 cycles as zero page (Array1),x 
    .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63,0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21,42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
.Array3: ; I'm not sure though... this is 192 bytes, my function needs to be within 64 bytes??
    .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0,21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21,42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63
        ;  Maybe I should put it in zero page?
;moved this so that it takes 4 cycles for lookup I think? Otherwise it 
;spans a page??
.TriPixel: ;Move above for no color, below tridone for color
;ok, lame, but just hack the beep in here for now?
;only costs a few seconds in playtime.. but can be better.
; check for beep on skips instead?
    CMP #255
    BEQ .GotoBeep   
    
    TAX ;color/index to x
    LDA .Array1-65,x
    sta (Screen),y
    INY
    LDA .Array2-65,x
    sta (Screen),y
    INY
    LDA .Array3-65,x
   ; sta (Screen),y
    STA PlotColor
.TriDone:
  LDX RLECount
  ;LDA PlotColor

.RLETop:
  sta (Screen),y
  INY
  DEX
  BNE .RLETop

.RLEDone
  dec Block_Counter
  BEQ .BLOCK
  JMP .readloop 

.GotoBeep:
    jmp .Beep

 ;$04f0 We are in a page!
.readloop:
 ;if vsync true, then...  Thinking about buffer....

 ;Read 2 Bytes: UNROLL!!!
  ;The unroll here added 7 frames a second on top of 30!
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
    ora VIA_PORTA

  ;jsr sd_readbyte
 sta RLECount
  ;Another Unroll
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
    ora VIA_PORTA
 
 sta PlotColor
 
;OK, now I need to see if it is a 'skip'
;Above  LDA PlotColor ;NEED THIS
  CMP #64
  ;BEQ .SkipRun ;it is 64, want to skip these. 
  BNE .TriPixel
  ;BCS .TriPixel ;BCS GREATER THAN 64
  ; No Color for you! Remmed above and moved below to remove color
  ; and save the BCS cycles

.SkipRun:
  clc  
  TYA
  adc RLECount
  TAY
  lda ScreenH
  adc #$00
  sta ScreenH
  CMP #$40
  BEQ .sRstTop
  
  dec Block_Counter
  BEQ .BLOCK
  JMP .readloop 
.sRstTop:
  LDA #$20
  STA ScreenH
  
  dec Block_Counter
  BEQ .BLOCK
  JMP .readloop 


.BLOCK: ; 644 CYCLES
  ;must throw away 10 bytes every block.
;OK, JUST UNROLL, NEED TO FIGURE OUT MACROS SO IT DOES NOT LOOK LIKE THIS
;Regardless, this is the fastest way I could think of to throw away 10 bytes
;It is worth FRAMES.. As in more than 1 Frame a second to do this silly stuff!
 ;->UnrollBelow
 lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA

  ;Read a Byte
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
  lda VIA_PORTA
;Done tossing bytes


; ;Debug bar
    ;   LDA #48
    ;   STA $2063
    ;   STA $2062
    ;   STA $2061
    ;   STA $2060
    ;   LDA #0
    ;   STA $20E3
    ;   STA $20E2
    ;   STA $20E1
    ;   STA $20E0
    ;   STA $2163
    ;   STA $2162
    ;   STA $2161
    ;   STA $2160

;HOW ABOUT WE LOOP FOREVER?
  JMP .readloop 
  ;Yep, it will happily stream garbage off the SD card forever.

.VgaWait:
;  ;debug
;  bra .BeepExit

 LDA VGAClock
 STA LastClock
 ;load a couple of bytes?
;  ;Debug bar
    ;   LDA #60
    ;   STA $20E3
    ;   STA $20E2
    ;   STA $20E1
    ;   STA $20E0
    ;   STA $20DF
    ;   LDA #0
    ;   STA $2063
    ;   STA $2062
    ;   STA $2061
    ;   STA $2060
.VgaStillWaiting:
 LDA VGAClock
 CMP LastClock
 BEQ .VgaStillWaiting
 ;rts
 bra .BeepExit

.VgaWait2:
;  ;debug
;  bra .BeepExit

 LDA VGAClock
 STA LastClock
;  ;Debug bar
    ;  LDA #4
    ;  STA $2163
    ;  STA $2162
    ;  STA $2161
    ;  STA $2160
    ;  STA $215F
    ;  LDA #0
    ;  STA $2063
    ;  STA $2062
    ;  STA $2061
    ;  STA $2060
 ;load a couple of bytes?
.VgaStillWaiting2:
 LDA VGAClock
 CMP LastClock
 BEQ .VgaStillWaiting2

 LDA VGAClock
 STA LastClock
 ;load a couple of bytes?
.VgaStillWaiting3:
 LDA VGAClock
 CMP LastClock
 BEQ .VgaStillWaiting3
 ;Debug
    ;  LDA #0
    ;  STA $2063
    ;  STA $2062
 bra .BeepExit

.BeepExit:     ;RTS
    dec Block_Counter;$00 ; counter 
    BEQ .MyBLOCK
    JMP .readloop 
 .MyBLOCK:
    jmp .BLOCK

.Beep:
    ;Actual Beep moved to system IRQ.
    ;Right now I will assume that the encoder will send 
    ;some number of Audio packets before starting to send frames.
    ;Yea' forget that nonsense....
    ;Just store the music in ROM and setup a IRQ routine
    ;to send the beeps. Stream video only for now?
    ;Same with V-sync. Will move to self v-sync
    ;on screen roll-over.
    ;Leave in place for now.

      LDA RLECount
      ;BEQ .BEEP_Off    ;if note 0 turn off beep; 
      CMP #1           ;Yes, putting the v-sync in the beep command.
      BEQ .VgaWait     ;Both meant to be run on frames.
      ;LAME, FIX THIS
      CMP #2           ;Yes, putting the v-sync in the beep command.
      BEQ .VgaWait2     ;Both meant to be run on frames at least
                        ;Maybe I should call it a Frame command?  
      LDX BeepWrite
      STA BeepBuffer,x
      ;LDA BeepWrite
      ;CPY #99
      ;BEQ .ResetBuffer
      INC BeepWrite  

      ;Last thing
      INC BeepCount
      LDA #1
      STA BeepEnable
      ;JMP .readloop 
      BRA .BeepExit
; .ResetBuffer:
;     STZ BeepWrite
;     ;JMP .readloop
;     BRA .BeepExit


.BootSD
 ;Not sure why I do this twice...? 
    stz VIA_PORTA
    lda #254;??? #$ff-SD_BIT_MISO 
    sta VIA_DDRA
    stz VIA_PORTA
    lda #254;??? #$ff-SD_BIT_MISO 
    sta VIA_DDRA


      ;LDA  VIA_DDRB;$6002
      ;ORA  #$80    ;%1000-0000
      LDA #255
      STA  VIA_DDRB;;MAKE EVERYTHING OUTPUT! $6002   ; Set the high bit in DDRB, to make PB7 an output.
      LDA  VIA_AUX;$600b
      ;Using only 1100-0000 instead of 1111-0000
      ;because it is a lower duty cycle and sounds fine.
      ;This would be a retangle wave instead of a square wave, but no real matter.
      ;This lower duty cycle is better if you have a speaker hooked 
      ;right up to the VIA to keep it from drawing too much power.
          
                            ; Set the two high bits in the ACR to get the
      ORA  #$C0   ;1100-0000; square-wave/RECTANGLE output on PB7.  (Don't enable the
      STA  VIA_AUX;$600b    ; T1 interrupt in the IER though.)

      ;LDA  #255           ; Set the T1 timeout period. 
                          ;LOWER THIS TO SOMETHING LIKE 77 FOR 1MHZ CPU CLOCK 
      ;STA  VIA_T1CL;$6004;;USE 255 if the Φ2 rate is 5MHz.  To get it going, write to
                           ;VIA_T1CH     
  
  ;Make CA2 pulse each time port A is read
  ;lets see if I can use this as the SD clock??
  ;gfoot says it can be done. I still need to figure it out

  lda #$0a 
  STA VIA_PCR

;Do screwy stuff to get SD card
  ;started up in SPI mode.
  jsr sd_init

;Send out ACIA that it did init
;    LDA #'I'
;    STA ACIA
;    LDA #'!'
;    STA ACIA
; Read a sector

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

;This change makes it pretty reliable after a reboot.
  ;jmp loop 
  jmp .reset 
  
  LDX #$00 
  ldy #$00
  
.readsuccess
  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  beq .SdBooted
  ;Retry until it works...
  jmp .reset
.SdBooted
  jmp .readgotdata


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
  LDA #'P'
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
  ;STA ACIA
  rts

.initfailed
  lda #'X'
  ;STA ACIA
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
    ora VIA_IORA;VIA_PORTANH ; read last bit without causing a clock pulse. Cool.
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
  LDA VIA_PORTA ;clock for handshake
  tya                         ; restore remaining bits to send
  dex
  bne .swloop                   ; loop if there are more bits to send
  rts
  

sd_waitresult:
  ; Wait for the SD card to return something other than $ff
  ;LDA #'W'
  ;STA ACIA
  jsr sd_readbyte
 ; STA ACIA  
  cmp #$ff
  ;JSR delay
  beq sd_waitresult
  rts

sd_sendcommand:
  ; Debug print which command is being executed
;    LDA #'S'
;    STA ACIA 
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


;Below is the lookup array for the 3 pixel compression.
;If the encoded byte is a color 0-63 it uses that
;to draw the current memory location. The next byte 
;is a repeat or Run Length Byte. 
;It will repeat that number of times.
;If the number is 1 it draws 1 pixel.
;If the encoded byte is value 64 it skips.
;so instead it reads the Run Length and adds that
;value to the memory pointer.
;If the value is 65 through 129 it uses the 3 lookup
;array's below to get a 3 pixel value.
;For Bad Apple! the table below is all possible 3 pixel
;values of Black/White/Light Grey/Dark Grey that are 
;used in this demo. It then uses the last pixel for Run Length.
; Tried to see what moving this to zero page did.. nothing..
; It seems that LDA #ZP,x and LDA #$FFFF,x are both 4 cycles
; as long as you don't jump page boundry.
 ;.org $1800 ;make sure it is page aligned..
; .org $700;$1F00 ;make sure it is page aligned..
; Array1:
;     .byte 0,0,0,21,0,0,42,0,0,63,0,21,21,0,0,21,21,42,42,0,0,21,21,63,63,0,42,42,0,0,42,42,63,63,0,63,63,21,21,21,42,21,21,63,21,42,42,21,21,42,42,63,63,21,63,63,42,42,42,63,42,63,63,63
; Array2:
;     .byte 0,0,21,0,0,42,0,0,63,0,21,0,21,21,42,0,42,0,21,21,63,0,63,0,21,42,0,42,42,63,0,63,0,42,63,0,63,21,21,42,21,21,63,21,42,21,42,42,63,21,63,21,42,63,21,63,42,42,63,42,63,42,63,63
; Array3:
;     .byte 0,21,0,0,42,0,0,63,0,0,21,21,0,42,21,42,0,21,0,63,21,63,0,21,0,42,42,0,63,42,63,0,42,0,63,63,0,21,42,21,21,63,21,21,42,42,21,63,42,63,21,42,21,63,63,21,42,63,42,42,63,63,42,63


