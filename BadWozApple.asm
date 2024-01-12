; NormalLuser/Fifty1Ford 
; WozMon edit for Bad Apple! Demo testing for 
; Ben Eater 6502 breadboard computer kit with
; Worlds Worst Video Card breadboard kit with
; ACIA Serial kit/addition.
; SD card pins: todo

 .org $A000
    .incbin "C:\Projects\6502\BadAppleONLYMusicROM.bin"


   .org $8000

ACIA        = $4000 
ACIA_CTRL   = ACIA+3
ACIA_CMD    = ACIA+2
ACIA_SR     = ACIA+1
ACIA_DAT    = ACIA

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


IN          = $0200          ;*Input buffer
XAML        = $24            ;*Index pointers
XAMH        = $25
STL         = $26
STH         = $27
L           = $28
H           = $29
YSAV        = $2A
MODE        = $2B
MSGL        = $2C
MSGH        = $2D
COUNTER     = $2E
CRC         = $2F
CRCCHECK    = $30


;Vsync Beep for Bad Apple!
BeepBuffer   = $1E00;$1800 ;$1F00 ;$1A ;$0200  ;
Display      = $2000     ;Start of memory mapped display. 100x64 mapped to 128x64

;Break zeropage for a sec, dont' want to deal..

;A wire connecting the VGA Vsync signal to the NMI is used for
;BUFV command for a Vsync Buffer switch. It can also be used
;for a 1/60th of a second timer or with user input a random seed.

;Below are chosen because they don't mess up EhBasic.
;Well... now that I commented out all the normal IRQ stuff anyway.
;This allows a Vsync IRQ driven Beep command. For Bad Apple!
;Whoops! EhBasic all kinds of screwy now.. I'll just use WozMon for now.
;Fix for EhBasic maybe someday?

BeepEnable        = $DC ;** don't move without changing IRQ code
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

; -START-

RESET
;EDIT
            STZ BeepEnable
            STZ VIA_AUX
;END EDIT
            CLD             ;Clear decimal arithmetic mode.
            CLI
            LDA #$1F        ;* Init ACIA to 19200 Baud.
            STA ACIA_CTRL
            LDA #$0B        ;* No Parity.
            STA ACIA_CMD
            LDA #$0D
            JSR ECHO        ;* New line.
            LDA #$0A
            JSR ECHO
            LDA #<MSG1
            STA MSGL
            LDA #>MSG1
            STA MSGH
            JSR SHWMSG      ;* Show Welcome.
            LDA #$0D
            JSR ECHO        ;* New line.
            LDA #$0A
            JSR ECHO      
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
NEXTCHAR    LDA ACIA_SR     ;*See if we got an incoming char
            AND #$08        ;*Test bit 3
            BEQ NEXTCHAR    ;*Wait for character
            LDA ACIA_DAT    ;*Load char
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
            CMP #$CC        ;* "L"?
            BEQ LOADINT     ;* Yes, Load Intel Code.
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

LOADINT     JSR LOADINTEL   ;* Load the Intel code.
            JMP SOFTRESET   ;* When returned from the program, reset EWOZ.

NOESCAPE    BIT MODE        ;Test MODE byte.
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
            AND #$0F        ;For MOD 8=0 ** changed to $0F to get 16 values per row **
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
ECHO        PHA             ;*Save A
            AND #$7F        ;*Change to "standard ASCII"
            STA ACIA_DAT    ;*Send it.
.WAIT       LDA ACIA_SR     ;*Load status register for ACIA
            AND #$10        ;*Mask bit 4.
            BEQ    .WAIT    ;*ACIA not done yet, wait.
            PLA             ;*Restore A
            RTS             ;*Done, over and out...

SHWMSG      LDY #$0
.PRINT      LDA (MSGL),Y
            BEQ .DONE
            JSR ECHO
            INY 
            BNE .PRINT
.DONE       RTS 


; Load an program in Intel Hex Format.
LOADINTEL   LDA #$0D
            JSR ECHO        ;New line.
            LDA #<MSG2
            STA MSGL
            LDA #>MSG2
            STA MSGH
            JSR SHWMSG      ;Show Start Transfer.
            LDA #$0D
            JSR ECHO        ;New line.
            LDY #$00
            STY CRCCHECK    ;If CRCCHECK=0, all is good.
INTELLINE   JSR GETCHAR     ;Get char
            STA IN,Y        ;Store it
            INY             ;Next
            CMP #$1B        ;Escape ?
            BEQ INTELDONE   ;Yes, abort.
            CMP #$0D        ;Did we find a new line ?
            BNE INTELLINE   ;Nope, continue to scan line.
            LDY #$FF        ;Find (:)
FINDCOL     INY
            LDA IN,Y
            CMP #$3A        ; Is it Colon ?
            BNE FINDCOL     ; Nope, try next.
            INY             ; Skip colon
            LDX #$00        ; Zero in X
            STX CRC         ; Zero Check sum
            JSR GETHEX      ; Get Number of bytes.
            STA COUNTER     ; Number of bytes in Counter.
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
            JSR GETHEX      ; Get Hi byte
            STA STH         ; Store it
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
            JSR GETHEX      ; Get Lo byte
            STA STL         ; Store it
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
            LDA #$2E        ; Load "."
            JSR ECHO        ; Print it to indicate activity.
NODOT       JSR GETHEX      ; Get Control byte.
            CMP #$01        ; Is it a Termination record ?
            BEQ INTELDONE   ; Yes, we are done.
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
INTELSTORE  JSR GETHEX      ; Get Data Byte
            STA (STL,X)     ; Store it
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            STA CRC         ; Store it
            INC STL         ; Next Address
            BNE TESTCOUNT   ; Test to see if Hi byte needs INC
            INC STH         ; If so, INC it.
TESTCOUNT   DEC COUNTER     ; Count down.
            BNE INTELSTORE  ; Next byte
            JSR GETHEX      ; Get Checksum
            LDY #$00        ; Zero Y
            CLC             ; Clear carry
            ADC CRC         ; Add CRC
            BEQ INTELLINE   ; Checksum OK.
            LDA #$01        ; Flag CRC error.
            STA CRCCHECK    ; Store it
            JMP INTELLINE   ; Process next line.

INTELDONE   LDA CRCCHECK    ; Test if everything is OK.
            BEQ OKMESS      ; Show OK message.
            LDA #$0D
            JSR ECHO        ; New line.
            LDA #<MSG4      ; Load Error Message
            STA MSGL
            LDA #>MSG4
            STA MSGH
            JSR SHWMSG      ; Show Error.
            LDA #$0D
            JSR ECHO        ; New line.
            RTS

OKMESS      LDA #$0D
            JSR ECHO        ; New line.
            LDA #<MSG3      ; Load OK Message.
            STA MSGL
            LDA #>MSG3
            STA MSGH
            JSR SHWMSG      ; Show Done.
            LDA #$0D
            JSR ECHO        ; New line.
            RTS

GETHEX      LDA IN,Y        ; Get first char.
            EOR #$30
            CMP #$0A
            BCC DONEFIRST
            ADC #$08
DONEFIRST   ASL
            ASL
            ASL
            ASL
            STA L
            INY
            LDA IN,Y        ; Get next char.
            EOR #$30
            CMP #$0A
            BCC DONESECOND
            ADC #$08
DONESECOND  AND #$0F
            ORA L
            INY
            RTS

GETCHAR     LDA ACIA_SR     ; See if we got an incoming char
            AND #$08        ; Test bit 3
            BEQ GETCHAR     ; Wait for character
            LDA ACIA_DAT    ; Load char
            RTS


MSG1        .byte "BadApple EWOZ Beep IRQ 0.2",0
MSG2        .byte "Start Intel Hex code Transfer.",0
MSG3        .byte "Intel Hex Imported OK.",0
MSG4        .byte "Intel Hex Imported with checksum error.",0


 .org $9000 ;Need to remember where this is.
LAB_CLS
      PHP ;make call-able from other stuff?
      PHA ;Seems to have issues, 
      PHX ;Push/Pop
      PHY 

      
      ;ADDED BEEP CLEAR 
      STZ   VIA_AUX;$600b;needs to be via address for PB7 timer
      LDX #0
      BRA FillScreen
 
 .org $9010 ;Lets keep it in 1 spot even if I remove or change beep clear above 
LAB_COLOR;Fifty1Ford
      PHP ;make call-able from other stuff
      PHA
      PHX 
      PHY 
;       ;JSR LAB_GTBY ;get byte in X
;       STX BackColor ;lets store the background color
       LDX BackColor ;lets LOAD the background color
;       ;Lets claw back a few OPS from GTBY and just fall through
;       ;BRA FillScreen
;       ;RTS

 
;Best FillScreen I could manage?? Smaller is easy, but can you do faster?
;32,910 cycles, follows the scanbeam. 
;100 Cycles less than split routine. Around 1/3 Cycles of orgional loop. 
;IE unrolled like this is 3x faster!

FillScreen:
      TXA ;Color stored in X
      ;We'll DEC instead of a INC and CMP to save cycles.
      LDX #255 ;one more than needed because of DEX below IE 0    
FillScreenLoop:
      ;DEX ;DEX up here so we can clear the 0 row
      PHP
      INX
  	;This is unrolled so that there is a STA for each row of the screen.
      ;Display mapping has 28 bytes at end of each row unused.
      ;Display location = $2000
      STA Display, x 
      STA Display +$80,x
	STA Display +$100,x
	STA Display +$180,x
	STA Display +$200,x
	STA Display +$280,x
	STA Display +$300,x
	STA Display +$380,x
	STA Display +$400,x
	STA Display +$480,x
	STA Display +$500,x
	STA Display +$580,x
	STA Display +$600,x
	STA Display +$680,x
	STA Display +$700,x
	STA Display +$780,x
	STA Display +$800,x
	STA Display +$880,x
	STA Display +$900,x
	STA Display +$980,x
	STA Display +$A00,x
	STA Display +$A80,x
	STA Display +$B00,x
	STA Display +$B80,x
	STA Display +$C00,x
	STA Display +$C80,x
	STA Display +$D00,x
	STA Display +$D80,x
	STA Display +$E00,x
	STA Display +$E80,x
	STA Display +$F00,x
	STA Display +$F80,x
	;MIDDLE ROW 31
	STA Display +$1000,x
	STA Display +$1080,x
	STA Display +$1100,x
	STA Display +$1180,x
	STA Display +$1200,x
	STA Display +$1280,x
	STA Display +$1300,x
	STA Display +$1380,x
	STA Display +$1400,x
	STA Display +$1480,x
	STA Display +$1500,x
	STA Display +$1580,x
	STA Display +$1600,x
	STA Display +$1680,x
	STA Display +$1700,x
	STA Display +$1780,x
	STA Display +$1800,x
	STA Display +$1880,x
	STA Display +$1900,x
	STA Display +$1980,x
	STA Display +$1A00,x
	STA Display +$1A80,x
	STA Display +$1B00,x
	STA Display +$1B80,x
	STA Display +$1C00,x
	STA Display +$1C80,x
	STA Display +$1D00,x
	STA Display +$1D80,x
	STA Display +$1E00,x
	STA Display +$1E80,x
	STA Display +$1F00,x
	STA Display +$1F80,x
	CPX #99
	BEQ FillScreenLoopEnd
	JMP FillScreenLoop
FillScreenLoopEnd:
      ;PLY ;Pop/Pull
      ;PLX 
      ;PLA 
      PLP 
      
	RTS
    ;JMP SOFTRESET


NMI:
    ; NMI:;
; BadApple Beep. Broken IRQ stuff anyway...  
; For my Ben Eater Breadboard Bad Apple! Demo I need a IRQ driven
; music routine. I'm still using EhBASIC as my 'Testbed' doing 
; tests and loading the test programs, etc. I try to stay out of the
; way of the rest of EhBASIC, but who knows.....   
; These stomp on it but are only read if we turn on the buffer.
; Maybe I'll build it into EhBasic at some point?

;BeepEnable  = $DF  ;ZP
;VGAClock    = $E2  ;ZP
      PHP ;Push that CPU flag
      PHA 
      PHX ;I don't actually mess with x
      PHY
      
      ;This is reset by user program.
      LDA #1
      STA VGASync
      ;Do I need this CLC??

      CLC
      DEC VGAClock ;$E2 ;VGACounter Lets dec 

      LDA BeepEnable ;$DF ;BeepEnable/IRQ
      BEQ NMIExit;IRQ Beep/buffer off

      LDA BeepCount
      BEQ NMIEmptyBuffer ;Exit if there is nothing in the buffer
      
      ;Do I need this CLC??
      CLC
      DEC BeepCount
      
      ;Kinda' a waste of cycles, but what can you do?
      ;Setup 'Beep' on PB7 to make sure it is on.
      LDA VIA_AUX;$600b
      ORA #$C0   ;1100-0000; square-wave/RECTANGLE output on PB7.  (Don't enable the
      STA VIA_AUX;$600b    ; T1 interrupt in the IER though.)
      LDA #255           ; Set the T1 timeout period. 
      STA VIA_T1CL;$6004

      ;Need to load the note to see if it is Zero 0
      LDX BeepRead; Load Buffer Location Pointer
      
      ;Do I need this??
      CLC
      INC BeepRead; INC Pointer for next time.      
      
      LDA BeepBuffer,X ;(BeepBuffer),y(buffer pointer)
      BEQ NMINoBeep ; on 0 stop Beep/PB7 timer
      
      ;OK, lets store the note
      STA VIA_T1CH;$6005;
      ;BEEP!
      ;sta ACIA ;$5000 ;testing
      BRA NMIExit

NMINoBeep
      STZ VIA_AUX ;$600b ;Beep Off
NMIExit
      ;CPY #99;99 byte buffer
      ;BEQ NMIResetBuffer
      
      PLY
      PLX ;I don't actually mess with x
      PLA
      PLP ;Pull that flag!
      RTI
NMIEmptyBuffer
      STZ BeepEnable
      ;STZ VIA_AUX
      PLY
      PLX ;I don't actually mess with x
      PLA
      PLP ;Pull that flag!
      RTI


IRQ:
            RTI


   .org $FFFA
   .word NMI
   .word RESET
   .word IRQ