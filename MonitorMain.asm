; TMS70C02 serial monitor 
; asl -cpu TMS70C02 MonitorMain.asm -o MonitorMain.p -L -olist MonitorMain.lst -v -C
; p2hex MonitorMain.p MonitorMain.hex

; All labels prefixed with a '_' are internal to the routine only, not 
; to be used outside it. In the Geany symbols list this keeps them sorted 
; apart from the main labels.

        INCLUDE PR_TMS70C02.asm

VERSMYR EQU     "0"
VERSMIN EQU     "3"
VERSPAT EQU     "6"

; Constants
HIBITMK EQU     7Fh
TXBSYBT EQU     01h
LF      EQU     0Ah    
CR      EQU     0Dh
ESC     EQU     01Bh

; Aliasses
;AREG    EQU     R0
;BREG    EQU     R1
CREG    EQU     R2
DREG    EQU     R3
I2REG   EQU     R4
I5REG   EQU     R5
COUNT1  EQU     R6

SYSFLGS EQU     R8     ; bit 0 = echo
OUTBYTE EQU     R9
INBYTE  EQU     R10     ;
MSGPTR  EQU     R12     ; H=11 L=12
ADDR1   EQU     R14     ; H=13 L=14  source / start address     0Dh-0Eh
ADDR2   EQU     R16     ; H=15 L=16  destination / end  address 0Fh-10h
ADDR3   EQU     R18     ; H=17 L=18  iterating address pointer  11h-12h
ADDR4   EQU     R20     ; H=19 L=20  iterating destination address 13h-14h  
DATA    EQU     R21     ; 15h

; Note buffer must be placed within one page (MSB doesn't change)
CLBUFPM EQU     R30    ; 001Eh   ; R110 pointer MSB
CLBUFP  EQU     R31    ; 001Fh   ; R111 command line buffer pointer LSB
CLBUF   EQU     R32    ; 0020h   ; R112 - R127   command line buffer
CLBUFE  EQU     R76    ; 004Ch   ;  command line buffer end



SP      EQU     0080h   ; R128 and up

; DBC board:
; 0000h - 00FFh = internal registers & RAM: R0(A), R1(B), ... R255
; 0100h - 0126h = peripherial registers P0-P26
; 2000h - 3FFFh = RAM
; E000h - FFFFh = ROM
    
        ORG     0E000h
        
        DB      "TMS70C02 Monitor", 0
        
        INCLUDE MonitorConio.asm
        INCLUDE MonitorCommands.asm

; TRAP0
RESET   MOV     #SP, B
        LDSP                            ; Set SP = 0x0080
        DINT                            ; Disable interrupts
        MOVP    #00101010b, IOCNT0  ; PO Clear INTI-, INT2, and INT3- flags, 
                                    ;  disable all INTns (3-32)
        MOVP    #00100010b, IOCNT2  ; P1 Select falling edge only for INTI & INT3 
        MOVP    #00001010b, IOCNT1  ; P2 Clear and disable INT4 and INT5
        MOVP    %00001000b, PORTA   ; %>0D0, PORTA    ;PORTA=x50    ;LEDs OFF
        MOVP    %10001110b, DDRA    ; %>0DF, ADDR     ;DDRA=0xDF  
        MOVP    %008h,      PORTB   ; PORTB=x0f (bit 0-3 only)
        MOVP    %01010101b, SCTL0   ; reset serial port & error flags
        MOVP    #0FFh,      T1MDATA ; P12 Load Timer 1 MSB reload register 
        MOVP    #0FFh,      T1LDATA ; P13 Load Timer 1 LSB reload register 
        MOVP    #00h,       T1CTL1  ; P14 Disable the timer output on B1 
        MOVP    #00h,       T1CTL0  ; P15 Initialize clock start, source, halt 
                                    ;   bit and prescaler value 
        MOVP    #0FFh,      T2MDATA ; P16 Load Timer 2 MSB reload register 
        MOVP    #0FFh,      T2LDATA ; P17 Load Timer 2 LSB reload register 
        MOVP    #00h,       T2CTL1  ; P18 Disable the timer output on B0 
        MOVP    #00h,       T2CTL0  ; P19 Initialize clock start, source, halt 
                                    ;  bit and prescaler value 
        
        MOV     #00h,       SYSFLGS     ; Echo ON
        MOVD    #CLBUF,     CLBUFP
        MOVD    #0000h,     ADDR1
        MOVD    #00FFh,     ADDR2
        MOV     #00h,       DREG
        CALL    @UARTINIT               ; Setup UART for 9600b
        CALL    @INT5INIT
        CALL    @DSPINIT
        EINT

;MON_PRMPT_LOOP:
        ; Print monitor prompt
        ; Get a character from user into Acc
        ; Call appropriate routine via MONCNDS 
        ; Loop 

;;**********************************************************************        
;; Main loop
;;**********************************************************************        
        MOVD    #INITMSG, MSGPTR
        CALL    @OUTSTR

        CALL    @PROMPT
_LOOP
        CALL    @WAIT4BYTE
        CMP     #CR, A
        JNZ     _LNOCR
        
        CALL    @PROMPT
        JMP     _LCONT
        
_LNOCR
        CALL    @MONCMDS
        CALL    @PROMPT
_LCONT
        XORP    %00000011b, PORTB       ; toggle B0 & P1 pins
        JMP     _LOOP
        
INITMSG
        DB      CR, LF, "** TMS70C02 Monitor Help Menu V", VERSMYR, ".", VERSMIN, ".", VERSPAT, " **", CR, LF, 0

;;**********************************************************************        
;; Prompt
;;**********************************************************************        
PROMPT  CALL    @NEWLINE
        MOV     #'>', A
        CALL    @OUTCHR
        RETS

;;**********************************************************************        
; ***** Interpreter - used by most commands, HELP being the exception
;;**********************************************************************        
MONCMDS
        CALL    @OUTCHR
        CALL    @TOUPPER
        CMP     #'C', A
        JNZ     _MC00
        CALL    @CMD_CALL
        JMP     _MC99
_MC00        
        CMP     #'D', A
        JNZ     _MC01
        CALL    @CMD_DUMP
        JMP     _MC99
_MC01        
        CMP     #'E', A
        JNZ     _MC03
        CALL    @CMD_ECHO
        JMP     _MC99
_MC03        
        CMP     #'F', A
        JNZ     _MC05
        CALL    @CMD_FILL
        JMP     _MC99
_MC05        
        CMP     #'G', A
        JNZ     _MC07
        CALL    @CMD_GO
        JMP     _MC99
_MC07        
        CMP     #'H', A
        JNZ     _MC09
        CALL    @CMD_HELP
        JMP     _MC99
_MC09        
        CMP     #'?', A
        JNZ     _MC13
        CALL    @CMD_HELP ; produces direct output
        JMP     _MC99
_MC13
        CMP     #'M', A
        JNZ     _MC15
        CALL    @CMD_MOD
        JMP      _MC99
_MC15
        CMP     #'R', A
        JNZ     _MC17
        CALL    @CMD_RAMT
        JMP      _MC99
_MC17
        CMP     #'T', A
        JNZ     _MC19
        CALL    @CMD_TEST
        JMP      _MC99
_MC19
        CMP     #'V', A
        JNZ     _MC21
        CALL    @CMD_COPY
        JMP      _MC99
_MC21
        CMP     #':', A
        JNZ     _MC23
        CALL    @CMD_HXINT
        JMP      _MC99
_MC23
        CMP     #'K', A
        JNZ     _MC24
        CALL    @CMD_KEYT
        JMP     _MC99
_MC24

_MC99
        RETS
        

COLLECT
        MOVD    #CLBUF, CLBUFP  ; set command line buffer start
_COLNXT
        CALL    WAIT4BYTE
        CMP     #CR, A
        JZ      _COLLDONE       ; when it is a CR, back to main
        CMP     #ESC, A
        JZ      _COLLESC
        CALL    @OUTCHR
        STA     *CLBUFP         ; store in buffer
        INC     CLBUFP          ; update pointer LSB and 
        
;        CALL    @COLLDBG
        CMP     #CLBUFE, CLBUFP       ; is it past the buffer?
        JPZ     _COLLERR
        BR      _COLNXT         ; loop for next char

_COLLESC                        ; ESC pressed
        MOV     #CLBUF, CLBUFP
        MOVD    #CLBESCMSG, MSGPTR
        CALL    @OUTSTR
        SETC                   ; set carry flag to abort current command
        RETS
        
_COLLERR                        ; buffer overflow
        MOV     #CLBUF, CLBUFP
        MOVD    #CLBERMSG, MSGPTR
        CALL    @OUTSTR
        RETS

_COLLDONE                       ; CR pressed
        CLR     A
        STA     *CLBUFP         ; terminate the buffer
        RETS
        
CLBERMSG
        DB      CR, LF, "Command line buffer overflow.", CR, LF, 0
        
CLBESCMSG
        DB      CR, LF, "ESC", CR, LF, 0

;; Basic routines, touching actual hardware
        
;;**********************************************************************
; OUTCHR  Send Serial 1 byte from A (PORTB Bit3=TXD)
; A,B are unchanged uses R33 (OUTBYTE)
;;**********************************************************************
OUTCHR  PUSH    A
_TXEMPTY 
        MOVP    SSTAT, A    ; SSTAT-P22  Bit0=TXRDY
        AND     %TXBSYBT, A
        JZ      _TXEMPTY     ; 0=TXBUSY keep checking...
        POP     A
        MOVP    A, TXBUF    ; TXBUF=P26  A->TXBUF 
        RETS

;;**********************************************************************
; CHKKEY test if RXBUF is full (data available)
; RETURNS: Z=0 if data is available
;;**********************************************************************
CHKKEY  MOVP SSTAT, A   ;Get UART SSTAT reg    9 cycles
        AND  %02h, A    ;Test for RXRDY bit    7 cycles
        RETS            ;Return Z=0 (NOT Zero return 0x20) if data otherwise zero  7 cycles


;;**********************************************************************
; WAIT4BYTE  Wait for serial char, only return when it arrived
;;**********************************************************************
WAIT4BYTE 
_WAITKEY CALL    @CHKKEY  
        JZ      _WAITKEY    ; If NOT SET keep checking
        ; Automatically runs GETBYTE when char received
;;**********************************************************************   
; Get byte from receive buffer 
;;**********************************************************************    
GETBYTE MOVP    RXBUF, A    ; RXBUF -> A
;        BTJZ    SYSFLGS, 01h, _GNBOECHO
;        CALL    @OUTCHR
_GNBOECHO
        RETS
               
; E.M. Klaus code modified & annotated
;;**********************************************************************
;; UARTINIT Setup UART for 9600b 8 data 1 stop No Parity
;; NOTE: @9600b 1 char takes 1.04ms
;;**********************************************************************
UARTINIT
        MOVP    %051h, SCTL0    ;SCTL0 = x10 Reset UART 0b01010001
        MOVP    %7, T3DATA      ;T3DATA= 14 for 4800b,  7 for 9600b (at 4.9152 MHz clock)
        ORP     %0F9h, PORTB    ;ENABLE TX Pin   PORTB Bit3=TXD 0b11111001
        ANDP    %0DFh, DDRA     ;PORTA BIT5 = INPUT (RXD)       0b11011111
        MOVP    %01001110b, SMODE ; %>0CE, SMODE    ;SMODE =CE 01001110   1 Stop, No Parity, 8Data, Async
        MOVP    %015h, SCTL0    ;SCTL0 =15 00010101   Tx & Rx Enabled
        MOVP    %0C0h, SCTL1    ;SCTL1 =C0 11000000   Use Timer3, no prescale bits
        RETS

        INCLUDE DKmonitor.asm

_INT5    ; Timer/Counter 2 is part of DKmonitor.asm
        RETI
 
INT4    ; Serial port
        BR      RESET
        
INT3    ; /INT3 pin 12
        BR      RESET
        
INT2    ; Timer/Counter 1
        BR      RESET
                
INT1    ; /INT1 pin 13
        BR      RESET
        
        ORG     0FFF4h          ; Set up 6 vectors 
                                ; =interrupts 3.6 Interrupts and System Reset 3-26
        DW      INT5    ; for Timer 2
        DW      INT4    ; for serial port
        DW      INT3    ; /INT3 pin
        DW      INT2    ; for Timer 1
        DW      INT1    ; /INT1 pin
        DW      RESET 
        
        END
