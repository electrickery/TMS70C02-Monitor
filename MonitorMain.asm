; TMS70C02 serial monitor 
; asl -cpu TMS70C02 MonitorMain.asm -o MonitorMain.p -L -olist MonitorMain.lst -v -C
; p2hex MonitorMain.p MonitorMain.hex

        INCLUDE PR_TMS70C02.asm
        


VERSMYR EQU     "0"
VERSMIN EQU     "1"
VERSPAT EQU     "0"

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
COUNT1  EQU     R4
SYSFLGS EQU     R30     ; bit 0 = echo
OUTBYTE EQU     R33
INBYTE  EQU     R34
MSGPTR  EQU     R38     ; H=37 L=38
ADDR1   EQU     R40     ; H=39 L=40  source / start address
ADDR2   EQU     R42     ; H=41 L=42  destination / end  address
ADDR3   EQU     R44     ; H=43 L=44  iterating address pointer

; Note buffer must be placed within one page (MSB doesn't change)
CLBUFPM EQU     R110    ; 006Eh   ; R110 pointer MSB
CLBUFP  EQU     R111    ; 006Fh   ; R111 command line buffer pointer LSB
CLBUF   EQU     R112    ; 0070h   ; R112 - R127   command line buffer
CLBUFE  EQU     R127    ; 007Fh   ;  command line buffer end
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

START   MOV     #SP, B
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
;        EINT

;;**********************************************************************        
;; Main loop
;;**********************************************************************        
        CALL    @CMD_HELP
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
        XORP    %00000011b, PORTB
        JMP     _LOOP
        
        
;MON_PRMPT_LOOP:
        ;Print monitor prompt
        ;Get a character from user into Acc
        ;Print a new line
        ;Respond to user input
        ;Print a new line	
;        JMP      MON_PRMPT_LOOP
;;**********************************************************************        

; ***** Interpreter
       
MONCMDS
        CALL    @OUTCHR
        CMP     #'D', A
        JNZ     _MC01
        CALL    @CMD_DUMP
        BR      _MC99
_MC01        
        CMP     #'d', A
        JNZ     _MC02
        CALL    @CMD_DUMP
        BR      _MC99
_MC02
        CMP     #'E', A
        JNZ     _MC03
        CALL    @CMD_ECHO
        BR      _MC99
_MC03        
        CMP     #'e', A
        JNZ     _MC04
        CALL    @CMD_ECHO
        BR      _MC99
_MC04        
        CMP     #'H', A
        JNZ     _MC05
        CALL    @CMD_HELP
        BR      _MC99
_MC05        
        CMP     #'h', A
        JNZ     _MC06
        CALL    @CMD_HELP
        BR      _MC99
_MC06        
        CMP     #'?', A
        JNZ     _MC07
        CALL    @CMD_HELP
        BR      _MC99
_MC07
        CMP     #'M', A
        JNZ     _MC08
        CALL    @COLLECT
        CALL    @CMD_MOD
        BR      _MC99
_MC08


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
        
;COLLDBG
;        MOV     #" ", A
;        CALL    @OUTCHR
;        MOV     #CLBUFE, A
;        CALL    @OUTHEX
;        MOV     #"?", A
;        CALL    @OUTCHR
;        MOV     CLBUFP, A
;        CALL    @OUTHEX
;        MOV     #" ", A
;        
;        RETS
        
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
        
CLBERMSG
        DB      CR, LF, "Command line buffer overflow.", CR, LF, 0
        
CLBESCMSG
        DB      CR, LF, "ESC", CR, LF, 0
        
        
        ORG    0FFF6h           ; Set up 4 vectors 
                                ; =interrupts 
        DW    START, START, START, START, START 
        
        
        END
