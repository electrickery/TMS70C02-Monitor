;MonitorConio3

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
;;**********************************************************************   
; Get byte from receive buffer 
;;**********************************************************************    
GETBYTE MOVP    RXBUF, A    ; RXBUF -> A
;        BTJZ    SYSFLGS, 01h, _GNBOECHO
;        CALL    @OUTCHR
_GNBOECHO
        RETS
        
NEWLINE MOV     #CR, A
        CALL    @OUTCHR
        MOV     #LF, A
        CALL    @OUTCHR
        RETS

; ***** Prompt

PROMPT  CALL    @NEWLINE
        MOV     #'>', A
        CALL    @OUTCHR
        RETS

;;**********************************************************************
;; OUTSTR
;; Output A string pointed to by RP37&38
;;**********************************************************************
OUTSTR  LDA     *MSGPTR    ; (RP38 & R37) -> A
        TSTA               ; Test for Zero  (end of string)
        JZ      _OUTSTRX    ; If Zero then EXIT
        CALL    OUTCHR     ; Otherwise output the character
        INC     MSGPTR     ; Inc MSGPTR
        JNZ     _OSNOC
        INC     MSGPTR-1
_OSNOC   JNE     OUTSTR
        INC     MSGPTR
        BR      OUTSTR     ; Continue...
_OUTSTRX RETS  

;;**********************************************************************
; TOUPPER
; converts character in A in range a-z to A-Z
;;**********************************************************************  
TOUPPER   
        CMP     #'a', A         ; 
        JN      _TPNOT          ; smaller than  'a'
        CMP     #'{', A
        JP      _TPNOT          ; bigger than 'z'
        AND     #11011111b, A   ; clear bit 5: lower to upper case
_TPNOT
        RETS
        
;;**********************************************************************
; FIRSTADR, SECONADR, GETDATA - interpret command line as 'ssss eeee dd'
;  to be replaced by a generic routine, similar to GETDATA
;;**********************************************************************
FIRSTADR        
        MOV     CLBUF,   A
        MOV     CLBUF+1, B
        CALL    @CHRS2BIN
        MOV     A, ADDR1-1      ; MSB start

        MOV     CLBUF+2, A
        MOV     CLBUF+3, B
        CALL    @CHRS2BIN
        MOV     A, ADDR1       ; LSB start
        RETS

SECONADR
        MOV     CLBUF+5,   A
        MOV     CLBUF+6, B
        CALL    @CHRS2BIN
        MOV     A, ADDR2-1      ; MSB end

        MOV     CLBUF+7, A
        MOV     CLBUF+8, B
        CALL    @CHRS2BIN
        MOV     A, ADDR2       ; LSB end
        RETS

THIRDADR
        MOV     #10, B          ; 10
        LDA     CLBUF(B)
        PUSH    A
        INC     B
        MOV     B, DREG
        LDA     CLBUF(B)        ; 11
        MOV     A, B
        POP     A
        CALL    @CHRS2BIN
        MOV     A, ADDR4-1
        
        MOV     DREG, B
        INC     B
        LDA     CLBUF(B)        ; 12
        PUSH    A
        INC     B
        LDA     CLBUF(B)        ; 13
        MOV     A, B
        POP     A
        CALL    @CHRS2BIN
        MOV     A, ADDR4
        
        RETS
        


;;**********************************************************************
; GETDATA - retrieve data from CLBUF, using B as index pointer
;;**********************************************************************
GETDATA                        ; 
        LDA     CLBUF(B)
        PUSH    A
        INC     B
        LDA     CLBUF(B)
        PUSH    A
        POP     B
        POP     A
        CALL    @CHRS2BIN
        MOV     A, DATA
        RETS        
