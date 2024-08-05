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
;; OUTNIBH
;; OUTPUT High 4 bits of A as 1 HEX Digit
;; OUTNIBL
;; OUTPUT Low 4 bits of A as 1 HEX Digit
;;**********************************************************************
OUTNIBH SWAP    A           ; OUT HEX LEFT HEX DIGIT
OUTNIBL AND     #00Fh, A    ; OUT HEX RIGHT HEX DIGIT
        OR      #'0', A
        CMPA    #':'
        JL      _OUTNIBX
        ADD     #7, A
_OUTNIBX CALL    @OUTCHR
        RETS 
     
;;**********************************************************************
;; OUTHEX
;; Output A as 2 HEX digits
;;**********************************************************************
OUTHEX  PUSH    A
        CALL    @OUTNIBH     ; Print High 4 bits
        POP     A            ; Get A from B 
        CALL    @OUTNIBL     ; Print Low 4 Bits
        RETS
       
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
        MOV     #0, B
        CALL    @GETADDR
        MOV     A, ADDR1-1
        MOV     B, ADDR1
        RETS

SECONADR
        MOV     #5, B           ; at index 5
        CALL    @GETADDR
        MOV     A, ADDR2-1
        MOV     B, ADDR2
        RETS

THIRDADR
        MOV     #10, B          ; at index 10
        CALL    @GETADDR
        MOV     A, ADDR4-1
        MOV     B, ADDR4
        RETS

;;**********************************************************************
; GETADDR - generic ASCII command-line retrieval. 
;           On input B contains the index to the first character.
;           Output: A is MSB, B is LSB 
;;**********************************************************************
GETADDR
        LDA     CLBUF(B)        ; B = +0 CLBUF pointer
        PUSH    A               ; A = 1st MSB char
        INC     B
        MOV     B, DREG         ; B, DREG = +1 CLBUF pointer
        LDA     CLBUF(B)        
        MOV     A, B            ; A, B = 2nd MSB char
        POP     A               ; A = 1st MSB char
        CALL    @CHRS2BIN       ; A, B input chars
        PUSH    A               ; A = address MSB
        
        MOV     DREG, B         ; B = +1 CLBUF pointer
        INC     B               ; B = +2 CLBUF pointer
        LDA     CLBUF(B)
        PUSH    A               ; A = 1st LSB char
        INC     B               ; B = +3 CLBUF pointer
        MOV     B, DREG         ; B, DREG = +3 CLBUF pointer
        LDA     CLBUF(B)
        MOV     A, B            ; A, B = 2nd LSB char
        POP     A               ; A = 1st LSB char
        CALL    @CHRS2BIN       ; A, B input chars
        MOV     A, B            ; A, B = address LSB
        POP     A               ; A = address MSB
        RETS
        

;;**********************************************************************
; GETDATA - retrieve data from two CLBUF bytes, using B as index pointer
;;**********************************************************************
GETDATA                        ; 
        LDA     CLBUF(B)
        PUSH    A
        INC     B
        LDA     CLBUF(B)
        MOV     A, B
        POP     A
        CALL    @CHRS2BIN
        RETS        

OUT1STAD
        MOV     ADDR1-1, A    ; MSB
        CALL    @OUTHEX
        MOV     ADDR1, A    ; LSB
        CALL    @OUTHEX
        RETS
        
OUT2NDAD
        MOV     ADDR2-1, A    ; MSB
        CALL    @OUTHEX
        MOV     ADDR2, A    ; LSB
        CALL    @OUTHEX
        RETS
        
OUT3RDAD
        MOV     ADDR4-1, A    ; MSB
        CALL    @OUTHEX
        MOV     ADDR4, A    ; LSB
        CALL    @OUTHEX
        RETS

OUTDATA
        MOV     DATA, A
        CALL    @OUTHEX
        RETS
