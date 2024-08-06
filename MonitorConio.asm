;MonitorConio

;;**********************************************************************
;  Common (intermediate) routines for the TMS70C02 serial monitor 
;;**********************************************************************  


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

;;**********************************************************************
;; INHEXBF  Fast INHEXB  No echo no checking for esc or CR
;; Used for PC to Target communications like GETIHEX
;;**********************************************************************
INHEXBF PUSH    B                  ;Save original B
        CALL    WAIT4BYTE            ;Get 1 char
         
        CMPA    #':'              ;> '9' ?? then add 9
        JL      _INHEXF1
        ADD     #9, A
_INHEXF1  RL   A                   ;Shift low nibble to high nibble
        RL      A          
        RL      A
        RL      A
        AND     #0F0h, A           ;Mask low bits
        PUSH    A                  ;Save high nibble on stack
         
        CALL    WAIT4BYTE            ;Get another char
         
        CMPA    #':'              ;> '9' ?? then add 9
        JL      _INHEXF2
        ADD     #9, A
_INHEXF2 AND     #00Fh, A           ;Mask High nibble
        POP     B                  ;get high nibble from stack ->B
        OR      B,A                ;combine A & B
        POP     B                  ;Original Restore B
        RETS

;;**********************************************************************
;; CHRS2BIN - MSN-char in A, LSN-char in B. Returned value in A
;;**********************************************************************
CHRS2BIN
        CALL    @CHR2NIB        ; return value in A, lower nibble
        SWAP    A
        MOV     A, CREG
        MOV     B, A
        CALL    @CHR2NIB
        OR      CREG, A        ; Merge nibbles
        RETS

;;                     0-9      A-F      a-f
;;                   30h-39h, 41h-46h, 61h-66h
;; sub '0'            0h-9h,  11h-16h, 31h-36h
;; if >9h sub 7       0h-9h,  0Ah-0Fh, 2Ah-2Fh
;; if >0Fh sub 20h    0h-9h,  0Ah-0Fh, 0Ah-0Fh       
CHR2NIB
        SUB     #'0', A     ; (A)-'0'
        CMP     #0Ah, A     ; (A)-'9' -- is it 0-9 ?
        JN      _C2NOK      ; Jump if yes
        SUB     #7, A
        CMP     #10h, A     ; is it A-F ?
        JN      _C2NOK
        SUB     #20h, A

_C2NOK
        AND     #0Fh, A
        RETS

;;**********************************************************************
;; INCADD3
;;**********************************************************************   
INCADD3 INC     ADDR3   ; next address LSB
        JNZ     _INCA3X
        INC     ADDR3-1 ; next address MSB
_INCA3X  RETS

;;**********************************************************************
;; Output newline; C R + LF
;;**********************************************************************   
NEWLINE MOV     #CR, A
        CALL    @OUTCHR
        MOV     #LF, A
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
