;MonitorCommands3


; ***** Dump
CMD_DUMP
        CALL    @COLLECT
        JC      _CDABORT        ; check for carry = ESC pressed
        CALL    @NEWLINE
        CALL    @NEWDUMP
_CDABORT

        RETS
        
NEWDUMP                 ; variants: no args     ; use previous address + 1, dump 256 bytes
                        ;           hhhh        ; use as start address, dump 256 bytes
                        ;           hhhh-hhhh   ; use as start & end address
                        ;           +           ; same as no args
                        ;           -           ; use previous address - 255, dump 256 bytes
        
        CMP     #"+", CLBUF 
        JZ      _ND_PLUS
        
        CMP     #'-', CLBUF
        JZ      _ND_MIN
        
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 
        JZ      _ND_NOARG
        
        CMP     #4, B
        JZ      _ND_1ADR
        
        CMP     #9, B
        JZ      _ND_2ADR   
        
        MOVD    #NDERMSG, MSGPTR
        CALL    @OUTSTR
        RETS
        
_ND_NOARG
_ND_PLUS
        CALL    @DUMP256
        JMP     _ND_END

_ND_MIN
        DEC     ADDR1-1
;        MOV     #20h, B
;        SUB     B, ADDR1
;        JNC     _ND_NC
        DEC     ADDR1-1
_ND_NC
        CALL    @DUMP256
        JMP     _ND_END

_ND_1ADR
        CALL    @FIRSTADR

        CALL    @DUMP256
        JMP     _ND_END

_ND_2ADR
        CALL    @FIRSTADR
        CALL    @SECONADR
        
        CALL    @DUMPVAR
        JMP     _ND_END


_ND_END
        RETS
        
NDERMSG 
        DB      " -- Incorrect format. Should be: +|-|aaaa|aaaa bbbb --", CR, LF, 0

; ***** Read / modify echo mode        
CMD_ECHO
        CALL    @COLLECT
        JC      _CE_ABORT
        CALL    @NEWLINE
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 

        CMP     #1, B
        JZ      _CE_1CHAR
        
_CE_ERR
        MOVD    #CEERMSG, MSGPTR
        CALL    @OUTSTR
        JMP     _CE_END

_CE_1CHAR
        MOV     CLBUF,  A
        CMP     #"?", A
        JZ      _CE_QUERY
        CMP     #"0", A
        JZ      _CE_SET
        CMP     #"1", A
        JZ     _CE_SET
        JMP    _CE_ERR
        
_CE_QUERY
        MOVD    #CEPFMSG, MSGPTR
        CALL    @OUTSTR
        MOV     SYSFLGS, A
        AND     #01h, A
        ADD     #"0", A
        CALL    @OUTCHR

        JMP     _CE_END

_CE_SET
        SUB     "0", A
        AND     #0FEh,  SYSFLGS
        OR      A, SYSFLGS
        JMP     _CE_QUERY

_CE_ABORT        
_CE_END
        RETS

CEERMSG DB      " -- Incorrect format. Should be: ?|0|1 --", CR, LF, 0
CEPFMSG DB      "Echo = ", 0
        
; ***** Help text
CMD_HELP    
        MOVD    #INITMSG, MSGPTR
        CALL    @OUTSTR
        MOVD    #HELPMSG, MSGPTR
        CALL    @OUTSTR
        RETS

; ***** Modify memory
CMD_MOD
        CALL    @COLLECT
        JC      _CMABORT        ; check for carry = ESC pressed
        
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 
        
        CMP     #7, B           ; aaaa dd
        JNZ     _CMERR
        
        CALL    @NEWLINE
        CALL    @MEMMOD
        JMP     _CMDONE
_CMERR
        MOVD    #CMERMSG, MSGPTR
        CALL    @OUTSTR  

_CMABORT        
_CMDONE
        RETS

MEMMOD  
        CALL    @FIRSTADR
        MOV     #5, B           ; index for 1st data char
        CALL    @GETDATA2
        MOV     #" ", A
        CALL    @OUTCHR
        MOV     DATA, A
;        CALL    @OUTHEX
        STA     *ADDR1
        RETS

        
CMERMSG DB      " -- Incorrect format. Should be: 'aaaa dd'--", CR, LF, 0

;;**********************************************************************
;; DUMP16                                                              
;; Call with start address in ADDR1
;;**********************************************************************
DUMP16  MOVD    ADDR1, ADDR3    ; Move ADDR1 To Temp RP
        MOV     ADDR3-1, A    ; print Address as 4 HEX chrs MSB
        CALL    OUTHEX
        MOV     ADDR3, A      ; LSB
        CALL    OUTHEX
        MOV     #" ", A       ; Print 2 spaces
        CALL    OUTCHR   
        CALL    OUTCHR
        
        MOV     #16, B        ; Set Byte count
                            ; ** DUMP 16 Data Bytes in HEX ***
_DUMP161 LDA    *ADDR3         ; Get Data byte
        CALL    OUTHEX        ; Print as HEX
        MOV     #" ", A        
        DEC     B
        CMP     #8, B         ; On 8th byte print two spaces
        JNZ     _DUMP162
        CALL    OUTCHR
_DUMP162 CALL    OUTCHR
        CALL    INCADD3       ; Inc ADDR3
        TSTB                ; Done?
        JNZ     _DUMP161       ; Do next byte
        
        CALL    OUTCHR        ; print 2 spaces
        CALL    OUTCHR
        MOVD    ADDR1, ADDR3  ; Move ADDR1 To Temp RP
        MOV     #16, B        ; Set Byte count
        
                            ; ** DUMP 16 Data Bytes in ASCII ***
_DUMP164 LDA    *ADDR3         ; Get Data byte
        CMP     #" ", A       ; Less than blank? 
        JPZ     _DUMP165
        MOV     #".", A
_DUMP165 CMP    #07Fh, A      ; Greater than `~`  
        JL      _DUMP166
        MOV     #".", A
_DUMP166 CALL   OUTCHR        ; print it (or the .)
        CALL    @INCADD3       ; Inc ADDR3
        DEC     B
;        TSTB                ; Done?
        JNZ     _DUMP164       ; Do next byte
;        MOV     #10, A
;        CALL    OUTCHR        ; Print LF&CR then return
;        MOV     #13, A
;        CALL    OUTCHR
_DUMPEND
        MOVD    ADDR3, ADDR1    ; update address for next dump
        RETS

;;************************************************************************
;; DUMP256                                                              
;; Call with start address in ADDR1
;;************************************************************************
DUMP256 MOV   #16, COUNT1
_DMP2561 CALL  DUMP16
        CALL    @NEWLINE
        DEC   COUNT1
        JZ    _DMP256X
        CLRC

        JMP   _DMP2561
_DMP256X RETS

;;************************************************************************
;; DUMPVAR
;;
;;************************************************************************
DUMPVAR 
        CMP     ADDR1-1, ADDR2-1   ; (ADDR2-1) - (ADDR1-1)
        JPZ     _DVLOOP
        JMP     _DVERR
        
_DVLOOP  ; while ADDR1 < ADDR2 call DUMP16
        CALL    @DUMP16
        CMP     ADDR1-1, ADDR2-1        ; ADDR1 will be updated by DUMP16...
        JN      _DV_END
        CMP     ADDR1, ADDR2
        JN      _DV_END
        CALL    @NEWLINE
        JMP     _DVLOOP
        
_DVERR
        MOVD    #DVERMSG, MSGPTR
        CALL    @OUTSTR
        JMP     _DV_END
        
_DV_END
        RETS
        
DVERMSG
        DB      " -- Incorrect format. Must be 'Dssss eeee' and ssss < eeee.", CR, LF, 0
        
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
;; OUTNIBH
;; OUTPUT High 4 bits of A as 1 HEX Digit
;; OUTNIBL
;; OUTPUT Low 4 bits of A as 1 HEX Digit
;;**********************************************************************
OUTNIBH RR      A           ; OUT HEX LEFT HEX DIGIT
        RR      A
        RR      A
        RR      A
OUTNIBL AND     #00Fh, A    ; OUT HEX RIGHT HEX DIGIT
        OR      #'0', A
        CMPA    #':'
        JL      OUTNIBX
        ADD     #7, A
OUTNIBX CALL    OUTCHR
        RETS 
     
;;**********************************************************************
;; OUTHEX
;; Output A as 2 HEX digits
;;**********************************************************************
OUTHEX  PUSH    B
        MOV     A, B        ; Save A in B 
        CALL    OUTNIBH     ; Print High 4 bits
        MOV     B, A        ; Get A from B 
        CALL    OUTNIBL     ; Print Low 4 Bits
        POP     B
        RETS

;;**********************************************************************
;; GETADDR  prompt for address and store in ADDR1  RP
;;**********************************************************************
GETADDR 
;        MOV     #':', A
;        CALL    @OUTCHR
       
        CALL INHEXBF       ; Get 2 hex chars
        MOV  A,     ADDR1-1 ; Store High Address
        CALL INHEXBF       ; Get 2 hex chars
        MOV  A,     ADDR1  ; Store Low Address
_GETADDX RETS

        MOV     ADDR3-1, A  ;
        MOV     ADDR3, B    ;
        
;;
;; CHRS2BIN - MSN-char in A, LSN-char in B. Returned value in A
;;
CHRS2BIN
        CALL    @CHR2NIB
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
        
;DEBUG  PUSH    B 
;       STSP
;       MOV     B, DREG
;       POP B
_C2NOK
        AND     #0Fh, A
        RETS

;;**********************************************************************
;; INCADD3
;;**********************************************************************   
INCADD3 INC     ADDR3   ; next address LSB
        JNZ     INCA3X
        INC     ADDR3-1 ; next address MSB
INCA3X  RETS

;;**********************************************************************
;; FILL command - fills memory with constant
;;**********************************************************************   
CMD_FILL
        CALL    @COLLECT
        JC      _CFABORT        ; check for carry = ESC pressed
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 
        
        CMP     #12, B          ; ssss eeee dd
        JNZ     _CF_ERR
        
        CALL    @NEWLINE
        CALL    @FILLER
        
_CF_ERR        
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 

_CFABORT
        RETS

;;**********************************************************************
;; FILLER
;;**********************************************************************
FILLER
        CALL    @FIRSTADR
        CALL    @SECONADR
        CALL    @GETDATA
        CMP     ADDR1-1, ADDR2-1        ; end must be greater than start
        JN      _FILERR
        
        MOVD    ADDR1, ADDR3
        MOV     DATA, A
_FILLOOP
        STA     *ADDR3
        INC     ADDR3
        JNC     _FILNC
        INC     ADDR3-1
_FILNC
        CMP     ADDR2-1, ADDR3-1        ; MSB pointer check
        JZ      _FLCHK
        JMP     _FILLOOP
_FLCHK        
        CMP     ADDR2, ADDR3            ; LSB pointer check
        JZ      _FILDONE

        JMP     _FILLOOP
        
_FILERR        
        MOVD    #FILER2MSG, MSGPTR
        CALL    @OUTSTR

_FILDONE
        RETS
        
FILER1MSG
        DB      " -- Incorrect format. Should be: 'Fssss eeee dd'", CR, LF, 0
FILER2MSG
        DB      " -- Incorrect address order. Should be: ssss < eeee", CR, LF, 0

;;**********************************************************************
; Go command Jumps to ssss
;;**********************************************************************
CMD_GO
        CALL    @COLLECT
        JC      _CGABORT        ; check for carry = ESC pressed
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 
        
        CMP     #4, B          ; ssss
        JNZ     _CG_ERR
        
        CALL    @NEWLINE   
        CALL    @FIRSTADR
        BR      *ADDR1          ; Ignoring the current CALL stack!
        
        JMP     _CGDONE         ; Won't be reached
        
_CG_ERR
        MOVD    #GOERRMSG, MSGPTR
        CALL    @OUTSTR
_CGABORT
_CGDONE
        RETS

GOERRMSG
        DB      " -- Incorrect format. Should be: 'Gssss'", CR, LF, 0
        
;;**********************************************************************
; Call command to to ssss
;;**********************************************************************
CMD_CALL        
        CALL    @COLLECT
        JC      _CCABORT        ; check for carry = ESC pressed
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 
        
        CMP     #4, B          ; ssss
        JNZ     _CC_ERR
        
        CALL    @NEWLINE   
        CALL    @FIRSTADR
        CALL    *ADDR1          ; 
        
        JMP     _CCDONE         ; Won't be reached
        
_CC_ERR
        MOVD    #CALERMSG, MSGPTR
        CALL    @OUTSTR
_CCABORT
_CCDONE
        RETS

CALERMSG
        DB      " -- Incorrect format. Should be: 'Cssss'", CR, LF, 0        
        
        
;;**********************************************************************
;; Messages
;;**********************************************************************
INITMSG
        DB      CR, LF, "** TMS70C02 Monitor Help Menu V", VERSMYR, ".", VERSMIN, ".", VERSPAT, " **", CR, LF, 0

HELPMSG
        DB      CR, LF, " Caaaa - Call subroutine at aaaa"
        DB      CR, LF, " D[||+|-|[aaaa[-bbbb]]] - Dump memory from aaaa to bbbb"
        DB      CR, LF, " E[e] - View/set echo"
        DB      CR, LF, " Faaaa eeee dd - Fill memory from aaaa to eeee with dd"
        DB      CR, LF, " Gaaaa - jump to address aaaa"
        DB      CR, LF, " Maaaa bb - Modify memory location"
        DB      CR, LF, " H - Help menu"
        DB      CR, LF, "*Raaaa eeee - RAM test from aaaa to eeee"
        DB      CR, LF, "*:ssaaaattdddddd....ddcc - receive Intel-hex record"
        DB      CR, LF, " * = not yet implemented", 0



