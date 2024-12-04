;MonitorCommands

;;**********************************************************************
;  Command routines for the TMS70C02 serial monitor 
;;**********************************************************************  

;;**********************************************************************
; ***** Dump command
;;**********************************************************************
CMD_DUMP
        CALL    @COLLECT
        JC      _CDABORT        ; check for carry = ESC pressed
        CALL    @NEWLINE
        CALL    @NEWDUMP
_CDABORT
        RETS
        
NEWDUMP
; variants: no args     ; use previous end address + 1, dump 256 bytes
;           hhhh        ; use as start address, dump 256 bytes
;           hhhh-hhhh   ; use as start & end address
;           +           ; same as no args
;           -           ; use previous address - 255, dump 256 bytes
        
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; no arg
        JZ      _ND_NOARG
        
 
        CMP     #1, B           ; one char
        JZ      _ND_1CHR
       
        CMP     #4, B           ; one address
        JZ      _ND_1ADR
        
        CMP     #9, B           ; two addresses
        JZ      _ND_2ADR   
        JMP     _ND_ERR
        
; One address argument 
_ND_1CHR
        CMP     #"+", CLBUF 
        JZ      _ND_PLUS
        
        CMP     #'-', CLBUF
        JZ      _ND_MIN
        JMP     _ND_ERR

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

; One address argument
_ND_1ADR
        CALL    @FIRSTADR

        CALL    @DUMP256
        JMP     _ND_END

; Two address argument
_ND_2ADR
        CALL    @FIRSTADR
        CALL    @SECONADR
        
        CALL    @DUMPVAR
        JMP     _ND_END

; Error message output
_ND_ERR
        MOVD    #NDERMSG, MSGPTR
        CALL    @OUTSTR
_ND_END
        RETS
        
NDERMSG 
        DB      " -- Incorrect format. Should be: +|-|aaaa|aaaa bbbb --", CR, LF, 0

; E.M. Klaus code modified & annotated
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
_D16HLP ; Dump16 Hex Loop
        LDA    *ADDR3         ; Get Data byte
        CALL    OUTHEX        ; Print as HEX
        MOV     #" ", A        
        DEC     B
        CMP     #8, B         ; On 8th byte print two spaces
        JNZ     _D16HDN
        CALL    OUTCHR
_D16HDN  ; Dump16 Hex Loop Done
        CALL    OUTCHR          ; Dump16 Hex loop Done
        CALL    INCADD3       ; Inc ADDR3
        TSTB                ; Done?
        JNZ     _D16HLP       ; Do next byte
        
        CALL    OUTCHR        ; print 2 spaces
        CALL    OUTCHR
        MOVD    ADDR1, ADDR3  ; Move ADDR1 To Temp RP
        MOV     #16, B        ; Set Byte count
        
                            ; ** DUMP 16 Data Bytes in ASCII ***
_D16ALP   ; Dump16 ASCII Loop
        LDA     *ADDR3         ; Get Data byte
        CMP     #" ", A       ; Less than blank? 
        JPZ     _DUMP165        ; No dot
        MOV     #".", A
_DUMP165 CMP    #07Fh, A      ; Greater than `~`  
        JL      _DUMP166       ; No dot
        MOV     #".", A
_DUMP166 
        CALL   OUTCHR        ; print it (or the .)
        CALL    @INCADD3       ; Inc ADDR3
        DEC     B
        JNZ     _D16ALP       ; Do next byte
        
_DUMPEND
        MOVD    ADDR3, ADDR1    ; update address for next dump
        RETS

;;**********************************************************************
;; DUMP256                                                              
;; Call with start address in ADDR1
;;**********************************************************************
DUMP256 MOV   #16, COUNT1
_DMP2561 CALL  DUMP16
        CALL    @NEWLINE
        DEC   COUNT1
        JZ    _DMP256X
        CLRC

        JMP   _DMP2561
_DMP256X RETS

;;**********************************************************************
;; DUMPVAR
;;
;;**********************************************************************
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
; ***** Read / modify echo mode  (not used yet)
;;**********************************************************************
CMD_ECHO
        CALL    @COLLECT
        JC      _CE_ABORT
        CALL    @NEWLINE
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 

        CMP     #1, B
        JZ      _CE_1CHAR
        

_CE_1CHAR
        MOV     CLBUF,  A
        CMP     #"?", A
        JZ      _CE_QUERY
        CMP     #"0", A
        JZ      _CE_SET
        CMP     #"1", A
        JZ     _CE_SET
        JMP    _CE_ERR
        
_CE_QUERY       ; E?
        MOVD    #CEPFMSG, MSGPTR
        CALL    @OUTSTR
        MOV     SYSFLGS, A
        AND     #01h, A
        ADD     #"0", A
        CALL    @OUTCHR

        JMP     _CE_END

_CE_SET         ; E0 or E1
        SUB     "0", A
        AND     #0FEh,  SYSFLGS
        OR      A, SYSFLGS
        JMP     _CE_QUERY       ; output the new state after change

;
_CE_ERR
        MOVD    #CEERMSG, MSGPTR
        CALL    @OUTSTR
        
_CE_ABORT        
_CE_END
        RETS

CEERMSG DB      " -- Incorrect format. Should be: ?|0|1 --", CR, LF, 0
CEPFMSG DB      "Echo = ", 0
        
;;**********************************************************************
; ***** H, h, ? Help text
;;**********************************************************************
CMD_HELP    
        MOVD    #INITMSG, MSGPTR
        CALL    @OUTSTR
        MOVD    #HELPMSG, MSGPTR
        CALL    @OUTSTR
        RETS

;;**********************************************************************
;; Help messages
;;**********************************************************************
HELPMSG
        DB      CR, LF, " Cssss - Call subroutine at ssss"
        DB      CR, LF, " D[||+|-|ssss[-eeee]] - Dump memory from ssss to eeee"
        DB      CR, LF, " E[e] - View/set echo"
        DB      CR, LF, " Fssss eeee dd - Fill memory from ssss to eeee-1 with dd"
        DB      CR, LF, " Gssss - jump to address aaaa"
        DB      CR, LF, " Mssss dd - Modify memory location ssss to dd"
        DB      CR, LF, " H - Help menu"
        DB      CR, LF, " Rssss eeee - RAM test from ssss to eeee"
        DB      CR, LF, " Vssss eeee nnnn - Copy memory range ssss to eeee to nnnn"
        DB      CR, LF, "*:llssssttdddddd....ddcc - receive Intel-hex record"
        DB      CR, LF, " * = not yet implemented", 0

;;**********************************************************************
; ***** Maaaa dd - Modify memory
;;**********************************************************************
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
        CALL    @GETDATA
        MOV     A, DATA
        MOV     #" ", A
        CALL    @OUTCHR
        MOV     DATA, A
;        CALL    @OUTHEX
        STA     *ADDR1
        RETS

CMERMSG DB      " -- Incorrect format. Should be: 'aaaa dd'--", CR, LF, 0


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
        
        JMP     _CFDONE
        
_CF_ERR        
        MOVD    #FILER1MSG, MSGPTR
        CALL    @OUTSTR

_CFABORT
_CFDONE
        RETS
        
FILER1MSG
        DB      " -- Incorrect format. Should be: 'Fssss eeee dd'", CR, LF, 0

;;**********************************************************************
;; FILLER
;;**********************************************************************
FILLER
        CALL    @FIRSTADR
        CALL    @SECONADR
        MOV     #0Ah, B
        CALL    @GETDATA
        MOV     A, DATA
        CMP     ADDR1-1, ADDR2-1        ; end must be greater than start
        JN      _FILERR
        
        MOVD    ADDR1, ADDR3
        MOV     DATA, A
_FILLOOP
        STA     *ADDR3
        INC     ADDR3                   ; LSB
        JNC     _FILNC
        INC     ADDR3-1                 ; MSB
_FILNC
        CMP     ADDR2, ADDR3        ; MSB pointer check
        JZ      _FLCHK
        JMP     _FILLOOP
_FLCHK        
        CMP     ADDR2-1, ADDR3-1            ; LSB pointer check
        JZ      _FILDONE

        JMP     _FILLOOP
        
_FILERR        
        MOVD    #FILER2MSG, MSGPTR
        CALL    @OUTSTR

_FILDONE
        RETS
        
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
; RAM test command from ssss to eeee
;;**********************************************************************
; - fill with 00h
; - check for 00h
; - for each location:
;   - check for 00h
;   - write 55h
;   - check for 55h
; - for each location:
;   - check for 55h
;   - write 0AAh
;   - check for 0AAh
; - fill with 0FFh
; - check for 0FFh

; TODO: refactor thi extremely repetitive routine below!
CMD_RAMT
        CALL    @COLLECT
        JC      _CRABORT        ; check for carry = ESC pressed
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 

        CMP     #9, B           ; ssss eeee
        JNZ     _CR_ERR
        
        CALL    @NEWLINE   
        CALL    @RAMTEST
        
        JMP     _CRDONE
        
_CR_ERR
        MOVD    #RTER1MSG, MSGPTR
        CALL    @OUTSTR
_CRABORT
_CRDONE
        RETS
        
RAMTEST
        CALL    @FIRSTADR
        CALL    @SECONADR
        MOVD    #RTSTRTMSG, MSGPTR      ; "Starting four phase RAM test."
        CALL    @OUTSTR
        
        ; first phase, fill with 00h
        MOVD    ADDR1, ADDR3
        MOV     #00h, A
        MOV     #"1", COUNT1
_RT1LOOP
        STA     *ADDR3 
        CMPA    *ADDR3
        JNZ     _RT12ER1
        INC     ADDR3
        JNC     _RT1NC
        INC     ADDR3-1
_RT1NC
        CMP     ADDR2-1, ADDR3-1        ; MSB pointer check
        JZ      _RT1CHK
        JMP     _RT1LOOP
_RT1CHK        
        CMP     ADDR2, ADDR3            ; LSB pointer check
        JZ      _RT1DONE
        JMP     _RT1LOOP
        
_RT1DONE
        MOVD    #RT1DONMSG, MSGPTR
        CALL    @OUTSTR
        JMP     _RT2PHASE
        
_RT12ER1         ; " data not 00h error at "
        MOV     COUNT1, A
        CALL    @OUTCHR
        MOV     #" ", A
        CALL    @OUTCHR
        MOVD    #RT2ERMSG, MSGPTR
        CALL    @OUTSTR
        MOV     ADDR3-1, A
        CALL    @OUTHEX
        MOV     ADDR3, A
        CALL    @OUTHEX
        BR     _RTDONE

_RT2PHASE
        ; second phase
        MOVD    ADDR1, ADDR3
        MOV     #"2", COUNT1
_RT2LOOP
        CLR     A
        CMPA    *ADDR3                  ; check each byte for initial 00h
        JNZ     _RT12ER1
        MOV     #055h, A
        STA     *ADDR3                  ; change byte to 055h
        CMPA    *ADDR3                  ; check new value
        JNZ     _RT23ER2
        INC     ADDR3
        JNC     _RT2NC
        INC     ADDR3-1
_RT2NC
        CMP     ADDR2-1, ADDR3-1        ; MSB pointer check
        JZ      _RT2CHK
        JMP     _RT2LOOP
_RT2CHK        
        CMP     ADDR2, ADDR3            ; LSB pointer check
        JZ      _RT2DONE
        JMP     _RT2LOOP
_RT2DONE 
        MOVD    #RT2DONMSG, MSGPTR
        CALL    @OUTSTR
       
        ; third phase
        MOVD    ADDR1, ADDR3
        MOV     #"3", COUNT1
_RT3LOOP
        MOV     #055h, A
        CMPA    *ADDR3                  ; check each byte for initial 055h
        JNZ     _RT23ER2                ;  " data not 55h error at "
        MOV     #0AAh, A
        STA     *ADDR3                  ; change byte to 0AAh
        CMPA    *ADDR3                  ; check new value
        JNZ     _RT34ER1                 ; " data not 0AAh error at "
        INC     ADDR3
        JNC     _RT3NC
        INC     ADDR3-1
_RT3NC
        CMP     ADDR2-1, ADDR3-1        ; MSB pointer check
        JZ      _RT3CHK
        JMP     _RT3LOOP
_RT3CHK        
        CMP     ADDR2, ADDR3            ; LSB pointer check
        JZ      _RT3DONE
        JMP     _RT3LOOP
        
_RT23ER2       ;  " data not 55h error at "
        MOV     COUNT1, A
        CALL    @OUTCHR
        MOV     #" ", A
        CALL    @OUTCHR
        MOVD    #RT23ERMSG, MSGPTR
        CALL    @OUTSTR
        MOV     ADDR3-1, A
        CALL    @OUTHEX
        MOV     ADDR3, A
        CALL    @OUTHEX
        JMP     _RTDONE
        
_RT3DONE
        MOVD    #RT3DONMSG, MSGPTR
        CALL    @OUTSTR
        
        ; fourth phase fill with 0FFh
        MOVD    ADDR1, ADDR3
        MOV     #"4", COUNT1
_RT4LOOP
        MOV     #0AAh, A
        CMPA    *ADDR3                  ; check each byte for initial 0AAh
        JNZ     _RT34ER1                ;  " data not AAh error at "
        MOV     #0FFh, A
        STA     *ADDR3                  ; change byte to 0FFh
        CMPA    *ADDR3                  ; check new value
        JNZ     _RT4ER1                 ; " data not 0FFh error at "
        INC     ADDR3
        JNC     _RT4NC
        INC     ADDR3-1
_RT4NC
        CMP     ADDR2-1, ADDR3-1        ; MSB pointer check
        JZ      _RT4CHK
        JMP     _RT4LOOP
_RT4CHK        
        CMP     ADDR2, ADDR3            ; LSB pointer check
        JZ      _RT4DONE
        JMP     _RT4LOOP
_RT4DONE
        MOVD    #RT4DONMSG, MSGPTR
        CALL    @OUTSTR
               
_RTDONE        
        RETS

_RT34ER1       ;  " data not AAh error at "
        MOV     COUNT1, A
        CALL    @OUTCHR
        MOV     #" ", A
        CALL    @OUTCHR
        MOVD    #RT34ERMSG, MSGPTR
        CALL    @OUTSTR
        MOV     ADDR3-1, A
        CALL    @OUTHEX
        MOV     ADDR3, A
        CALL    @OUTHEX
        JMP     _RTDONE
        
_RT4ER1       ;  " data not FFh error at "
        MOVD    #RT4ERMSG, MSGPTR
        CALL    @OUTSTR
        MOV     ADDR3-1, A
        CALL    @OUTHEX
        MOV     ADDR3, A
        CALL    @OUTHEX
        JMP     _RTDONE

RTER1MSG
        DB      " -- Incorrect format. Should be: 'Rssss eeee'", CR, LF, 0        
RTSTRTMSG
        DB      "Starting four phase RAM test.", CR, LF, 0
RT1DONMSG
        DB      "Phase 1, ?? > 000h check done.", CR, LF, 0
RT2DONMSG
        DB      "Phase 2, 000h > 055h check done.", CR, LF, 0
RT3DONMSG
        DB      "Phase 3, 055h > 0AAh check done.", CR, LF, 0
RT4DONMSG        
        DB      "Phase 4, 0AAh > 0FFh check done.", CR, LF, 0
RT2ERMSG
        DB      " data not 000h error at ", 0
RT23ERMSG
        DB      " data not 055h error at ", 0
RT34ERMSG
        DB      " data not 0AAh error at ", 0
RT4ERMSG
        DB      " data not 0FFh error at ", 0
        
;;**********************************************************************        
;  CMD_HXINT - download Hex-Intel files. To be implemented.
;  :ssaaaattdddddd..ddcc. Note buffer is 44 bytes large, just enough for
;  a 16 Byte record, like: ':10EB30000717A4F906A3DF05A24E14A21515A2C05B'
;;**********************************************************************
CMD_HXINT
        CALL    @COLLECT
        CALL    @NEWLINE
        
        MOVD    #CLBUF, MSGPTR
        CALL    @OUTSTR
        RETS

;;**********************************************************************        
;  CMD_COPY Vssss eeee nnnn
;;**********************************************************************
CMD_COPY
        CALL    @COLLECT
        JC      _VP_ABORT        ; check for carry = ESC pressed
        MOV     CLBUFP, B       ; find command line length
        SUB     #CLBUF, B       ; 

        CMP     #14, B           ; ssss eeee
        JNZ     _VP_ERR
        
        CALL    @FIRSTADR
        CALL    @SECONADR
        CALL    @THIRDADR
        
        CMP     ADDR1-1, ADDR2-1        ; end must be greater than start
        JN      _VP_ERR
        
        MOVD    ADDR1, ADDR3
        
        MOVD    ADDR1, R225
        MOVD    ADDR2, R227
        MOVD    ADDR3, R229
        MOVD    ADDR4, R231

_VP_LOOP
        LDA     *ADDR3
        STA     *ADDR4
        INC     ADDR3
        INC     ADDR4
        JNC      _VP_NCS
        INC     ADDR3-1
_VP_NCS
        JNC      _VP_NCD
        INC     ADDR4-1
_VP_NCD        
        
        CMP     ADDR2-1, ADDR3-1            ; LSB pointer check
        JNZ      _VP_LOOP
        CMP     ADDR2, ADDR3                ; MSB pointer loop
        JNZ     _VP_LOOP

        JMP     _VP_DONE
    
_VP_ERR
        MOVD    #VPERMSG, MSGPTR
        CALL    @OUTSTR

_VP_ABORT
_VP_DONE
        RETS

VPERMSG DB      " -- Incorrect format. Must be 'Vssss eeee nnnn' and ssss < eeee.", CR, LF, 0

;;**********************************************************************
;; T ? - Test command to check command line to binary conversions
;;       Not part of the functional set of commands
;;**********************************************************************
CMD_TEST
        CALL    @COLLECT
        CALL    @NEWLINE
        
        MOV     #0, B
        CALL    @FIRSTADR
        MOV     #5, B
        CALL    @GETDATA
        MOV     A, DATA
        
        CALL    @NEWLINE
        
        CALL    @OUT1STAD
        MOV     #" ", A
        CALL    @OUTCHR
        
        MOVD    ADDR1, R241

        CALL    @OUTDATA
        CALL    @NEWLINE
        RETS

;;**********************************************************************
;; K
;;**********************************************************************
CMD_KEYT
        CALL    @DSPCLR
_CKLOOP                         ; wait loop until char is received
        CALL    @KEYTEST
        CALL    @CHKKEY

        JZ      _CKLOOP
        CALL    @DSPFIL         ; restore display buffer
        RETS


