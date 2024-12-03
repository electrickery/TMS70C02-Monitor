; Display/Hex Keyboard monitor TMS70C02
;

; ADDRESSES in peripherial file. The 0140h-014Fh region 
KEYDSPCOL EQU    0140h   ; WR: Keyboard (0-3) & Display (0-5) column counter
KEYS     EQU     0140h   ; RD: Keyboard (0-3) row values
DSPPATT  EQU     0148h   ; WR: Display character pattern

; Keyboard buffer. Four bytes directly copied from KEYS for first four key/disp columns
KEYBUF  EQU     R80     ; 50h
; R78-R81 is keyboard buffer. 
KEYBUFE EQU     R83     ; 53h
KEYRCNT EQU     KEYBUFE-KEYBUF+1
KEYVAL  EQU     R84     ; 54h
KEYSHFT EQU     R85     ; 55h

DSPBUF  EQU     R88     ; 58h
; R82-R87 is display buffer
DSPBUFE EQU     R93     ; 5Dh
DSPBFSZ EQU     DSPBUFE-DSPBUF+1
DSPMOFS EQU     R94     ; 5Eh
DSPCNT  EQU     R95  ; points to currently active digit (and keyboard row), used by INT5 ; 5Fh

; Timer 1 data for blinking PB1 (for now). This timer triggers INT 2
T1MSBD  EQU     03Fh ; Timer 1 data MSB 1F00h ~ 0.2s
T1LSBD  EQU     000h ; Timer 1 data LSB
T1PSD   EQU     01Fh ; Timer 1 prescaler
; Timer 2 data for Display/Keyboard scan rate, ~ 1us. This timer triggers INT 5
T2MSBD  EQU     000h ; 000h ; 0FFh ; Timer 2 data MSB
T2LSBD  EQU     025h ; 025h ; 0FFh ; Timer 2 data LSB
T2PSD   EQU     01Fh ; Timer 2 prescaler

; Test routine
; Iterates over keyboard buffer, converts the key value to a display 
; pattern, and puts it in the display buffer, four left-most digits.
KEYTEST
    CLR     B
_KTNXT
    LDA     @KEYBUF(B)
    PUSH    B
    CALL    @KEY2HEX
    MOV     A, B
    LDA     @DSPCHR(B)  
    POP     B   
    MOV     A, DSPBUF(B)
    INC     B
    CMP     %KEYRCNT, B ; B - %KEYRCNT, carry set on negative; B: 1, 2, 3, 4
    JNC      _KTNXT

    RETS
    
KEY2NUM
        DB      0FFh, 00Fh ; not a value, just no key pressed
        DB      0FEh, 000h ; Key 0
        DB      0FDh, 001h ; Key 1
        DB      0FBh, 002h ; Key 2
        DB      0F7h, 003h ; Key 3
        DB      0EFh, 004h ; Key 4
        DB      0DFh, 005h ; Key 5
        DB      0BFh, 006h ; Key 6
        DB      07Fh, 007h ; Key 7
        DB      000h, 00Eh ; not a valid value, multiple bits clear
        
; Converts the buffered key values (KEYBUF) to hex patterns on the displays (DSPBUF)
; A pressed key changes one bit in a byte, the eight valid values fit on a single digit.
; The input KEYS value is placed in A. The output value, using the lower nibble
; recognised patterns: FFh, FEh, FDh, FBh, F7h, EFh, DFh, BFh, 7Fh. Others are multiple keys pressed
KEY2HEX
    CMPA    %0FFh
    JNZ      _K2H0
    MOV     %0Fh, A ; not a value, just no key pressed
    JMP     _K2HDN
_K2H0    
    CMPA     %0FEh
    JNZ      _K2H1
    MOV      %00h, A ; Key 0
    JMP     _K2HDN
_K2H1    
    CMPA     %0FDh
    JNZ      _K2H2
    MOV      %01h, A ; Key 1
    JMP     _K2HDN
_K2H2    
    CMPA     %0FBh
    JNZ      _K2H3
    MOV      %02h, A ; Key 2
    JMP     _K2HDN
_K2H3
    CMPA     %0F7h
    JNZ      _K2H4
    MOV      %03h, A ; Key 3
    JMP     _K2HDN
_K2H4
    CMPA     %0EFh
    JNZ      _K2H5
    MOV      %04h, A ; Key 4
    JMP      _K2HDN
_K2H5
    CMPA     %0DFh
    JNZ      _K2H6
    MOV      %05h, A ; Key 5
    JMP      _K2HDN
_K2H6
    CMPA     %0BFh
    JNZ      _K2H7
    MOV      %06h, A ; Key 6
    JMP      _K2HDN
_K2H7
    CMPA     %07Fh
    JNZ      _K2HER
    MOV      %07h, A ; Key 7
    JMP     _K2HDN
_K2HER
    MOV     %0Eh, A ; not a valid value, multiple bits clear

_K2HDN  ; Done    
    RETS

;;**********************************************************************
;; INT5INIT 3-41  - configures both INT2 and INT5
;; timer period = clock * (prescalerValue + 1) * (timer value + 1)
;; 4.9152 MHz / 4 = 1228800 Hz -> 8.1380e-07s. 8.1380e-07 * 32 * 65536 = 1.71s
;; 4.9152 MHz / 4 = 1228800 Hz -> 8.1380e-07s. 8.1380e-07 * 32 * 37 = 0.001s
;;**********************************************************************
INT2INIT ; INT2 is used for keyboard/display scanning via Trap 5
        ; Timer 1 Data 3.7, 3-38
        MOVP    %T1MSBD, T1MDATA ; Load the actual T1MDATA register with initial MSB value
        MOVP    %T1LSBD, T1LDATA ; Load the actual T1LDATA register with initial LSB value

        ; Timer 1 Output Function 3.7.9, 3-50
        MOVP    %01000000b, T1CTL1 ; bit 7=0; no cascade, bit 6=1; B1 toggle, bit 5-0; don't care
        ; Timer 1 and Prescalar Operation 3.7.7, 3-48
        MOVP    %10000000b | T1PSD, T1CTL0 ; bit 7=1; reload & start, bit 6=0; internal clock, 
                                   ; bit 5=0; timer active when idle, bit 4-0; prescaler
        RETS
INT5INIT
        ; Timer 2 Data 3.7, 3-42
        MOVP    %T2MSBD, T2MDATA ; Load the actual T2MDATA register with initial MSB value
        MOVP    %T2LSBD, T2LDATA ; Load the actual T2LDATA register with initial LSB value
        
        ; Timer Output Function 3.7.9, 3-50
        MOVP    %00000000b, T2CTL1 ; bit 7=0; no cascade, bit 6=0; no B0 toggle, bit 5-0; don't care
        ; Timer and Prescalar Operation 3.7.7, 3-48
        MOVP    %10000000b | T2PSD, T2CTL0 ; bit 7=1; reload & start, bit 6=0; internal clock, 
                                   ; bit 5=0; timer active when idle, bit 4-0; prescaler
        MOV     %0FFh, KEYVAL
        RETS
IOCTLINIT
        ; Interrupt Control 3.6.3, 3-32
        MOVP    %00101010b, IOCNT0 ; bit 7-6=00; don' t care, 
                                   ; bit 5-4=10; INT3 cleared & disabled, 
                                   ; bit 3-2=11; INT2 cleared & enabled, 
;                                   ; bit 3-2=10; INT2 cleared & disabled, 
                                   ; bit 1-0=10; INT1 cleared & disabled.
        MOVP    %00001110b, IOCNT1 ; bit 3-2=11; INT5 cleared & enabled, 
;        MOVP    %00001010b, IOCNT1 ; bit 3-2=11; INT5 cleared & disabled, 
                                   ; bit 1-0=10; INT4 cleared & disabled.
        MOVP    %00100010b, IOCNT2 ; bit 5-4=10: INT3 edge only, falling edge, 
                                   ; bit 1-0=10; INT1 edge only, falling edge.
        RETS                       ; Interrupts enabled in main routine
        
DSPINIT ; Fill display buffer
        CLR     DSPMOFS ; intital value startup message, also no h-scroll flag
        CALL    @DSPFIL
        MOV     %0, I2REG       ; Initiate the message shifting
        CLR     B
        CLR     A
        STA     @KEYBUF(B)
        INC     B
        STA     @KEYBUF(B)
        INC     B
        STA     @KEYBUF(B)
        INC     B
        STA     @KEYBUF(B)
        INC     B
        RETS
        
DSPFIL
        CLR     B     
_DFLOOP
        LDA     @DSPMSG(B) ; Message index
        PUSH    B
        MOV     A, B
        LDA     @DSPCHR(B) ; Character pattern
        POP     B
        STA     @DSPBUF(B)
        INC     B
        CMP     %DSPBFSZ, B ; B - %DSPBFSZ. Loops for  B = 1, 2, 3, 4, 5, 6
        JNC     _DFLOOP

        RETS
        
;IDSPFIL

DSPCLR
        CLR     B     
_DCLOOP
        MOV     DSPSP, A
        STA     @DSPBUF(B)
        INC     B
        CMP     %DSPBFSZ, B ; B - %DSPBFSZ. Loops for  B = 1, 2, 3, 4, 5, 6
        JNC     _DCLOOP

        RETS

INT2    ; INT 2 / Timer/Counter 1 ; INT2 
        PUSH    A
        PUSH    B
        
        POP     B
        POP     A
        RETI
                
INT5    ; INT 5 / Timer/Counter 2 is used for keyboard/display scanning
; 
; Display routine, lights one hex digit per interrupt cycle
        PUSH    A
        PUSH    B
        ; Clear pattern buffer, preventing bleeding
        LDA     DSPSP
        MOVP    A, DSPPATT      ; all segments off by displaying a space
        MOV     DSPCNT, B       ; fetch current display index 
        ; Get next pattern from buffer
        MOV     %DSPBUF, A      ; load start of display buffer
        INC     B               ; update pointer to next LED display
        CMP     %DSPBFSZ, B     ; check for display overflow B-%DSPBUFSZ
        JNC     _T5NOC          ; Jump when B <= DSPBUFSZ
        CLR     B               ; Reset B 
_T5NOC ; 
        LDA     DSPBUF(B)       ; Load the value from the current position in the buffer
        MOV     B, DSPCNT       ; Store new display index pointer
        MOVP    B, KEYDSPCOL    ; select the display column
        MOVP    A, DSPPATT      ; place the pattern in LED segment register
        CMP     %KEYRCNT, B     ; check for key row overflow B-%KEYRCNT
        JC      _T5KDONE         ; Skip next when all 4 keyboard rows are done
        MOVP    KEYS, A         ; 
        STA     KEYBUF(B)       ; store keys value in keyboard row
_T5KDONE ; 
        POP     B
        POP     A
        RETI    ; INT5 end
        
        
KEYS2KEY        ; Input: column in B. Key pattern in A. Output: KEYVAL
        PUSH    B       ; save column
        MOV     #7, B
_K2K1   ; Loop through the bits, sending them to the C-flag
        RLC     A
        JC      _K2KFND ; a set carry bit means the key was pressed
        DEC     B
        JNZ     _K2K1
        MOV     %0FFh, A  ; flags no key found
        JMP     _K2KDN
_K2KFND ; Key found
        POP     A       ; get column
        RL      A
        RL      A
        OR      %0F8h, A
        AND     B, A
        MOV     A, KEYVAL       ; KEYVAL is the combination of the key bit 
                                ; (extracted from KEYBUF)
                                ; and keyboard column (KEYDSPCOL) 
                                ; bits 0-2 are the key, bits 3-4 the column
        
_K2KDN  ; KEYS2KEY done
;        RETS
        
VAL2PAT
        ; Input value in B (5 bits, key 00-1F), display index in A (0 - 5)
        PUSH    A
        LDA     DSPCHR(B)
        POP     B
        STA     DSPBUF(B)
        RETS


; This table doesn't contain patterns, but pointers to the patterns in DSPCHR
; It allows simple conversion of numeric nibbles (values 0-F) to hex-character patterns:
;  MOV n, B ; LDA DSPCHR(B) ; MOV p, B ; STA DSPBUF(B) ; (n is the nibble, p is the position on DSPBUF)
;DSPMSG  DB      DSPT, DSPN, DSP5, DSPSP, DSP7, DSP0, DSPC, DSP0, DSP2, 0 ; tnS 70C02
;DSPMSG   DB      16h,  13h,  05h,  10h,   07h,  00h,  0Ch,  00h,  02h,  0FFh ; space version
;DSPMSG   DB      16h,  13h,  05h,  07h,  00h,  0Ch,  00h,  02h,  0FFh ; no space version
DSPMSG   DB      16h,  07h,  00h,  0Ch,  00h,  02h,  0FFh ; short version
; DISPLAY
; Hex character patterns
;                  ---a
;                 |f  |b
;                  ---g
;                 |e  |c
;                  ---d .dp
;               .gfedcba
DSPCHR
DSP0    DB      00111111b ; 0      3Fh
        DB      00000110b ; 1      06h
DSP2    DB      01011011b ; 2      5bh
        DB      01001111b ; 3      4Fh
        DB      01100110b ; 4      66h
DSP5    DB      01101101b ; 5      6Dh
        DB      01111101b ; 6      7Dh
DSP7    DB      00000111b ; 7      07h
        DB      01111111b ; 8      7Fh
        DB      01101111b ; 9      6Fh
        DB      01110111b ; A      77h
        DB      01111100b ; b      7Ch
        DB      00111001b ; C      39h
        DB      01011110b ; d      5Eh
        DB      01111001b ; E      79h
        DB      01110001b ; F      71h
DSPSP   DB      00000000b ;    10
DSPMN   DB      01000000b ; -  11  40h
DSPH    DB      01110110b ; H  12
DSPN    DB      01010100b ; n  13  54h
DSPP    DB      01110011b ; P  14
DSPR    DB      01010000b ; r  15
DSPT    DB      01111000b ; t  16
DSPU    DB      00011100b ; U  17
DSPY    DB      01110000b ; y
DSPUS   DB      00010000b ; _
DSPIS   DB      01001000b ; =
DSPEX   DB      10000110b ; !
DSPUP   DB      00100011b ; ^
