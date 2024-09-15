; Display/Hex Keyboard monitor TMS70C02
;

; ADDRESSES
KEYDSPCOL       EQU     0140h   ; WR: Keyboard (0-3) & Display (0-5) column counter
KEYS            EQU     0140h   ; RD: Keyboard (0-3) row values
DSPPATT         EQU     0148h   ; WR: Display character pattern

; MASKS
ROWMASK         EQU     00001111b
LEDMASK         EQU     11110000b

KEYRCNT EQU     3
KEYROW  EQU     R77
KEYBUF EQU      R78
; R78-R83 is keyboard buffer. Last two not used. Fix this?
KEYBUFE EQU     R83
KeyBFSZ EQU     R83-R78+1
DSPBUF  EQU     R84
; R82-R87 is display buffer
DSPBUFE EQU     R89
DSPBFSZ EQU     R89-R84+1
DSPCNT  EQU     R90

T2MSBD  EQU     000h ; 000h ; 0FFh
T2LSBD  EQU     025h ; 025h ; 0FFh
T2PSD   EQU     01Fh
T2CL0D  EQU     10011111b

;;**********************************************************************
;; INT2INIT 3-41
;; timer period = clock * (prescalerValue + 1) * (timer value + 1)
;; 4.9152 MHz / 4 = 1228800 Hz -> 8.1380e-07s. 8.1380e-07 * 32 * 65536 = 1.71s
;; 4.9152 MHz / 4 = 1228800 Hz -> 8.1380e-07s. 8.1380e-07 * 32 * 37 = 0.001s
;;**********************************************************************
INT5INIT ; Timer 2 Data 3.7, 3-42
        MOVP    %T2MSBD, T2MDATA ; Load the actual T2MDATA register with initial MSB value
        MOVP    %T2LSBD, T2LDATA ; Load the actual T2LDATA register with initial LSB value
        
        ; Timer Output Function 3.7.9, 3-50
        MOVP    %01000000b, T2CTL1 ; bit 7=0; no cascade, bit 6=1; B0 toggle, bit 5-0; don't care
        ; Timer and Prescalar Operation 3.7.7, 3-48
        MOVP    %10011111b, T2CTL0 ; bit 7=1; reload & start, bit 6=0; internal clock, 
                                  ; bit 5=0; timer active when idle, bit 4-0; prescaler
        ; Interrupt Control 3.6.3, 3-32   3C=00111100
        MOVP    %00101010b, IOCNT0 ; bit 7-6=00; don' t care, 
                                  ; bit 5-4=10; INT3 cleared & disabled, 
                                  ; bit 3-2=10; INT2 cleared & disabled, 
                                  ; bit 1-0=10; INT1 cleared & disabled
        MOVP    %00001110b, IOCNT1 ; bit 3-0=1110; INT5 cleared & enabled, INT4 cleared & disabled
        MOVP    %00100010b, IOCNT2 ; bit 5-4=10: INT3 edge only, falling edge, 
                                  ; bit 1-0=10; INT1 edge only, falling edge

        RETS
        
DSPINIT ; Initialize display buffer
        MOV     %DSPBFSZ, B
        MOV     DSPSP, A
_DILOOP
        STA     DSPBUF(B)
        DEC     B
        JP      _DILOOP
        RETS
                
INT5    ; Timer/Counter 2
        ; Re-init Timer 2
;        MOVP    %10011111b, T2CTL0 ; set bit 7; reload & start INT5
;        XORP    %00000010b, PORTB       ; toggle B1 pin
;        MOVP    %'.', TXBUF    ; TXBUF=P26  A->TXBUF         
        ; Display update
; 
; Load value at DSPBUF[DSPCNT] into DSPPATT
; Increment DSPCNT
; If DSPCNT> DSPBUFSZ set DSPCNT to 0
        PUSH    A
        PUSH    B
        LDA     DSPSP
        MOVP    A, DSPPATT      ; 07h, KEYDSPCOL ; Select a non existing display
        MOV     DSPCNT, B       ; load display index pointer
        MOV     %DSPBUF, A      ; load start of display buffer
        INC     B               ; update pointer
        CMP     %DSPBFSZ, B     ; check for overflow B-%DSPBUFSZ
        JNC     _T2NOC          ; Jump when B <= DSPBUFSZ
        MOV     %0, B           ; Reset B  
_T2NOC
        LDA     DSPBUF(B)       ; Load the value from the current position in the buffer
        MOV     B, DSPCNT       ; Store new display index pointer
        MOVP    A, DSPPATT      ; place the pattern 
        MOVP    KEYS, A
        STA     KEYBUF(B)       ; get the keyboard row
        MOVP    B, KEYDSPCOL    ; select the display column

        POP     B
        POP     A
        RETI    ; INT5 end

;DSPMSG  DB      DSPT, DSPN, DSP5, DSPSP, DSP7, DSP0, DSPC, DSP0, DSP2, 0 ; tnS 70C02
DSPMSG   DB      16h,  13h,  05h,  10h,   07h,  00h,  0Ch,  00h,  02h,  0FFh
; DISPLAY
; Hex character patterns
;                  ---a
;                 |f  |b
;                  ---g
;                 |e  |c
;                  ---d .dp
;               .gfedcba
DSPCHR
DSP0    DB      00111111b ; 0
        DB      00000110b ; 1
DSP2    DB      01011011b ; 2
        DB      01001111b ; 3
        DB      01100110b ; 4
DSP5    DB      01101101b ; 5
        DB      01111101b ; 6
DSP7    DB      00000111b ; 7
        DB      01111111b ; 8
        DB      01101111b ; 9
        DB      01110111b ; A
        DB      01111100b ; b
        DB      00111001b ; C
        DB      01011110b ; d
        DB      01111001b ; E
        DB      01110001b ; F
DSPSP   DB      00000000b ;    10
DSPMN   DB      01000000b ; -  11
DSPH    DB      01110110b ; H  12
DSPN    DB      01010100b ; n  13
DSPP    DB      01110011b ; P  14
DSPR    DB      01010000b ; r  15
DSPT    DB      01111000b ; t  16
DSPU    DB      00011100b ; U  17
DSPY    DB      01110000b ; y
DSPUS   DB      00010000b ; _
DSPIS   DB      01001000b ; =
DSPEX   DB      10000110b ; !
DSPUP   DB      00100011b ; ^
