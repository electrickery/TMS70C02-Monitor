;
;*------------------------------------------------- 
;* Peripheral Register Definition TMS70C02
;* TMS7000_Family_Data_Manual_1989, SPND001C, 3-14
;*------------------------------------------------- 
P0      EQU     0100h
P1      EQU     0101h
P2      EQU     0102h
P3      EQU     0103h
P4      EQU     0104h
P5      EQU     0105h
P6      EQU     0106h
P7      EQU     0107h
P8      EQU     0108h
P9      EQU     0109h
P10     EQU     010Ah
P11     EQU     010Bh
P12     EQU     010Ch
P13     EQU     010Dh
P14     EQU     010Eh
P15     EQU     010Fh
P16     EQU     0110h
P17     EQU     0111h
P18     EQU     0112h
P19     EQU     0113h
P20     EQU     0114h
P21     EQU     0115h
P22     EQU     0116h
P23     EQU     0117h
P24     EQU     0118h
P25     EQU     0119h
P26     EQU     011Ah

IOCNT0  EQU     P0              ; Interrupts and mode control 
IOCNT2  EQU     P1
IOCNT1  EQU     P2
; P3 reserved
PORTA   EQU     P4              ; Port A - pin 5 UART input 
DDRA    EQU     P5              ; Port A direction 
PORTB   EQU     P6              ; Port B - low nibble only, pin 3 UART output 
; P7 reserved
PORTC   EQU     P8              ; Port C - port data value
DDRC    EQU     P9              ; Port C - direction
PORTD   EQU     P10             ; Port D - port data value
DDRD    EQU     P11             ; Port D - direction
T1MDATA EQU     P12             ; Timer 1 MSB reload/readout
T1LDATA EQU     P13             ; Timer 1 LSB reload/readout
T1CTL1  EQU     P14             ; Timer 1 Control 1
T1CTL0  EQU     P15             ; Timer 1 Control 0
T2MDATA EQU     P16             ; Timer 2 MSB reload/readout
T2LDATA EQU     P17             ; Timer 2 LSB reload/readout
T2CTL1  EQU     P18             ; Timer 2 Control 1
T2CTL0  EQU     P19             ; Timer 2 Control 0
SMODE   EQU     P20             ; Serial port mode control
SCTL0   EQU     P21             ; Serial port control 0
SSTAT   EQU     P22             ; Serial port status
T3DATA  EQU     P23             ; Timer 3 reload decrementer value
SCTL1   EQU     P24             ; Serial port control 1
RXBUF   EQU     P25             ; Receive buffer
TXBUF   EQU     P26             ; Transmitter buffer

