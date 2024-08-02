#!/bin/bash
#

FILE=$1
ASM=/mnt/M/Programming/c/asl-current/asl
P2BIN=/mnt/M/Programming/c/asl-current/p2bin
P2HEX=/mnt/M/Programming/c/asl-current/p2hex
FILEBASE=${FILE%%.*}
TARGET=TMS70C02

rm -f $FILEBASE.p $FILEBASE.lst $FILEBASE.bin $FILEBASE.hex
$ASM -cpu TMS70C02 $FILEBASE.asm -o $FILEBASE.p -L -olist $FILEBASE.lst -v -C

if [ $? = 0 ]
then
    $P2BIN $FILEBASE.p $FILEBASE.bin
    $P2HEX $FILEBASE.p $FILEBASE.hex
else
    echo Error!
fi
