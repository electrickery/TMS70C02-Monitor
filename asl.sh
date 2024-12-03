#!/bin/bash
#

FILE=$1
FILEBASE=${FILE%%.*}
PATH=/mnt/M/Programming/c/asl-current
ASM=$PATH/asl
P2BIN=$PATH/p2bin
P2HEX=$PATH/p2hex
TARGET=TMS70C02
RM=/usr/bin/rm

$RM -f $FILEBASE.p $FILEBASE.lst $FILEBASE.bin $FILEBASE.hex
$ASM -cpu TMS70C02 $FILEBASE.asm -o $FILEBASE.p -L -olist $FILEBASE.lst -v -C

if [ $? = 0 ]
then
    $P2BIN $FILEBASE.p $FILEBASE.bin
    $P2HEX $FILEBASE.p $FILEBASE.hex
else
    echo Error!
fi
