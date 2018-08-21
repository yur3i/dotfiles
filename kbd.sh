#!/bin/bash

id=$(xinput | grep -Eo "Dell Dell USB Keyboard\s+id=[0-9]{1,4}" | cut -c 45-)

if [ "$id" != "" ]
then
    setxkbmap -device $id -layout ru
fi
