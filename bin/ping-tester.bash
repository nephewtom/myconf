#!/bin/bash
while true; do
    #     *** DATE: Thu Sep 17 10:17:50 CEST 2015
    echo -e "\n*** DATE:" `date`
    echo "---------------------------------------"
    ping -c5 $1;
    echo "###"
    sleep 1
done
