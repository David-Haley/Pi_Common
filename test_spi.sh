#!/bin/bash
# script to build test_spi
gcc -c -g ../Pi_Common_C/src/spi_driver.c -o obj/spi_driver.o
gnatmake -g -D obj src/test_spi.adb -largs obj/spi_driver.o
