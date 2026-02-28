#!/bin/bash
# script to release all GPIO pins
echo 17 >/sys/class/gpio/unexport
echo 18 >/sys/class/gpio/unexport
echo 27 >/sys/class/gpio/unexport
echo 22 >/sys/class/gpio/unexport
echo 23 >/sys/class/gpio/unexport
echo 24 >/sys/class/gpio/unexport
echo 25 >/sys/class/gpio/unexport
