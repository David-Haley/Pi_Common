# Pi_Common
This repository contains a number of Ada packages that allow basic access to the RPi GPIO I2C and SPI.

## RPI_GPIO Package
Allows basic operations on the the Raspberry Pi GPIO pins as both parallel inpit and output.

## I2C
The file i2c_interface.ads provides a binding for the I2C driver in the PI_Common_C repository. The package DFR0555_Display provides an example of how to use the I2C interface.

## SPI
The file spi_interface.ads provides a binding for the SPI driver in the PI_Common_C repository. The package AD7091R2 provides an example of how to use the SPI interface. The program Test_SPI provides a simple loop back test when SPI_MOSI is connected to SPI_MISO with a jumper wite.

## AD7091R2 Package.
Provides for setup and reading of data from the Analogue Devices AD7091R2 two channel 12 bit A to D. It could be extended to work with the for and eight channel versions of this IC.

This package uses the SPI interface and requires hardware (monostable etc) to reduce the read pulse width from the minimum pulse width available through the file system GPIO to the 500 ns required for the ADC.

## TIC5940 Package
Is a device driver for Texas Instruments TIC5940 sixteen channel PWM LED driver. Can be used to operate multiple cascaded ICs to drive large nimbers of LEDs. Provides 4096 levels of PWM brightness control and 63 levels of analogue current setting to match individual LED brightness.

Additional hardware is needed to clock the TIC5940 and service the Blank and XLATE inputs on each display cycle, note the speed required is too high for the RPI to do directly and maintain stsble brightness.

The package can also drive two seven segment displays including the decimal points. Arbitart mapping of segments is provided potentially symplifying the PCB layout.

## ANSI_Console Package
Provides basic ANSI terminal cursor positioning and setting of text colour etc.

## Linux_Signals Package
This package provides for the interception of the SIGTERM signal and ctrl_c to allow an orderly shutdown of a program when these requests are received. This is useful to allow a program to be run as a systend service.

## DFR0555_Display Package
Provides comprehensive control over the DFRobot sixteen character two line display (LCD1602 Module). Based on the PCA9633R2 LED back light driver and API31086L LCD driver.

The back light can be turned on, off and have its brightness controlled.

The cursor can be positioned, be visible or hidden. writing of full lines, single characters or strings is provided.
