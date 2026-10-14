-- An interface to RPI Linux SPI device intended to be callable from Ada.
-- Copied from an example on the RPi forum.
-- Mainly automativally generated interface to spi_driver.c

-- Author    : David Haley
-- Created   : 29/04/2022
-- Last Edit : 15/10/2026

-- 20261015: Comments updated, errors are now returned as -errno.
-- 20260525: Casing of package name and heading comments updated.  
-- 20251011: Check on word length added
-- 20220490: Comments corrected

pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

package SPI_Interface is

   subtype SPI_Devices is unsigned_char range 0 .. 1;
   subtype SPI_Modes is unsigned_char range 0 .. 3;
   subtype SPI_Speeds is unsigned_long range 3125 .. 125000000;
   
   function SPI_Open
     (SPI_Device : SPI_Devices;
      Mode : SPI_Modes;
      Speed : SPI_Speeds) return int  -- spi_driver.h:9
   with Import => True, 
        Convention => C, 
        External_Name => "SPI_Open";

  -- SPI_Device 0 .. 1
  -- Mode 0 .. 3
  -- Speed 3125 .. 125000000
  -- Must be called before any transfers, returns 0 if successful.
  -- Return values:
  --  0: Success
  -- -EINVAL: SPI_Device, Mode or Speed out of range
  -- -EBUSY: An SPI device is already open
  -- -EIO: Mode or word length did not read back as set
  -- Other negative values are -errno from open or ioctl. The device is closed
  -- if any part of the configuration fails.

   function SPI_Transfer
     (Tx_Buffer : access Unsigned_8;
      Rx_Buffer : access Unsigned_8;
      Length : unsigned_short) return int  -- spi_driver.h:24
   with Import => True, 
        Convention => C, 
        External_Name => "SPI_Transfer";

  -- Tx_Buffer and Rx_Buffer should be the same length and must be at least
  -- Length bytes long;
  -- Transfers Length bytes from the Tx_Buffer to the SPI interface and reads
  -- back Length bytes into the RxBuffer.
  -- Return values.
  -- 0: Success
  -- -EBADF: The SPI device is not open
  -- Other negative values are -errno from ioctl with second parameter
  -- SPI_IOC_MESSAGE(1)
  
   function SPI_Close return int  -- spi_driver.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "SPI_Close";

  -- Close the SPI device should be called before program termination.
  -- Return values:
  -- 0: Success;
  -- -EBADF: The SPI device is not open
  -- Other negative values are -errno from close.
  
end SPI_Interface;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
