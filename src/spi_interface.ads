pragma Ada_2012;
pragma Style_Checks (Off);

with interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

package spi_interface is

  -- An interface to RPI Linux SPI device intended to be callable from Ada.
  -- Copied from an example on the RPi forum.
  -- Maimly automativally generated interface to SPI_Driber implemented in C.
  -- Author    : David Haley
  -- Created   : 29/04/2022
  -- Last Edit : 30/04/2022
  -- 20220430: Renamed spi_interface buffer type changed to Unsigned_8,
  -- suptype limits added

   subtype SPI_Devices is unsigned_char range 0 .. 1;
   subtype SPI_Modes is unsigned_char range 0 .. 3;
   subtype SPI_Speeds is unsigned_long range 3125 .. 125000000;
   
   function SPI_Open
     (SPI_Device : SPI_Devices;
      Mode : SPI_Modes;
      Speed : SPI_Speeds) return int  -- spi_driver.h:8
   with Import => True, 
        Convention => C, 
        External_Name => "SPI_Open";

  -- SPI_Device 0 .. 1
  -- Mode 0 .. 3
  -- Speed 3125 .. 125000000
  -- Must be called before any transfers, returns 0 if successful.
  -- Return values:
  --  0: Success
  -- -1: Unknown SPI device
  -- -2: Error opening SPI device
  -- -3: Unknown SPI mode requested
  -- -4: Error setting Mode
  -- -5: Bad speed requested
  
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
  -- Negative values are the error code returned from call to ioctl with second
  -- parameter SPI_IOC_MESSAGE(1)
  
   function SPI_Close return int  -- spi_driver.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "SPI_Close";

  -- Close  the SPI device should be called before program termination.
  -- Return values:
  -- 0: Success;
  -- Non zero values are the error code returned from call to close
  
end spi_interface;
