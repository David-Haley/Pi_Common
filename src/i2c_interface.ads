pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

  -- An interface to RPI Linux i2c device intended to be callable from Ada.
  -- Based on generic example code
  -- raspberry-projects.com/pi/programming-in-c/i2c/using-the-i2c-interface.
  -- Author    : David Haley
  -- Created   : 03/03/2023
  -- Last Edit : 03/03/2023
  -- Name changed to I2C_Interface, subtypes I2C_Devices and IC_Addresses added.

package I2C_Interface is

   subtype I2C_Devices is unsigned_char range 0 .. 1;
   subtype IC_Addresses is int range 16#08# .. 16#77#;
   

   function I2C_Open (I2C_Device : I2C_Devices) return int  -- i2c_driver.h:9
   with Import => True, 
        Convention => C, 
        External_Name => "I2C_Open";

  -- I2C_Device 0 .. 1
  -- Returns 0 if successful, otherwise the error code from open
  
   function Set_IC_Address (IC_Address : IC_Addresses) return int
   with Import => True, 
        Convention => C, 
        External_Name => "Set_IC_Address"; -- i2c_driver.h:13

  -- Sets the IC_Address to be used for subsequent read and write operations.
  -- only 7 bit addresses are supported address should lie in range 0 .. 7F.
  -- Returns 0 if successful, otherwise the error code from ioctl
  
   function I2C_Write (Tx_Buffer : access unsigned_char;
                       Length : unsigned_short) return int  -- i2c_driver.h:18
   with Import => True, 
        Convention => C, 
        External_Name => "I2C_Write";

  -- Tx_Buffer must be at least Length bytes long.
  -- Transfers Length bytes from the Tx_Buffer to the specified IC_Address.
  -- Return positive values the number of bytes written, negative values are the
  -- error code returned from the call to write.
  
   function I2C_Read (Rx_Buffer : access unsigned_char;
                      Length : unsigned_short) return int  -- i2c_driver.h:24
   with Import => True, 
        Convention => C, 
        External_Name => "I2C_Read";

  -- Rx_Buffer must be at least Length bytes long.
  -- Transfers Length bytes into the RxBuffer.
  -- Return positive values the number of btres read, negative values are the
  -- error code returned from the call to read.
  
   function I2C_Close return int  -- i2c_driver.h:30
   with Import => True, 
        Convention => C, 
        External_Name => "I2C_Close";

  -- Close  the SPI device should be called before program termination.
  -- Return values:
  -- 0: Success;
  -- Non zero values are the error code returned from the call to close
  
end I2C_Interface;
