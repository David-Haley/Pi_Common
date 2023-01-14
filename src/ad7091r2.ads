-- Driver for Analogue devices AD7091R-2
-- provides a function for reading both channels of the A/D.
-- Author    : David Haley
-- Created   : 22/09/2017
-- Last Edit : 06/05/2022
-- 20220506: Converted to use SPI_Interface.ads, implemented in C and callable
-- from Ada.

with Ada.Finalization; use Ada.Finalization;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with RPi_GPIO; use RPi_GPIO;
with SPI_Interface; use SPI_Interface;

generic
   AD_Reset_Pin  : GPIO_Pins := Gen3; -- Reset for A/D hardware
   AD_Sample_Pin : GPIO_Pins := Gen4; -- Starts A/D conversion
   SPI_Device : SPI_Devices := 0; -- SPI channel to be used
   SPI_Speed : SPI_Speeds := 12500000;
   -- 12.5 Mhz half maximum clock for AD7091R2 and exactly settable in RPI

package AD7091R2 is

   type A_Volts is mod 2**12;
   type A_Channels is mod 2**1;
   type A_Volt_Arrays is array (A_Channels) of A_Volts;
   SPI_Error : exception;

   function AD_Read return A_Volt_Arrays;
   -- reads both channels of the A/D

   private

   type SPI_States is new Limited_Controlled with record
      Enabled : Boolean := False;
      Reset_Pin : GPIO_Pins := AD_Reset_Pin;
      Sample_Pin : GPIO_Pins := AD_Sample_Pin;
      Device : SPI_Devices := SPI_Device;
      Speed : SPI_Speeds := SPI_Speed;
   end record; -- SPI_States

   overriding
   procedure Initialize (SPI_State : in out SPI_States);

   overriding
   procedure Finalize (SPI_State : in out SPI_States);

end AD7091R2;
