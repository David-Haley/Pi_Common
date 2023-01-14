-- Test program for spi_driver.c, the simple interface is written in C to allow
-- calls to ioctl to be made.
-- Author    : David Haley
-- Created   : 30/04/2022
-- Last Edit : 02/05/2022
-- 20220501: Shifting values as opposed to random values.
-- 20220502: Values start at 0 

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with SPI_Interface; use SPI_Interface;

procedure Test_SPI is

   subtype Buffer_Indices is Natural range 0 .. 1023;
   type Buffers is array (Buffer_Indices) of aliased Unsigned_8;
   TX_Buffer, RX_Buffer : Buffers;
   TX_Buffer_Ptr : constant access Unsigned_8 := TX_Buffer (0)'Access;
   Rx_Buffer_Ptr : constant access Unsigned_8 := RX_Buffer (0)'Access;
   Length : constant unsigned_short :=
     unsigned_short (Buffer_Indices'Last + 1);
   Device : SPI_Devices;
   Mode : SPI_Modes;
   Speed : SPI_Speeds;
   Pass, Fail : Natural := 0;
   Return_Value : int;
   
begin -- Test_SPI
   if Argument_Count = 3 then
      Device := SPI_Devices'Value (Argument (1));
      Mode := SPI_Modes'Value (Argument (2));
      Speed := SPI_Speeds'Value (Argument(3));
      Return_Value := SPI_Open (Device, Mode, Speed);
      Assert (Return_Value = 0, "Open failed:" & Return_Value'Img);
      for T in Natural range 0 .. 999 loop
         for I in Buffer_Indices loop
            TX_Buffer (I) := Unsigned_8'Mod (T + I);
         end loop; -- I in Buffer_Indices
         Return_Value := SPI_Transfer (Tx_Buffer_Ptr, Rx_Buffer_Ptr, Length);
         Assert (Return_Value = 0, "Transfer failed:" & Return_Value'Img);
         if TX_Buffer = RX_Buffer then
            Pass := Pass + 1;
         else
            Fail := Fail + 1;
         end if; -- TX_Buffer = RX_Buffer
      end loop; -- T in Positive range 1 .. 1000
      Put_line ("Pass count:" & Pass'Img & " Fail count:" & Fail'Img);
      Return_Value := SPI_Close;
      Assert (Return_Value = 0, "Close failed:" & Return_Value'Img);
   else
      Put_Line ("Usage: test_spi Device Mode Speed");
   end if; -- Argument_Count = 3
end Test_SPI;
