-- Test program for i2c_driver.c, the simple interface is written in C to allow
-- calls to open, close, read write, and ioctl to be made.
-- Author    : David Haley
-- Created   : 03/03/2023
-- Last Edit : 15/10/2026

--  20261015 : Probe with a zero length write (quick command) rather than
--  writing a data byte to every device.
--  20260615 : Some compiler warnings removed.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with I2C_Interface; use I2C_Interface;

procedure Test_I2C is

   subtype Buffer_Indices is Natural range 0 .. 9;
   type Buffers is array (Buffer_Indices) of aliased unsigned_char;

   Return_Value : int;
   Buffer : Buffers := [others => 0];
   Buffer_Ptr : constant access unsigned_char :=
     Buffer (Buffer_Indices'First)'Access;
   
begin -- Test_I2C
   Return_Value := I2C_Open (1);
   Assert (Return_Value = 0, "I2C_Open failed error code:" & Return_Value'Img);
   for A in IC_Addresses loop
      Return_Value := Set_IC_Address (A);
      Assert (Return_Value = 0, "I2C_Set_Adddress failed error code:" &
              Return_Value'Img);
      Return_Value := I2C_Write (Buffer_Ptr, 0);
      if Return_Value = 0 then
         Put_Line (A'Image & " IC found");
      else
         Put_Line (A'Image & " No IC");
      end if; -- Return_Value = 1
   end loop; -- A in IC_Addresses
   pragma Warnings (Off, "-gnatwm");
   Return_Value := I2C_Close; -- useless assignment warning suppressed
   pragma Warnings (On, "-gnatwm");
end Test_I2C;
