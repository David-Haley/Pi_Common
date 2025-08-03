with Ada.Exceptions; use Ada.Exceptions;
with GPIO_Driver_h; use GPIO_Driver_h;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

-- Author    : David Haley
-- Created   : 31/07/2017
-- Last Edit : 02/08/2025
-- Allows basic operations on the the Raspberry Pi GPIO pins as both
-- parallel input and output.
-- 20250802 : Ported to use C interface to libgpiod.
-- 20220508 : Added Initialization and general rewrite.
-- 20190714 : Finalization is only required if one or more pins have been bound

package body RPi_GPIO is

   Chip_Name : constant chars_ptr := New_String ("gpiochip0");
   Invalid_Line : constant Interfaces.C.int := -1;

   type GPIO is record
      Pin_Name : chars_ptr;
      Line_Number : Interfaces.C.int;
      Pin_Direction : Pin_Directions;
      Pin_State : Pin_States;
      Pin_Bound : Boolean;
   end record;

   Pin_Array : array (GPIO_Pins) of GPIO :=
      (
         Gen0 => (New_String ("GPIO17"), Invalid_Line, In_Pin, Pin_Low, False),
         Gen1 => (New_String ("GPIO18"), Invalid_Line, In_Pin, Pin_Low, False),
         Gen2 => (New_String ("GPIO27"), Invalid_Line, In_Pin, Pin_Low, False),
         Gen3 => (New_String ("GPIO22"), Invalid_Line, In_Pin, Pin_Low, False),
         Gen4 => (New_String ("GPIO23"), Invalid_Line, In_Pin, Pin_Low, False),
         Gen5 => (New_String ("GPIO24"), Invalid_Line, In_Pin, Pin_Low, False),
         Gen6 => (New_String ("GPIO25"), Invalid_Line, In_Pin, Pin_Low, False)
      );
   
   procedure Bind_Pin (Pin : in GPIO_Pins;
                       Pin_Direction : in Pin_Directions) is
                       
      -- initialises the pin as either input or output.

   begin -- Bind_Pin
      if Pin_Array (Pin).Pin_Bound then
         raise Pin_Bound with "Bind_Pin " & Pin'Img & ' ' & Pin_Direction'Img;
      end if; -- Pin_Array (Pin).Pin_Bound
      if Pin_Direction = Out_Pin then
         Pin_Array (Pin).Line_Number := Open_Output (Pin_Array (Pin).Pin_Name);
      else
         Pin_Array (Pin).Line_Number := Open_Input (Pin_Array (Pin).Pin_Name);
      end if; -- Pin_Direction = Out_Pin
      if Pin_Array (Pin).Line_Number = Invalid_Line then
         raise Pin_General_Failure with "Failed to bind " & Pin'Img & " as " &
           Pin_Direction'Img;
      else
         Pin_Array (Pin).Pin_Direction := Pin_Direction;
         Pin_Array (Pin).Pin_Bound := True;
      end if; -- Pin_Array (Pin).Line_Number = Invalid_Line
   end Bind_Pin;
   
   procedure Release_Pin (Pin : in GPIO_Pins) is
   -- Unbinds pin, allows conversion from input to output etc.
   
   begin -- Release_Pin
      if not Pin_Array (Pin).Pin_Bound then
         raise Pin_Bound with "Release_Pin ";
      end if; -- Pin_Array (Pin).Pin_Bound
      Close_GPIO_Line (Pin_Array (Pin).Line_Number);
      Pin_Array (Pin).Line_Number := Invalid_Line;
      Pin_Array (Pin).Pin_Direction := In_Pin;
      Pin_Array (Pin).Pin_State := Pin_Low;
      Pin_Array (Pin).Pin_Bound := False;
   end Release_Pin;

   function Read_Pin (Pin : in GPIO_Pins) return Pin_States is

      -- Reads input pins and returns current state, for output pins it returns
      -- the state after the last write.

   begin -- Read_Pin
      if not Pin_Array (Pin).Pin_Bound then
         raise Pin_not_Bound with "Read_Pin " & Pin'Img;
      end if; -- not Pin_Array (Pin).Pin_Bound
      if Pin_Array (Pin).Pin_Direction = In_Pin then
         case Read_Input (Pin_Array (Pin).Line_Number) is
            when 1 =>
               Pin_Array (Pin).Pin_State := Pin_High;
            when 0 =>
               Pin_Array (Pin).Pin_State := Pin_Low;
            when others =>
               raise Pin_General_Failure with "Failure reading " & Pin'Img;
         end case; -- Read_Input (Pin_Array (Pin).Line_Number))
      end if; -- Pin_Array (Pin).Pin_Direction = In_Pin
      -- for output pins the last state written is stored in Pin_Array
      return Pin_Array (Pin).Pin_State;
   end Read_Pin;

   procedure Write_Pin (Pin_State : in Pin_States; Pin : in GPIO_Pins) is

      -- Sets Pin to the Pin_State.
      
      C_Return : int;

   begin -- Write_Pin
      if not Pin_Array (Pin).Pin_Bound then
         raise Pin_not_Bound with "Write_Pin " & Pin'Img & ' ' & Pin_State'Img;
      end if; -- not Pin_Array (Pin).Pin_Bound
      if Pin_Array (Pin).Pin_Direction /= Out_Pin then
         raise Pin_Illegal_Operation with "Write_Pin " & Pin'Img & ' ' &
           Pin_State'Img;
      end if; -- Pin_Array (Pin).Pin_Direction /= Out_Pin
      if Pin_State then
          C_Return := Set_High (Pin_Array (Pin).Line_Number);
      else
          C_Return := Set_Low (Pin_Array (Pin).Line_Number);
      end if; -- Pin_State;
      if C_Return = 0 then
         Pin_Array (Pin).Pin_State := Pin_State;
      else
         raise Pin_General_Failure with "Write_Pin " & Pin'Img & ' ' &
           Pin_State'Img;
      end if; -- C_Return = 0
   end Write_Pin;
   
   procedure Strobe_High (Pin : in GPIO_Pins) is
   
   -- Sets a previously low output high for the minimum time possible,
   -- time will depend on Pi speed and overhead. if output was initially
   -- high it will be set low on return, this is not treated as an error.
   
   begin -- Strobe_High
      if not Pin_Array (Pin).Pin_Bound then
         raise Pin_not_Bound with "Strob_High " & Pin'Img;
      end if; -- not Pin_Array (Pin).Pin_Bound
      if Pin_Array (Pin).Pin_Direction /= Out_Pin then
         raise Pin_Illegal_Operation with "Surob_High " & Pin'Img;
      end if; -- Pin_Array (Pin).Pin_Direction /= Out_Pin
      if Pulse_High (Pin_Array (Pin).Line_Number) = 0 then
         Pin_Array (Pin).Pin_State := Pin_Low;
         -- Sets the pin stste even if it was correct
      else
         raise Pin_General_Failure with "Strobe_High " & Pin'Img;
      end if; -- Pulse_High (Pin_Array (Pin).Line_Number) = 0
   end Strobe_High;
   
   procedure Strobe_Low (Pin : in GPIO_Pins) is
   
   -- Sets a previously high output low for the minimum time possible,
   -- time will depend on Pi speed and overhead. if output was initially
   -- low it will be set high on return, this is not treated as an error.
   
   begin -- Strobe_Low
      if not Pin_Array (Pin).Pin_Bound then
         raise Pin_not_Bound with "Strob_Low " & Pin'Img;
      end if; -- not Pin_Array (Pin).Pin_Bound
      if Pin_Array (Pin).Pin_Direction /= Out_Pin then
         raise Pin_Illegal_Operation with "Surob_Low " & Pin'Img;
      end if; -- Pin_Array (Pin).Pin_Direction /= Out_Pin
      if Pulse_Low (Pin_Array (Pin).Line_Number) = 0 then
         Pin_Array (Pin).Pin_State := Pin_High;
         -- Sets the pin stste even if it was correct
      else
         raise Pin_General_Failure with "Strobe_Low " & Pin'Img;
      end if; -- Pulse_Low (Pin_Array (Pin).Line_Number) = 0
   end Strobe_Low;

   procedure Initialize (Controlled_Boolean : in out Controlled_Booleans) is

   begin -- Initialize
      if Open_GPIO_Chip (Chip_Name) /= 0 then
         raise Pin_General_Failure with "Failure opening GPIO chip" &
           Value (Chip_Name);
      end if; -- Open_GPIO_Chip (Chip_Name) /= 0
      Controlled_Boolean.State := True;
   end Initialize;

   procedure Finalize (Controlled_Boolean : in out Controlled_Booleans) is

   begin -- Finalize
      -- Allow for finalisation being called more than once
      if Controlled_Boolean.State then
         Controlled_Boolean.State := False;
         Close_GPIO_Chip;
      end if; -- Controlled_Boolean
   end Finalize;

   Finalisation_Required : Controlled_Booleans;
   
end RPI_GPIO;
