-- Allows basic operations on the the Raspberry Pi GPIO pins as both
-- parallel input and output.

-- Author    : David Haley
-- Created   : 31/07/2017
-- Last Edit : 14/10/2026

--  20261014 : Pin state held in a protected object to make the package task
--  safe. GPIO chip opened on first Bind_Pin rather than at elaboration.
--  Release_Pin raises Pin_not_Bound for an unbound pin. Exception messages
--  include the error reported by the C driver. Pin state resynchronised
--  after a failed strobe.
--  20260526 : Compiler warnings removed and heading comments relocated
--  Redundant Finalization removed.
-- 20260219 : Ported to ues libgpio version 2, Chip_Name changef from
-- "gpiochip0" to "/dev/gpiochip0"
-- 20250802 : Ported to use C interface to libgpiod.
-- 20220508 : Added Initialization and general rewrite.
-- 20190714 : Finalization is only required if one or more pins have been bound

with GNAT.OS_Lib;
with GPIO_Driver_h; use GPIO_Driver_h;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body RPi_GPIO is

   Chip_Name : constant chars_ptr := New_String ("/dev/gpiochip0");

   type GPIO is record
      Pin_Name : chars_ptr;
      Line_Number : Interfaces.C.int;
      Pin_Direction : Pin_Directions;
      Pin_State : Pin_States;
      Pin_Bound : Boolean;
   end record;

   type Pin_Arrays is array (GPIO_Pins) of GPIO;

   function Error_Text (C_Return : in int) return String is
     (" (" & GNAT.OS_Lib.Errno_Message (Err => Integer (-C_Return)) & ')');
   -- Describes the -errno value returned by the C driver.

   protected GPIO_State is
      -- Serialises all access to Pin_Array and the C driver, which holds
      -- global state of its own.

      procedure Bind (Pin : in GPIO_Pins;
                      Pin_Direction : in Pin_Directions);

      procedure Release (Pin : in GPIO_Pins);

      procedure Read (Pin : in GPIO_Pins;
                      Pin_State : out Pin_States);

      procedure Write (Pin_State : in Pin_States;
                       Pin : in GPIO_Pins);

      procedure Strobe (Pin : in GPIO_Pins;
                        High : in Boolean);
      -- Strobes high if High otherwise low.

      procedure Close_Chip;
      -- Releases all lines and the chip, if it has been opened.

   private

      Chip_Open : Boolean := False;

      Pin_Array : Pin_Arrays :=
         [
            Gen0 => (New_String ("GPIO17"), -1, In_Pin, Pin_Low, False),
            Gen1 => (New_String ("GPIO18"), -1, In_Pin, Pin_Low, False),
            Gen2 => (New_String ("GPIO27"), -1, In_Pin, Pin_Low, False),
            Gen3 => (New_String ("GPIO22"), -1, In_Pin, Pin_Low, False),
            Gen4 => (New_String ("GPIO23"), -1, In_Pin, Pin_Low, False),
            Gen5 => (New_String ("GPIO24"), -1, In_Pin, Pin_Low, False),
            Gen6 => (New_String ("GPIO25"), -1, In_Pin, Pin_Low, False)
         ];

   end GPIO_State;

   protected body GPIO_State is

      procedure Check_Output (Pin : in GPIO_Pins; Operation : in String) is
         -- Raises an exception unless Pin is bound as an output.

      begin -- Check_Output
         if not Pin_Array (Pin).Pin_Bound then
            raise Pin_not_Bound with Operation & ' ' & Pin'Img;
         end if; -- not Pin_Array (Pin).Pin_Bound
         if Pin_Array (Pin).Pin_Direction /= Out_Pin then
            raise Pin_Illegal_Operation with Operation & ' ' & Pin'Img;
         end if; -- Pin_Array (Pin).Pin_Direction /= Out_Pin
      end Check_Output;

      procedure Bind (Pin : in GPIO_Pins;
                      Pin_Direction : in Pin_Directions) is

         C_Return : int;

      begin -- Bind
         if Pin_Array (Pin).Pin_Bound then
            raise Pin_Bound with "Bind_Pin " & Pin'Img & ' ' &
              Pin_Direction'Img;
         end if; -- Pin_Array (Pin).Pin_Bound
         if not Chip_Open then
            C_Return := Open_GPIO_Chip (Chip_Name);
            if C_Return /= 0 then
               raise Pin_General_Failure with "Failure opening GPIO chip " &
                 Value (Chip_Name) & Error_Text (C_Return);
            end if; -- C_Return /= 0
            Chip_Open := True;
         end if; -- not Chip_Open
         if Pin_Direction = Out_Pin then
            C_Return := Open_Output (Pin_Array (Pin).Pin_Name);
         else
            C_Return := Open_Input (Pin_Array (Pin).Pin_Name);
         end if; -- Pin_Direction = Out_Pin
         if C_Return < 0 then
            raise Pin_General_Failure with "Failed to bind " & Pin'Img &
              " as " & Pin_Direction'Img & Error_Text (C_Return);
         end if; -- C_Return < 0
         Pin_Array (Pin).Line_Number := C_Return;
         Pin_Array (Pin).Pin_Direction := Pin_Direction;
         Pin_Array (Pin).Pin_State := Pin_Low;
         Pin_Array (Pin).Pin_Bound := True;
      end Bind;

      procedure Release (Pin : in GPIO_Pins) is

      begin -- Release
         if not Pin_Array (Pin).Pin_Bound then
            raise Pin_not_Bound with "Release_Pin " & Pin'Img;
         end if; -- not Pin_Array (Pin).Pin_Bound
         Close_GPIO_Line (Pin_Array (Pin).Line_Number);
         Pin_Array (Pin).Line_Number := -1;
         Pin_Array (Pin).Pin_Direction := In_Pin;
         Pin_Array (Pin).Pin_State := Pin_Low;
         Pin_Array (Pin).Pin_Bound := False;
      end Release;

      procedure Read (Pin : in GPIO_Pins;
                      Pin_State : out Pin_States) is

         C_Return : int;

      begin -- Read
         if not Pin_Array (Pin).Pin_Bound then
            raise Pin_not_Bound with "Read_Pin " & Pin'Img;
         end if; -- not Pin_Array (Pin).Pin_Bound
         if Pin_Array (Pin).Pin_Direction = In_Pin then
            C_Return := Read_Input (Pin_Array (Pin).Line_Number);
            if C_Return < 0 then
               raise Pin_General_Failure with "Failure reading " & Pin'Img &
                 Error_Text (C_Return);
            end if; -- C_Return < 0
            Pin_Array (Pin).Pin_State := C_Return /= 0;
         end if; -- Pin_Array (Pin).Pin_Direction = In_Pin
         -- for output pins the last state written is stored in Pin_Array
         Pin_State := Pin_Array (Pin).Pin_State;
      end Read;

      procedure Write (Pin_State : in Pin_States;
                       Pin : in GPIO_Pins) is

         C_Return : int;

      begin -- Write
         Check_Output (Pin, "Write_Pin " & Pin_State'Img);
         if Pin_State then
            C_Return := Set_High (Pin_Array (Pin).Line_Number);
         else
            C_Return := Set_Low (Pin_Array (Pin).Line_Number);
         end if; -- Pin_State
         if C_Return /= 0 then
            raise Pin_General_Failure with "Write_Pin " & Pin'Img & ' ' &
              Pin_State'Img & Error_Text (C_Return);
         end if; -- C_Return /= 0
         Pin_Array (Pin).Pin_State := Pin_State;
      end Write;

      procedure Strobe (Pin : in GPIO_Pins;
                        High : in Boolean) is

         C_Return, Read_Back : int;

      begin -- Strobe
         if High then
            Check_Output (Pin, "Strobe_High");
            C_Return := Pulse_High (Pin_Array (Pin).Line_Number);
         else
            Check_Output (Pin, "Strobe_Low");
            C_Return := Pulse_Low (Pin_Array (Pin).Line_Number);
         end if; -- High
         if C_Return /= 0 then
            -- The strobe may have failed part way through, the stored state
            -- is updated to match the line if it can be read.
            Read_Back := Read_Input (Pin_Array (Pin).Line_Number);
            if Read_Back >= 0 then
               Pin_Array (Pin).Pin_State := Read_Back /= 0;
            end if; -- Read_Back >= 0
            raise Pin_General_Failure with "Strobe " & Pin'Img & ' ' &
              High'Img & Error_Text (C_Return);
         end if; -- C_Return /= 0
         Pin_Array (Pin).Pin_State := not High;
         -- Sets the pin state even if it was correct
      end Strobe;

      procedure Close_Chip is

      begin -- Close_Chip
         if Chip_Open then
            Close_GPIO_Chip;
            Chip_Open := False;
            for P in GPIO_Pins loop
               Pin_Array (P).Line_Number := -1;
               Pin_Array (P).Pin_Bound := False;
            end loop; -- P in GPIO_Pins
         end if; -- Chip_Open
      end Close_Chip;

   end GPIO_State;

   procedure Bind_Pin (Pin : in GPIO_Pins;
                       Pin_Direction : in Pin_Directions) is

      -- initialises the pin as either input or output.

   begin -- Bind_Pin
      GPIO_State.Bind (Pin, Pin_Direction);
   end Bind_Pin;

   procedure Release_Pin (Pin : in GPIO_Pins) is
   -- Unbinds pin, allows conversion from input to output etc.

   begin -- Release_Pin
      GPIO_State.Release (Pin);
   end Release_Pin;

   function Read_Pin (Pin : in GPIO_Pins) return Pin_States is

      -- Reads input pins and returns current state, for output pins it returns
      -- the state after the last write.

      Result : Pin_States;

   begin -- Read_Pin
      GPIO_State.Read (Pin, Result);
      return Result;
   end Read_Pin;

   procedure Write_Pin (Pin_State : in Pin_States; Pin : in GPIO_Pins) is

      -- Sets Pin to the Pin_State.

   begin -- Write_Pin
      GPIO_State.Write (Pin_State, Pin);
   end Write_Pin;

   procedure Strobe_High (Pin : in GPIO_Pins) is

   -- Sets a previously low output high for the minimum time possible,
   -- time will depend on Pi speed and overhead. if output was initially
   -- high it will be set low on return, this is not treated as an error.

   begin -- Strobe_High
      GPIO_State.Strobe (Pin, High => True);
   end Strobe_High;

   procedure Strobe_Low (Pin : in GPIO_Pins) is

   -- Sets a previously high output low for the minimum time possible,
   -- time will depend on Pi speed and overhead. if output was initially
   -- low it will be set high on return, this is not treated as an error.

   begin -- Strobe_Low
      GPIO_State.Strobe (Pin, High => False);
   end Strobe_Low;

   procedure Finalize (Controlled_Boolean : in out Controlled_Booleans) is

   begin -- Finalize
      -- Allow for finalisation being called more than once
      if not Controlled_Boolean.State then
         Controlled_Boolean.State := True;
         GPIO_State.Close_Chip;
      end if; -- not Controlled_Boolean.State
   end Finalize;

   pragma Warnings (Off, "-gnatwu");
   Finalisation_Required : Controlled_Booleans;
   --  Required to cause Finalization code to run, releasing the GPIO chip.
   --  Warning for unreferenced variable supptessed.
   pragma Warnings (On, "-gnatwu");

end RPi_GPIO;
