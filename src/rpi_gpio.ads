-- Allows basic operations on the the Raspberry Pi GPIO pins as both
-- parallel inpit and output.

-- Author    : David Haley
-- Created   : 02/07/2017
-- Last Edit : 26/05/2026

--  20260526 : Compiler warnings removed, header comments rearranged.
-- 20250803 : Release_Pin added.
-- 20250802 : Ported to use C interface to libgpiod, Strob_High and Strob_Low
-- added.
-- 20220607 : Private declarations added.

with Ada.Finalization; use Ada.Finalization;

package RPi_GPIO is

   Pin_Low : constant Boolean := False;
   Pin_High : constant Boolean := True;
   -- Provides a descriptive names for pin states whilst allowing direct use
   -- of boolean expressions.

   type GPIO_Pins is (Gen0, Gen1, Gen2, Gen3, Gen4, Gen5, Gen6);
   -- Pins named GPIO-GenX on schematic

   type Pin_Directions is (In_Pin, Out_Pin);

   subtype Pin_States is Boolean;

   procedure Bind_Pin (Pin : in GPIO_Pins;
                       Pin_Direction : in Pin_Directions);
   -- initialises the pin as either input or output.
   
   procedure Release_Pin (Pin : in GPIO_Pins);
   -- Unbinds pin, allows conversion from input to output etc.

   function Read_Pin (Pin : in GPIO_Pins)
     return Pin_States;
   -- Reads input pins and returns current state, for output pins it returns the
   -- state after the last write.

   procedure Write_Pin (Pin_State : in Pin_States;
                        Pin : in GPIO_Pins);
   -- Sets Pin to the Pin_State.
   
   procedure Strobe_High (Pin : in GPIO_Pins);
   
   -- Sets a previously low output high for the minimum time possible,
   -- time will depend on Pi speed and overhead. if output was initially
   -- high it will be set low on return, this is not treated as an error.
   
   procedure Strobe_Low (Pin : in GPIO_Pins);
   
   -- Sets a previously high output low for the minimum time possible,
   -- time will depend on Pi speed and overhead. if output was initially
   -- low it will be set high on return, this is not treated as an error.

   Pin_Bound, Pin_not_Bound, Pin_General_Failure, Pin_Illegal_Operation
      : exception;

private

   type Controlled_Booleans is new Limited_Controlled with
      record
         State : Boolean := False;
      end record;

   overriding
   procedure Initialize (Controlled_Boolean : in out Controlled_Booleans);

   overriding
   procedure Finalize (Controlled_Boolean : in out Controlled_Booleans);


end RPi_GPIO;
