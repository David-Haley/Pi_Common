with Ada.Text_IO; use Ada.Text_IO;
with Ada.Finalization; use Ada.Finalization;

-- Author    : David Haley
-- Created   : 02/07/2017
-- Last Edit : 07/05/2022
-- Allows basic operations on the the Raspberry Pi GPIO pins as both
-- parallel inpit and output.
-- 20220607 : Private declarations added.

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

   function Read_Pin (Pin : in GPIO_Pins)
     return Pin_States;
   -- Reads input pins and returns current state, for output pins it returns the
   -- state after the last write.

   procedure Write_Pin (Pin_State : in Pin_States;
                        Pin : in GPIO_Pins);
   -- Sets Pin to the Pin_State.

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
