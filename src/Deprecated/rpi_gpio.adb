with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

-- Author    : David Haley
-- Created   : 31/07/2017
-- Last Edit : 08/05/2019
-- Allows basic operations on the the Raspberry Pi GPIO pins as both
-- parallel inpit and output.
-- 20220508 : Added Initialization and general rewrite.
-- 20190714 : Finalization is only required if one or more pins have been bound

package body RPi_GPIO is

   GPIO_Path : constant String := "/sys/class/gpio/";
   Pin_Export : constant String := GPIO_Path & "export";
   Pin_Unexport : constant String := GPIO_Path & "unexport";
   Pin_Path : constant String := GPIO_Path & "gpio";
   Pin_Value : constant String := "/value";
   Pin_Dir : constant String := "/direction";
   Direction_In : constant String := "in"  & NUL;
   Direction_Out : constant String := "out" & NUL;
   Pin_Name : constant array (GPIO_Pins) of string (1 .. 2) :=
      ("17", "18", "27", "22", "23", "24", "25");
   FS_Delay : constant Duration := 0.1; -- some delay is required, how much?

   type GPIO is record
      Pin_File : File_Type;
      Pin_Direction : Pin_Directions := In_Pin;
      Pin_State : Pin_States := False;
      -- ensures that it is defined but may not be correct value
      Pin_Bound : Boolean := False;
   end record;

   Pin_Array : array (GPIO_Pins) of GPIO;
   Export_File : File_Type;
   
   procedure Bind_Pin (Pin : in GPIO_Pins;
                       Pin_Direction : in Pin_Directions) is
                       
      -- initialises the pin as either input or output.

      Direction_File : File_Type;

   begin -- Bind_Pin
      if Pin_Array (Pin).Pin_Bound then
         raise Pin_Bound with "Bind_Pin " & Pin'Img & ' ' & Pin_Direction'Img;
      end if; -- Pin_Array (Pin).Pin_Bound
      Put (Export_File, Pin_Name (Pin) & NUL);
      Flush (Export_File);
      -- makes pin accessible in user space
      delay FS_Delay;
      Open (Direction_File, Out_File, Pin_Path & Pin_Name (Pin) & Pin_Dir);
      if Pin_Direction = Out_Pin then
         Put_Line (Direction_File, Direction_Out);
         Flush (Direction_File);
         Open (Pin_Array (Pin).Pin_File, Out_File,
               Pin_Path & Pin_Name (Pin) & Pin_Value);
      else
         Put_Line (Direction_File, Direction_In);
         Flush (Direction_File);
         Open (Pin_Array (Pin).Pin_File, In_File,
               Pin_Path & Pin_Name (Pin) & Pin_Value);
      end if; -- Pin_Direction = Out_Pin
      Pin_Array (Pin).Pin_Direction := Pin_Direction;
      Pin_Array (Pin).Pin_Bound := true;
      Close (Direction_File);
   end Bind_Pin;

   function Read_Pin (Pin : in GPIO_Pins) return Pin_States is

      -- Reads input pins and returns current state, for output pins it returns
      -- the state after the last write.

      Read_Result : string (1 .. 2);
      Characters_Read : Natural;

   begin -- Read_Pin
      if not Pin_Array (Pin).Pin_Bound then
         raise Pin_not_Bound with "Read_Pin " & Pin'Img;
      end if; -- not Pin_Array (Pin).Pin_Bound
      if Pin_Array (Pin).Pin_Direction = In_Pin then
         Get_Line (Pin_Array (Pin).Pin_File, Read_Result, Characters_Read);
         Reset (Pin_Array (Pin).Pin_File);
         -- if "file" is not reset the any subsequent read attempt will raise
         -- an exception, presumably because EOF is true
         if Characters_Read < 1 then
            raise Pin_General_Failure with "Characters" & Characters_Read'Img;
         end if; -- Characters_Read < 1
         if Read_Result (1) = '1' then
            Pin_Array (Pin).Pin_State := True;
         elsif Read_Result (1) = '0' then
            Pin_Array (Pin).Pin_State := False;
         else
            raise Pin_General_Failure with "Read '" & Read_Result (1) & "'";
         end if; -- Read_Result (1) = '1'
      end if; -- Pin_Array (Pin).Pin_Direction = Out_Pin
      return Pin_Array (Pin).Pin_State;
   end Read_Pin;

   procedure Write_Pin (Pin_State : in Pin_States; Pin : in GPIO_Pins) is

      -- Sets Pin to the Pin_State.

   begin -- Write_Pin
      if not Pin_Array (Pin).Pin_Bound then
         raise Pin_not_Bound with "Write_Pin " & Pin'Img & ' ' & Pin_State'Img;
      end if; -- not Pin_Array (Pin).Pin_Bound
      if Pin_Array (Pin).Pin_Direction /= Out_Pin then
         raise Pin_Illegal_Operation with "Write_Pin " & Pin'Img & ' ' &
           Pin_State'Img;
      end if; -- Pin_Array (Pin).Pin_Direction /= Out_Pin
      Pin_Array (Pin).Pin_State := Pin_State;
      if Pin_State then
         Put_Line (Pin_Array (Pin).Pin_File , "1" & NUL);
      else
         Put_Line (Pin_Array (Pin).Pin_File , "0" & NUL);
      end if; -- Pin_State;
      Pin_Array (Pin).Pin_State := Pin_State;
      Flush (Pin_Array (Pin).Pin_File);
   end Write_Pin;

   procedure Initialize (Controlled_Boolean : in out Controlled_Booleans) is

   begin -- Initialize
      Open (Export_File, Out_File, Pin_Export);
      Controlled_Boolean.State := True;
   end Initialize;

   procedure Finalize (Controlled_Boolean : in out Controlled_Booleans) is

      Direction_File, Unexport_File : File_Type;

   begin -- Finalize
      -- Allow for finalisation being called more than once
      if Controlled_Boolean.State then
         Controlled_Boolean.State := False;
         Open (Unexport_File, Out_File, Pin_Unexport);
         for Pin in GPIO_Pins loop
            if Pin_Array (Pin).Pin_Bound then
               -- Return output pins to high Z state
               if Pin_Array (Pin).Pin_Direction = Out_Pin then
                  Open (Direction_File, Out_File, Pin_Path & Pin_Name (Pin) &
                        Pin_Dir);
                  Put_Line (Direction_File, Direction_In);
                  Flush (Direction_File);
                  Close (Direction_File);
               end if; -- Pin_Array (Pin).Pin_Direction = Out_Pin
               Close (Pin_Array (Pin).Pin_File);
               Put (Unexport_File, Pin_Name (Pin) & NUL);
               Flush (Unexport_File);
            end if; -- Pin_Array (Pin).Pin_Bound
         end loop; -- for Pin in 
      end if; -- Controlled_Boolean
      -- Closing the Export and Unexport files raises a Program_Error exception
      -- The code to close these files has been retained with an exception
      -- handler that does nothing!
      begin -- Unexport
         Close (Unexport_File);
      exception
        when others => null;
      end; -- Unexport
      begin -- Export
         Close (Export_File);
      exception
        when others => null;
      end; -- Export
   end Finalize;

   Finalisation_Required : Controlled_Booleans;

end RPI_GPIO;
