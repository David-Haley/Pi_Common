-- test program for DFRobot 0555 (2 x 16) display.
-- Author:    David Haley
-- Created:   07/03/2023
-- Last Edit: 15/03/2023

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions; use Ada.Exceptions;
-- with Ada.Strings.Unbounded_Strings; use Ada.Strings.Unbounded_Strings;
with Interfaces; use Interfaces;
with DFR0555_Display;

procedure Test_DFR0555_Display is

   package U_8_IO is new Ada.Text_IO.Modular_IO (Unsigned_8);
   package Column_IO is new
     Ada.Text_IO.Integer_IO (DFR0555_Display.LCD_Columns);
   package Line_IO is new Ada.Text_IO.Integer_IO (DFR0555_Display.LCD_Lines);
   package Boolean_IO is new Ada.Text_IO.Enumeration_IO (Boolean);

   Test : Character;
   Test_Message_0 : constant DFR0555_Display.Display_Strings :=
     "DFR0555 RPI ADA!";
   Test_Message_1 : constant DFR0555_Display.Display_Strings :=
     "2 Lines 16 Chars";

   procedure Set_Brightness is

      Brightness : Unsigned_8;

   begin -- Set_Brightness
      Put ("Brightness (0 .. 255): ");
      U_8_IO.Get (Brightness);
      DFR0555_Display.Set_Brightness (Brightness);
   end Set_Brightness;

   procedure Position_Cursor is

      Column : DFR0555_Display.LCD_Columns;
      Line : DFR0555_Display.LCD_Lines;
      Visible : Boolean;

   begin --
      Put ("Cursor Position, format Column Line Viaiability: ");
      Column_IO.Get (Column);
      Line_IO.Get (Line);
      Boolean_IO.Get (Visible);
      DFR0555_Display.Position_Cursor (Column, Line, Visible);
   end;

   procedure Put_Character is

      Item : Character;

   begin -- Put_Character
      Put ("Character to send: ");
      Get (Item);
      DFR0555_Display.Put (Item);
   end Put_Character;

   procedure Put_String is

   begin -- Put_String
      Skip_Line;
      Put ("String to send: ");
      DFR0555_Display.Put (Get_Line);
   end Put_String;

begin -- Test_DFR0555_Display
   DFR0555_Display.Enable_Display;
   loop -- One test
      Put_Line ("A Set Brightness");
      Put_Line ("B Gacklight on");
      Put_Line ("C Backlight off");
      Put_Line ("D Clear display");
      Put_Line ("E Display test message");
      Put_Line ("F Position_Cursor");
      Put_Line ("G Put one character at cursor position");
      Put_Line ("H Put string at cursor position");
      Put_Line ("0 Exit tests");
      Put ("Test: ");
      Get (Test);
      exit when Test = '0';
      begin -- Test exception block
         case To_Upper (Test) is
            when 'A' =>
               Set_Brightness;
            when 'B' =>
               DFR0555_Display.Backlight_On;
            when 'C' =>
               DFR0555_Display.Backlight_Off;
            when 'D' =>
               DFR0555_Display.Clear;
            when 'E' =>
               DFR0555_Display.Put_Line (0, Test_Message_0);
               DFR0555_Display.Put_Line (1, Test_Message_1);
            when 'F' =>
               Position_Cursor;
            when 'G' =>
               Put_Character;
            when 'H' =>
               Put_String;
            when others =>
               Put_Line ("Unknown test: '" & Test & "'");
         end case; -- To_Upper (Test)
      exception
         when E : others =>
            Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      end; -- Test exception block
   end loop; -- One test
   DFR0555_Display.Disable_Display;
end Test_DFR0555_Display;
