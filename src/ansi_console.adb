-- Modified  : David Haley
-- Created   : 22/03/2019
-- Last Edit : 04/04/2019
-- 20180404 : Set_BG_Colour added
-- 20190322 : Ser_FG_Colour added

with Ada.Text_IO, Ada.Characters.Latin_1;
use Ada.Text_IO, Ada.Characters.Latin_1;

package body ansi_console is
   EscSquareBracket : constant string :=  ESC & "[";

   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);

   procedure Set_Cursor (Visible : in Boolean := true) is

   begin -- Set_Console
      if Visible then
         Put (EscSquareBracket & "0;3;5v");
      else
         Put (EscSquareBracket & "1;3;4v");
      end if;
   end Set_Cursor;

   procedure Goto_XY (X : in X_Pos := X_Pos'First;
                      Y : in Y_Pos := Y_Pos'First) is

   begin
      Put (EscSquareBracket &
             Character'Val ((Y + 1) / 10 + Character'Pos ('0')) &
             Character'Val ((Y + 1) mod 10 + Character'Pos ('0')) & ";" &
             Character'Val ((X + 1) / 10 + Character'Pos ('0')) &
             Character'Val ((X + 1) mod 10 + Character'Pos ('0')) & "H");
   end Goto_XY;

   procedure Set_FG_Colour (Colour : in FG_Colours) is

      Number : Positive;
      for Number'Address use Colour'Address;

   begin -- Set_FG_Colour
      Put (EscSquareBracket);
      Positive_IO.Put (Number, 2);
      Put ('m');
   end Set_FG_Colour;

   procedure Set_BG_Colour (Colour : in BG_Colours) is

      Number : Positive;
      for Number'Address use Colour'Address;

   begin -- Set_BG_Colour
      Put (EscSquareBracket);
      Positive_IO.Put (Number, 2);
      Put ('m');
   end Set_BG_Colour;

   --------------------
   -- SCREEN CONTROL --
   --------------------

   procedure Clear_Screen is

   begin
      Put (EscSquareBracket & "2J" );
   end Clear_Screen;

   -------------------
   -- SOUND CONTROL --
   -------------------
   procedure Bleep is
   begin
      Put (BEL);
   end Bleep;

 end ansi_console;
