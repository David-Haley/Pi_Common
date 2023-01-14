-- Modified  : David Haley
-- Created   : 22/03/2019
-- Last Edit : 04/04/2019
-- 20180404 : Set_BG_Colour added
-- 20190322 : Set_FG_Colour added

package ansi_console is

   ----------------------
   -- TYPE DEFINITIONS --
   ----------------------
   Max_X : Positive := 79;
   Max_Y : Positive := 23;
   subtype X_Pos is Natural range 0 .. Max_X;
   subtype Y_Pos is Natural range 0 .. Max_Y;

   type FG_Colours is (FG_Black, FG_Red, FG_Green, FG_Yellow, FG_Blue,
                       FG_Magenta, FG_Cyan, FG_White);
   for FG_Colours'Size use Positive'Size;
   for FG_Colours use (FG_Black => 30, FG_Red => 31, FG_Green => 32,
                       FG_Yellow => 33, FG_Blue => 34, FG_Magenta => 35,
                       FG_Cyan => 36, FG_White => 37);

   type BG_Colours is (BG_Black, BG_Red, BG_Green, BG_Yellow, BG_Blue,
                       BG_Magenta, BG_Cyan, BG_White);
   for BG_Colours'Size use Positive'Size;
   for BG_Colours use (BG_Black => 40, BG_Red => 41, BG_Green => 42,
                       BG_Yellow => 43, BG_Blue => 44, BG_Magenta => 45,
                       BG_Cyan => 46, BG_White => 47);

   --------------------
   -- SCREEN CONTROL --
   --------------------

   procedure Clear_Screen;

   procedure Goto_XY (X : in X_Pos := X_Pos'First; Y : in Y_Pos := Y_Pos'First);

   procedure Set_Cursor (Visible : in Boolean := true);

   procedure Set_FG_Colour (Colour : in FG_Colours);

   procedure Set_BG_Colour (Colour : in BG_Colours);

   -------------------
   -- SOUND CONTROL --
   -------------------
   procedure Bleep;



end ansi_console;
