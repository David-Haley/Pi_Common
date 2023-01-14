-- Author    : David Haley
-- Created   : 22/03/2019
-- Last Edit : 04/04/2019
-- 20190404 : Rewrite for both foreground and background colours

with Ada.Text_IO; use Ada.Text_IO;
with ansi_console; use ansi_console;

procedure Test_ANSI is

   Test_Text : constant String := "abcd_ efgh";

   package FG_IO is new Ada.Text_IO.Enumeration_IO (FG_Colours);
   use FG_IO;

   package BG_IO is new Ada.Text_IO.Enumeration_IO (BG_Colours);
   use BG_IO;

begin -- Test_ANSI
   Set_BG_Colour (BG_Black);
   Set_FG_Colour (FG_Black);
   Put (Test_Text); -- invisible
   Set_FG_Colour (FG_White);
   for FG_Colour in FG_Colours loop
      Put (FG_Colour, Test_Text'Length);
   end loop; -- FG_Colour in FG_Colours
   Set_BG_Colour (BG_Black);
   Set_FG_Colour (FG_White);
   New_Line;
   for BG_Colour in BG_Colours loop
      Put (BG_Colour, Test_Text'Length);
      Set_BG_Colour (BG_Colour);
      for FG_Colour in FG_Colours loop
         Set_FG_Colour (FG_Colour);
         Put (Test_Text);
      end loop; -- FG_Colour in FG_Colours
      Set_BG_Colour (BG_Black);
      Set_FG_Colour (FG_White);
      New_Line;
   end loop; -- BG_Colour in FG_Colours
end Test_ANSI;
