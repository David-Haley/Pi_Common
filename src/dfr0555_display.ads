-- Package to support DFRobot 0555 (2 x 16) display.
-- Author:    David Haley
-- Created:   04/03/2023
-- Last Edit: 16/03/2023

with Interfaces; use Interfaces;

package DFR0555_Display is

   subtype LCD_Lines is Natural range 0 .. 1;
   subtype LCD_Columns is Natural range 0 .. 15;

   subtype Display_Strings is String (1 .. LCD_Columns'Last + 1);

   LED_Error, LCD_Error : exception;

   procedure Enable_Display;
   -- Opens I2C device and initialise the display

   procedure Set_Brightness (Brightness : Unsigned_8;
                             Group_Brightness : Unsigned_8 := Unsigned_8'Last);
   -- Sets brightness of back light LED, must be called before turning on the
   --  backlight.

   procedure Backlight_On;
   -- Turns on the backlight.

   procedure Backlight_Off;
   -- Turns off the backlight. 

   procedure Clear;
   -- Clears all display text

   procedure Put_Line (LCD_Line : in LCD_Lines;
                       Display_String : in Display_Strings);
   -- Send text to fill one lines of the display.

   procedure Position_Cursor (Column : in LCD_Columns;
                              Line : in LCD_Lines;
                              Visible : in Boolean := True);
   -- Positions cursor to the specified character position and controles
   -- visibility.

   procedure Put (Item : Character);
   -- Puts one characer (Item) at the current cursor position. Raises an
   -- exception if an attempt is made to write beyond LCD_Columns'Last.

   procedure Put (Item : String);
   -- Puts string (Item) atarting at the current cursor position. Raises an
   -- exception if an attempt is made to write beyond LCD_Columns'Last.

   procedure Disable_Display;
   -- Blanks display and closes I2C device

end DFR0555_Display;
