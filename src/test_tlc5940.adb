-- Test program for two cascaded TLC5940. Dsecrete LEDs on first chip 0 .. 7
-- and seven segment display on seond chip 8 .. 15;

-- Author    : David Haley
-- Created   : 16/08/2017
-- Last Edit : 09/06/2022

-- 20220609 : Driver_Types renamed to TLC5940_Driver_Types;
-- 20220119 : End_Ambient_Light no longer required.
-- 20190716 : Comparitor pin is now non default Gen3 to match test hardware.
-- 20190326 : Ambient light reported menu two column
-- 20190324 : two cascaded TLC4940s LEDs on dirst and & segment display on
-- second; Octal clock added as default test
-- 20190322 : Seven segment and ramp correction tests added
-- 20190320 : Ramping brightness test added

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Interfaces; use Interfaces;
with ANSI_console; use ANSI_console;
with RPi_GPIO; use RPi_GPIO;
with TLC5940_Driver_Types; Use TLC5940_Driver_Types;
with TLC5940;

procedure Test_TLC5940 is
   type Test_Device is (LEDs, Display); -- two devices
   type Test_Display is (Digit_One); -- one seven segment display
   Test_Requested : Character := 'c'; -- default to runnning octal clock
   Menu_Start : constant Y_Pos := 7;
   Menu_Column_2 : constant X_Pos := 39;

   package Test_LEDs is new TLC5940 (Test_Device, Test_Display);
   use Test_LEDS;
   Last_LED : constant LED_Channels := 7;
   -- Last discrete LED excludes seven segment display

   Display_Segment : constant Segment_Arrays := (8, 9, 10, 11, 12, 13, 14, 15);

   package Correction_IO is new Modular_IO (Corrections);
   package Greyscale_IO is new Modular_IO (Greyscales);

   task Octal_Clock is
      entry Start;
      entry Stop;
      entry End_Tests;
   end Octal_Clock;

   procedure Display_Driver_State is

      Y_Offset : Y_Pos := 0;

   begin -- Display_Driver_State
      for Chip in Test_Device loop
         Goto_XY (0, Y_Offset);
         Put ("Device: " & Test_Device'Image (Chip));
         if Get_Blanking then
            Put (" Blanked");
         else
            Put (" Lit");
         end if; -- Get_Blanking
         Goto_XY (0, Y_Offset + 1);
         Put ("GS:");
         for LED_Chanel in LED_Channels loop
            Greyscale_IO.put (Get_Greyscale (Chip, LED_Chanel), 5);
         end loop; -- for LED_Chanel in LED_Channels
         Goto_XY (0, Y_Offset + 2);
         Put ("DC:");
         for LED_Chanel in LED_Channels loop
            Correction_IO.put (Get_Correction (Chip, LED_Chanel), 5);
         end loop; -- for LED_Chanel in LED_Channels
         Y_Offset := Y_Offset + 3;
      end loop; -- Chip in Test Device
      Goto_XY (0, Y_Offset);
      Put ("Ambient Light:");
      Greyscale_IO.put (Get_Ambient_Light, 5);
   end Display_Driver_State;

   procedure Full_Correction is

      Setting_Required : Boolean := False;

   begin -- Full_Correction
      for Chip in Test_Device loop
         for LED_Chanel in LED_Channels loop
            Setting_Required := Setting_Required or
              Get_Correction (Chip, LED_Chanel) /= Corrections'Last;
         end loop; -- for LED_Chanel in LED_Channels
      end loop; -- Chip in Test Device
      if Setting_Required then
         for Chip in Test_Device loop
            for LED_Chanel in LED_Channels loop
               Set_Correction (Chip, LED_Chanel, Corrections'Last);
            end loop; -- for LED_Chanel in LED_Channels
         end loop; -- Chip in Test Device
         Write_Corrections;
         Delay 1.0;
      end if; -- Setting_Required
   end Full_Correction;

   procedure Zero_Corrections is

      Setting_Required : Boolean := False;

   begin -- Zero_Corrections
      for Chip in Test_Device loop
         for LED_Chanel in LED_Channels loop
            Setting_Required := Setting_Required or
              Get_Correction (Chip, LED_Chanel) /= Corrections'First;
         end loop; -- for LED_Chanel in LED_Channels
      end loop; -- Chip in Test Device
      if Setting_Required then
         for Chip in Test_Device loop
            for LED_Chanel in LED_Channels loop
               Set_Correction (Chip, LED_Chanel, Corrections'First);
            end loop; -- for LED_Chanel in LED_Channels
         end loop; -- Chip in Test Device
         Write_Corrections;
         Delay 1.0;
      end if; -- Setting_Required
   end  Zero_Corrections;

   procedure All_Corrections (Correction : in Corrections) IS

   begin --  All_Corrections
      for Chip in Test_Device loop
         for LED_Chanel in LED_Channels loop
            Set_Correction (Chip, LED_Chanel, Correction);
         end loop; -- for LED_Chanel in LED_Channels
      end loop; -- Chip in Test Device
      Write_Corrections;
   end  All_Corrections;

   task body Octal_Clock is

      Run_Clock : Boolean := False;
      Tests_Running : Boolean := True;
      Seconds_Count : Natural;
      Last_Lit : LED_Channels;
      Next_Time : Time;
      Minimum_Brightness : constant Greyscales := 1;
      Auto_Brightness : Greyscales;

   begin -- Octal_Clock
      Next_Time := Clock;
      Seconds_Count := Natural (Seconds (Next_Time));
      Next_Time := Time_Of (Year (Next_Time),
                            Month (Next_Time),
                            Day (Next_Time),
                            Day_Duration (Seconds_Count));
      -- time generally in the past so the loop runs immediately with no
      -- fractional part of seconds equal to zero.
      while Tests_Running loop
         select
            accept Start do
               Full_Correction;
               Set_Correction (Display, 0, 15);
               Write_Corrections;
               for LED_Chanel in LED_Channels range
                 LED_Channels'First  .. Last_LED loop
                  Set_Greyscale (LEDs, LED_Chanel, Greyscales'First);
               end loop; -- for LED_Chanel in LED_Channels range ...
               Set_Digit (Digit_One, 8, Greyscales'First, True);
               Write_LEDs;
               Run_Clock := True;
            end Start;
         or
            accept Stop do
               Run_Clock := False;
            end Stop;
         or
            accept End_Tests  do
               Tests_Running := False;
            end End_Tests;
         or
            delay until Next_Time;
            if Run_Clock then
               Seconds_Count := Natural (Seconds (Next_Time)) mod 60;
               Auto_Brightness := Get_Ambient_Light;
               if Auto_Brightness < Minimum_Brightness then
                  Auto_Brightness := Minimum_Brightness;
               end if; -- Auto_Brightness < Minimum_Brightness
               Set_Digit (Digit_One, Seconds_Count / 8, Auto_Brightness);
               Last_Lit := Seconds_Count mod 8;
               for I in reverse LED_Channels range
                 LED_Channels'First .. Last_LED loop
                  if I > Last_Lit then
                     Set_Greyscale (LEDs, I, Greyscales'First);
                  elsif I = Last_Lit then
                     Set_Greyscale (LEDs, I, Auto_Brightness);
                  else
                     Set_Greyscale (LEDs,
                                    I,
                                    Shift_Right (Get_Greyscale (LEDs, I + 1), 1)
                                   );
                  end if; -- I > Last_Lit
               end loop; -- I in LED_Channels reverse range ...
               Write_LEDs;
               Display_Driver_State;
            end if; -- Run_Clock
            Next_Time := Next_Time + 1.0;
         end select;
      end loop; -- Tests_Running
   end Octal_Clock;

begin -- Test_TLC5940
   Initialise_Digit (Digit_One, Display, Display_Segment);
   Initialise_Ambient_Light (Display, 0, Gen3);
   Light_LEDs;
   while Test_Requested /= '0' loop
      case Test_Requested is
         when '0' =>
            exit;
         when '1' =>
            -- All LEDs full brightness
            Full_Correction;
            for LED_Chanel in LED_Channels range
              LED_Channels'First .. Last_LED loop
               Set_Greyscale (LEDs, LED_Chanel, Greyscales'Last);
            end loop; -- for LED_Chanel in LED_Channels range ...
            Set_Digit (Digit_One, 8, Decimal_Lit => True);
            Write_LEDs;
         when '2' =>
            -- Light one LED
            declare

               LED_to_Light : LED_Channels := LED_Channels'First;

            begin -- test 2
               Full_Correction;
               if LED_to_Light < Last_LED then
                  LED_to_Light := LED_to_Light + 1;
               else
                  LED_to_Light := LED_Channels'First;
               end if; -- LED_to_Light < LED_Channels'Last
               for I in LED_Channels range LED_Channels'First .. Last_LED loop
                  if I = LED_to_Light then
                     Set_Greyscale (LEDs, I, Greyscales'Last);
                  else
                     Set_Greyscale (LEDs, I, Greyscales'First);
                  end if; -- I = LED_to_Light
               end loop; -- for I in LED_Channels
               Write_LEDs;
            end; -- test 2
         when '3' =>
            -- All LEDs half brightness
            for LED_Chanel in LED_Channels range
              LED_Channels'First .. Last_LED loop
               Set_Greyscale (LEDs, LED_Chanel, Greyscales'Last / 2);
            end loop; -- LED_Chanel in LED_Channels range ...
            Set_Digit (Digit_One, 8, Greyscales'Last / 2, True);
            Write_LEDs;
         when '4' =>
            -- All Corections to half
            All_Corrections (Corrections'Last / 2);
            Write_Corrections;
         when '5' =>
            Full_Correction;
            -- ramp led prightness
            for LED_Chanel in LED_Channels range
              LED_Channels'First  .. Last_LED loop
               Set_Greyscale (LEDs, LED_Chanel, Greyscales'First);
            end loop; -- for LED_Chanel in LED_Channels range ...
            Set_Digit (Digit_One, 8, Greyscales'First, True);
            Write_LEDs;
            Delay 1.0;
            for Brightness in Greyscales loop
               for LED_Chanel in LED_Channels range
                 LED_Channels'First .. Last_LED loop
                  Set_Greyscale (LEDs, LED_Chanel, Brightness);
               end loop; -- for LED_Chanel in LED_Channels range ...
               Set_Digit (Digit_One, 8, Brightness, True);
               Write_LEDs;
               Delay 0.01;
            end loop; -- Brighness in Greyscales
         when '6' =>
            -- step with tail
            Full_Correction;
            declare -- test 6
               subtype Brighness_Indices is Natural range
                 LED_Channels'First .. Last_LED + 12;
               Brightness_Array : array (Brighness_Indices) of Greyscales :=
                 (Greyscales'Last, others => Greyscales'First);
            begin -- test-- test 6
               for LED_Chanel in LED_Channels range
                 LED_Channels'First .. Last_LED loop
                  Set_Greyscale (LEDs, LED_Chanel, Greyscales'First);
               end loop; -- for LED_Chanel in LED_Channels range ...
               Set_Digit (Digit_One, 8, Greyscales'First, True);
               Write_LEDs;
               Delay 1.0;
               For Brightness_Index in Brighness_Indices loop
                  for Tail_index in reverse Brighness_Indices range
                    Brighness_Indices'First .. Brightness_Index - 1 loop
                     Brightness_Array (Tail_index) :=
                       Brightness_Array (Tail_index + 1) / 2;
                  end loop; -- Tail_index in reverse Brighness_Indices
                  for LED_Index in LED_Channels range
                    LED_Channels'First .. Last_LED loop
                     Set_Greyscale (LEDs, LED_Index,
                                    Brightness_Array (LED_Index));
                  end loop; -- LED_Index in LED_Channels range ...
                  Write_LEDs;
                  Delay 1.0;
                  if Brightness_Index < Brighness_Indices'Last then
                     Brightness_Array (Brightness_Index + 1) :=
                       Brightness_Array (Brightness_Index);
                  end if; -- Brightness_Index < Brighness_Indices'Last
               end loop; -- LED_Index in Brighness_Indices
            end; -- test-- test 6
         when '7' =>
            -- step through 7 segment characters
            Full_Correction;
            for LED_Chanel in LED_Channels range
              LED_Channels'First .. Last_LED loop
               Set_Greyscale (LEDs, LED_Chanel, Greyscales'First);
            end loop; -- for LED_Chanel in LED_Channels range ...
            for DP in Boolean loop
               For Number in Hex_Digits loop
                  Set_Digit (Digit_One, Number, Decimal_Lit => DP);
                  Write_LEDs;
                  delay 1.0;
               end loop; -- Number in Hex_Digits
            end loop; -- DP in Boolean
         when '8' =>
            Blank_LEDs;
         when '9' =>
            Light_LEDs;
         when 'a' | 'A' =>
            Full_Correction;
            for LED_Chanel in LED_Channels range
                 LED_Channels'First .. Last_LED loop
               Set_Greyscale (LEDs, LED_Chanel, Greyscales'Last);
            end loop; -- for LED_Chanel in LED_Channels range ...
            Set_Digit (Digit_One, 8, Decimal_Lit => True);
            Write_LEDs;
            delay 1.0;
            Zero_Corrections;
            for Correction in Corrections loop
               for Chip in Test_Device loop
                  for LED_Chanel in LED_Channels loop
                     Set_Correction (Chip, LED_Chanel, Correction);
                  end loop; -- LED_Chanel in LED_Channels
               end loop; -- Chip in Test_Device
               Write_Corrections;
               Delay 0.48;
            end loop; -- Brighness in Greyscales
         when 'b' | 'B' =>
            Full_Correction;
            for LED_Chanel in LED_Channels range
                 LED_Channels'First .. Last_LED loop
               Set_Greyscale (LEDs, LED_Chanel, Greyscales'Last);
            end loop; -- for LED_Chanel in LED_Channels range ...
            Set_Digit (Digit_One, 8, Decimal_Lit => True);
            Write_LEDs;
            delay 1.0;
            Zero_Corrections;
            for Chip in Test_Device loop
               for LED_Chanel in LED_Channels loop
                  Set_Correction (Chip, LED_Chanel, Corrections'Last);
                  if LED_Chanel - 1 in LED_Channels then
                     Set_Correction (Chip, LED_Chanel - 1,
                                     Corrections'First);
                  end if;
                  Write_Corrections;
                  delay 1.0;
               end loop; -- for LED_Chanel in LED_Channels
            end loop; -- Chip in Test_Device
         when 'c' | 'C' =>
            -- octal clock
            declare

               Stop_Clock : Character := ' ';

            begin -- test c
               Clear_Screen;
               Octal_Clock.Start;
               while Stop_Clock /= 'y' and Stop_Clock /= 'Y' loop
                  Goto_XY (Menu_Column_2, Menu_Start + 8);
                  Put ("Stop Clock? ");
                  Get (Stop_Clock);
               end loop; -- Stop_Clock /= 'y' and Stop_Clock /= 'Y'
               Octal_Clock.Stop;
            end; -- test c
         when others =>
            Goto_XY (Menu_Start, 9);
            Put ("Invalid Test Request");
            Bleep;
      end case; -- Test_Requested
      Display_Driver_State;
      Goto_XY (0, Menu_Start);
      Put ("0: End Tests");
      Goto_XY (Menu_Column_2, Menu_Start);
      Put ("1: Set all LEDs full brightness");
      Goto_XY (0, Menu_Start + 1);
      Put ("2: One LED on and step forward");
      Goto_XY (Menu_Column_2, Menu_Start + 1);
      Put ("3: Set all LEDs half brightness");
      Goto_XY (0, Menu_Start + 2);
      Put ("4: Set all Corections to half brightness");
      Goto_XY (Menu_Column_2, Menu_Start + 2);
      Put ("5: Ramp all LED Brightness");
      Goto_XY (0, Menu_Start + 3);
      Put ("6: Step with tail");
      Goto_XY (Menu_Column_2, Menu_Start + 3);
      Put ("7: Seven Segment Test");
      Goto_XY (0, Menu_Start + 4);
      Put ("8: Blank LEDS");
      Goto_XY (Menu_Column_2, Menu_Start + 4);
      Put ("9: Light LEDS");
      Goto_XY (0, Menu_Start + 5);
      Put ("A: Ramp Corrections");
      Goto_XY (Menu_Column_2, Menu_Start + 5);
      Put ("B: Step one full Correction others zero");
      Goto_XY (0, Menu_Start + 6);
      Put ("C: Octal Clock");
      Goto_XY (0, Menu_Start + 8);
      Put ("Test? ");
      Get (Test_Requested);
      Clear_Screen;
   end loop; -- while Test_Requested /= '0'
   Octal_Clock.End_Tests;

end Test_TLC5940;
