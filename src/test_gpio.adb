with Ada.text_IO; use Ada.text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;
with RPI_GPIO; use RPI_GPIO;

-- Test program for rpi_gpio.adb (Ada) and gpio_driver.c (C language).
-- Origionaly for file system implementation of rpi_gpio, now uses libgpiod

-- Author    : David Haley
-- Created   : 2017
-- Last Edit : 03/08/2025
-- 20250803: Updated to test Release_Pin, Strobe_High and Strobe_Low
-- functionality.

procedure Test_GPIO is
   
   procedure Counter is

	-- The first three GPIO pins are set as outputs to perform a binary count
	-- and the last three GPIO pins set as inputs to read back the first three.
	-- The binary count progresses to 7 and raps around. Steps are at 1 s
	-- intervals, press 'x' key to stop test. The output and input pins are read
	-- producing a three line output, see example below:
	-- Count: 3
	-- 011 Clock: 0
	-- 0110
	-- Count 4
	-- 100 Clock: 0
	-- 1000
	-- The Gen3 output provides a narrow clock pulse at 500 ms after the other
	-- outputs change state. Pulse polarity inverte every eight cycles.
	-- Test wiring required on the 40 pin header (RPI 2, 3 and 4)
	-- Gen0 (GPIO 17) 11 to 16 Gen4 (GPIO 23)
	-- Gen1 (GPIO 18) 12 to 18 Gen5 (GPIO 24)
	-- Gen2 (GPIO 27) 13 to 22 Gen6 (GPIO 25)
	-- Gen3 15 monitor with an oscilliscope
	-- ground pins 6, 9, 14, 20, 25, 30, 34, 39,
		  
      LED0 : constant GPIO_Pins:= Gen0;
      LED1 : constant GPIO_Pins:= Gen1;
      LED2 : constant GPIO_Pins:= Gen2;
      Clk : constant GPIO_Pins:= Gen3;
      Read_LED0 : constant GPIO_Pins:= Gen4;
      Read_LED1 : constant GPIO_Pins:= Gen5;
      Read_LED2 : constant GPIO_Pins:= Gen6;
      
      type Second_Counters is mod 8;

      procedure Setup_Pins is

      begin -- Setup_Pins
         Bind_Pin (LED0, Out_Pin);
         Bind_Pin (LED1, Out_Pin);
         Bind_Pin (LED2, Out_Pin);
         Bind_Pin (Clk, Out_Pin);
         Bind_Pin (Read_LED0, In_Pin);
         Bind_Pin (Read_LED1, In_Pin);
         Bind_Pin (Read_LED2, In_Pin);
      end Setup_Pins;

      procedure Update_LEDS (Second_Count : in Second_Counters) is
      begin -- Update_LEDs
         case Second_Count is
            when 2#000# =>
               Write_Pin (Pin_Low, LED0);
               Write_Pin (Pin_Low, LED1);
               Write_Pin (Pin_Low, LED2);
            when 2#001# =>
               Write_Pin (Pin_High, LED0);
               Write_Pin (Pin_Low, LED1);
               Write_Pin (Pin_Low, LED2);
            when 2#010# =>
               Write_Pin (Pin_Low, LED0);
               Write_Pin (Pin_High, LED1);
               Write_Pin (Pin_Low, LED2);
            when 2#011# =>
               Write_Pin (Pin_High, LED0);
               Write_Pin (Pin_High, LED1);
               Write_Pin (Pin_Low, LED2);
            when 2#100# =>
               Write_Pin (Pin_Low, LED0);
               Write_Pin (Pin_Low, LED1);
               Write_Pin (Pin_High, LED2);
            when 2#101# =>
               Write_Pin (Pin_High, LED0);
               Write_Pin (Pin_Low, LED1);
               Write_Pin (Pin_High, LED2);
            when 2#110# =>
               Write_Pin (Pin_Low, LED0);
               Write_Pin (Pin_High, LED1);
               Write_Pin (Pin_High, LED2);
            when 2#111# =>
               Write_Pin (Pin_High, LED0);
               Write_Pin (Pin_High, LED1);
               Write_Pin (Pin_High, LED2);
         end case; -- Second Count
      end Update_LEDS;
      
      Update_Interval : constant Duration := 0.5;
      Next_Time : Time;
      Character_Available : Boolean;
      Current_Character : character;
      Second_Count : Second_Counters := 0;
      Pulse_Polarity : Boolean := False;
      
   begin -- Counter
      Put_Line ("Starting Counter text, press 'x' to exit.");
      Setup_Pins;
      Put_Line ("Pins Bound and directions set");
      Write_Pin (not Pulse_Polarity, Clk);
      Put_Line ("Clock pin set");
      Next_Time := Clock + Update_Interval;
      loop
         Put_Line ("Count:" & Second_Count'Img);
         Update_LEDS (Second_Count);
         delay until Next_Time;
         Next_Time := Next_Time + Update_Interval;
         if Pulse_Polarity then
            Strobe_High (Clk);
         else
            Strobe_Low (Clk);
         end if; -- Pulse_Polarity
         for Pin in reverse GPIO_Pins loop
            if Pin = Clk then
               Put (" Clock: ");
               if Read_Pin (Clk) then
                  Put_Line ("1");
               else
                  Put_Line ("0");
               end if; -- Read_Pin (Clk)
            end if; -- Pin = Clk
            if Read_Pin (Pin) then
               Put ('1');
            else
               Put ('0');
            end if; -- Read_Pin (Pin)
         end loop; -- for Pin in GPIO_Pins
         New_Line;
         Get_Immediate (Current_Character, Character_Available);
         exit when Character_Available and then (Current_Character = 'x');
         Second_Count := Second_Count + 1;
         if Second_Count = 0 then
            Pulse_Polarity := not Pulse_Polarity;
            Write_Pin (not Pulse_Polarity, Clk);
         end if; -- Second_Count = 0
         delay until Next_Time;
         Next_Time := Next_Time + Update_Interval;
      end loop;
      for Pin in GPIO_Pins loop
         Release_Pin (Pin);
      end loop; -- Pin in GPIO_Pins
   end Counter;
   
   function Menu return Character is
   
   Choice : Character;
   
   begin -- Menu
      Put_Line ("Counter c");
      Put_Line ("Bind Input i");
      Put_Line ("Bind Output o");
      Put_Line ("Read Input r");
      Put_Line ("Set Output High h");
      Put_Line ("Set Output Low l");
      Put_Line ("Pules Output p");
      Put_Line ("Release Line z");
      Put_Line ("Exit e");
      Put ("Command: ");
      get (Choice);
      return Choice;
   end Menu;
   
   function Get_Pin return GPIO_Pins is
      
      package Pin_IO is new Ada.Text_IO.Enumeration_IO (GPIO_Pins);
   
      Pin : GPIO_Pins;
      Valid : Boolean := True;
      
   begin -- Get_Pin
      loop -- until valid input;
         Put ("Enter pin name [genx where 0 <= x <= 6] : ");
         begin -- Get pin exception block
            Pin_IO.Get (Pin);
         exception
            when others =>
               Valid := False;
               Put_Line ("Invalid Pin Name");
         end; -- Get pin exception block
         exit when Valid;
         Valid := True;
      end loop; -- until valid input;
      return Pin;
   end Get_Pin;
   
   Run : Boolean := True;
   Pin : GPIO_Pins;

begin -- Test_GPIO
   while Run loop
      begin -- GPIO exception block
         case Menu is
            when 'c' | 'C' =>
               Counter;
            when 'i' | 'I' =>
               Bind_Pin (Get_Pin, In_Pin);
            when 'o' | 'O' =>
               Bind_Pin (Get_Pin, Out_Pin);
            when 'r' | 'R' =>
               Pin := Get_Pin;
               Put (Pin'Img & " = ");
               if Read_Pin (Pin) then
                  Put_Line ("1");
               else
                  Put_Line ("0");
               end if; -- Read_Pin (Pin)
            when 'h' | 'H' =>
               Write_Pin (Pin_High, Get_Pin);
            when 'l' | 'L' =>
               Write_Pin (Pin_Low, Get_Pin);
            when 'p' | 'P' =>
               Pin := Get_Pin;
               if Read_Pin (Pin) then
                  Put_Line ("Minnimum duration low");
                  Strobe_Low (Pin);
               else
                  Put_Line ("Minimum duration high");
                  Strobe_High (Pin);
               end if; -- Read_Pin (Pin)
            when 'z' | 'Z' =>
               Release_Pin (Get_Pin);
            when 'e' | 'E' =>
               Run := False;
            when others =>
               Put_Line ("Invalid Menu Choice");
         end case; -- Menu
      exception
         when E : Pin_Bound | Pin_not_Bound | Pin_General_Failure |
           Pin_Illegal_Operation =>
            Put_Line (Exception_Information (E));
         when F: others =>
            Put_Line ("Tests terminated due to unhandled exception");            
            Put_Line (Exception_Information (F));
            Run := False;
      end; -- GPIO exception block
   end loop; -- Run
end Test_GPIO;
