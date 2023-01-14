with Ada.text_IO, Ada.Calendar, Ada.Characters.Latin_1, RPI_GPIO;

use Ada.text_IO, Ada.Calendar, Ada.Characters.Latin_1, RPI_GPIO;

-- The first four GPIO pins perform a binary count and the last three read back
-- the first three. The binary count progresses to the maximum and is then
-- raps around. Steps are at 1 s intervals, press any key to stop. The output
-- and input pins are read producing a two line output, see example below:
-- 0110
-- 011
--
-- 1110
-- 111

procedure Test_GPIO is

   LED0 : constant GPIO_Pins:= Gen0;
   LED1 : constant GPIO_Pins:= Gen1;
   LED2 : constant GPIO_Pins:= Gen2;
   LED3 : constant GPIO_Pins:= Gen3;
   Read_LED0 : constant GPIO_Pins:= Gen4;
   Read_LED1 : constant GPIO_Pins:= Gen5;
   Read_LED2 : constant GPIO_Pins:= Gen6;
   Update_Interval : Duration := 1.0;
   Next_Time : Time;
   Character_Available : Boolean;
   Current_Character : character;
   Max_Seconds : constant Natural := 2#1111#;
   subtype Second_Counters is Natural range 0 .. Max_Seconds;
   Second_Count : Second_Counters;

   procedure Setup_Pins is

   begin -- Setup_Pins
      Bind_Pin (LED0, Out_Pin);
      Bind_Pin (LED1, Out_Pin);
      Bind_Pin (LED2, Out_Pin);
      Bind_Pin (LED3, Out_Pin);
      Bind_Pin (Read_LED0, In_Pin);
      Bind_Pin (Read_LED1, In_Pin);
      Bind_Pin (Read_LED2, In_Pin);
   end Setup_Pins;

   procedure Update_LEDS (Second_Count : in Second_Counters) is
   begin -- Update_LEDs
      case Second_Count is
         when 2#0000# =>
            Write_Pin (Pin_Low, LED0);
            Write_Pin (Pin_Low, LED1);
            Write_Pin (Pin_Low, LED2);
            Write_Pin (Pin_Low, LED3);
         when 2#0001# =>
            Write_Pin (Pin_High, LED0);
            Write_Pin (Pin_Low, LED1);
            Write_Pin (Pin_Low, LED2);
            Write_Pin (Pin_Low, LED3);
         when 2#0010# =>
            Write_Pin (Pin_Low, LED0);
            Write_Pin (Pin_High, LED1);
            Write_Pin (Pin_Low, LED2);
            Write_Pin (Pin_Low, LED3);
         when 2#0011# =>
            Write_Pin (Pin_High, LED0);
            Write_Pin (Pin_High, LED1);
            Write_Pin (Pin_Low, LED2);
            Write_Pin (Pin_Low, LED3);
         when 2#0100# =>
            Write_Pin (Pin_Low, LED0);
            Write_Pin (Pin_Low, LED1);
            Write_Pin (Pin_High, LED2);
            Write_Pin (Pin_Low, LED3);
         when 2#0101# =>
            Write_Pin (Pin_High, LED0);
            Write_Pin (Pin_Low, LED1);
            Write_Pin (Pin_High, LED2);
            Write_Pin (Pin_Low, LED3);
         when 2#0110# =>
            Write_Pin (Pin_Low, LED0);
            Write_Pin (Pin_High, LED1);
            Write_Pin (Pin_High, LED2);
            Write_Pin (Pin_Low, LED3);
         when 2#0111# =>
            Write_Pin (Pin_High, LED0);
            Write_Pin (Pin_High, LED1);
            Write_Pin (Pin_High, LED2);
            Write_Pin (Pin_Low, LED3);
         when 2#1000# =>
            Write_Pin (Pin_Low, LED0);
            Write_Pin (Pin_Low, LED1);
            Write_Pin (Pin_Low, LED2);
            Write_Pin (Pin_High, LED3);
         when 2#1001# =>
            Write_Pin (Pin_High, LED0);
            Write_Pin (Pin_Low, LED1);
            Write_Pin (Pin_Low, LED2);
            Write_Pin (Pin_High, LED3);
         when 2#1010# =>
            Write_Pin (Pin_Low, LED0);
            Write_Pin (Pin_High, LED1);
            Write_Pin (Pin_Low, LED2);
            Write_Pin (Pin_High, LED3);
         when 2#1011# =>
            Write_Pin (Pin_High, LED0);
            Write_Pin (Pin_High, LED1);
            Write_Pin (Pin_Low, LED2);
            Write_Pin (Pin_High, LED3);
         when 2#1100# =>
            Write_Pin (Pin_Low, LED0);
            Write_Pin (Pin_Low, LED1);
            Write_Pin (Pin_High, LED2);
            Write_Pin (Pin_High, LED3);
         when 2#1101# =>
            Write_Pin (Pin_High, LED0);
            Write_Pin (Pin_Low, LED1);
            Write_Pin (Pin_High, LED2);
            Write_Pin (Pin_High, LED3);
         when 2#1110# =>
            Write_Pin (Pin_Low, LED0);
            Write_Pin (Pin_High, LED1);
            Write_Pin (Pin_High, LED2);
            Write_Pin (Pin_High, LED3);
         when 2#1111# =>
            Write_Pin (Pin_High, LED0);
            Write_Pin (Pin_High, LED1);
            Write_Pin (Pin_High, LED2);
            Write_Pin (Pin_High, LED3);
      end case; -- Second Count
   end Update_LEDS;

begin -- Test_GPIO
   Put_Line ("Starting Pi GPIO Test");
   Flush;
   Setup_Pins;
   Put_Line ("Pins Exported and direction set");
   Flush;
   Next_Time := Clock + Duration (Update_Interval);
   Second_Count := 0;
   loop
      Put_Line (Second_Counters'image (Second_Count));
      Update_LEDS (Second_Count);
      if Second_Count < Max_Seconds then
         Second_Count := Second_Count + 1;
      else
         Second_Count := 0;
      end if; -- Second_Count < Max_Seconds
      delay until Next_Time;
      Next_Time := Next_Time + Duration (Update_Interval);
      for Pin in GPIO_Pins loop
         if Pin = Gen4 then
            New_Line;
         end if; -- Pin = Gen4
         if Read_Pin (Pin) then
            Put ('1');
         else
            Put ('0');
         end if; -- Read_Pin (Pin)
      end loop; -- for Pin in GPIO_Pins
      New_Line;
      New_Line; -- blank line between each read back group
      Get_Immediate (Current_Character, Character_Available);
      exit when Character_Available and then (Current_Character = 'x');
   end loop;
   Put_Line ("Normal Program Termination");
end Test_GPIO;
