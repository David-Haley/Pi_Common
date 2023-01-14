-- This program tests Linux_Signal and is intended to be loaded as a service.
-- Approximately once per minute it writes to a file.
-- Author    : David Haley
-- Created   : 28/05/2022
-- Last Edit : 28/05/2022

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with Linux_Signals; use Linux_Signals;

procedure Test_Service is

   Next_Time : Time := Clock;
   Log_File : File_Type;
   Second_Count : Natural := 0;

begin -- Test_Service
   Create (Log_File, Out_File, "Test_Service_Log_File.txt");
   Put_Line (Log_File, Time_String (Next_Time));
   Put_Line (Log_File, Command_Name);
   Put_Line (Log_File, Current_Directory);
   for I in Positive range 1 .. Argument_Count loop
      Put_Line (Log_File, I'Img & ": """ & Argument (I) & '"');
   end loop; -- I in Positive range 1 .. Argument_Count
   Handlers.Install;
   loop
      Next_Time := Next_Time + 1.0;
      delay until Next_Time;
      Second_Count := Second_Count + 1;
      if Second_Count >= 60 then
         Second_Count := 0;
         Put_Line (Log_File, Time_String);
      end if; -- Second_Count >= 60
      exit when Handlers.Signal_Stop or Ctrl_C_Stop;
   end Loop;
   if Handlers.Signal_Stop then
      Put_Line (Log_File, "Signal stop " & Time_String);
   end if; -- Handlers.Signal_Stop
   if Ctrl_C_Stop then
      Put_Line (Log_File, "Ctrl C stop " & Time_String);
   end if; -- Ctrl_C_Stop
   Handlers.Remove;
   Close (Log_File);
   Set_Exit_Status (Success);
end Test_Service;
