--  Program to test MQTT.Subscriber package.

--  Author    : David Haley
--  Created   : 03/02/2026
--  Last_Edit : 04/03/2026

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with MQTT; use MQTT;
with MQTT.Subscriber;
with Linux_Signals;

procedure Test_MQTT_Subscriber is

begin -- Test_MQTT_Subscriber
   if Argument_Count < 4 then
      raise Program_Error with 
         "Missing Topic Broker_Name, User_Name and Password";
   end if; -- Argument_Count < 4
   Linux_Signals.Handlers.Install;
   declare -- Subscriber declaration block
      package Subcriber is new MQTT.Subscriber (Argument (1));
      use Subcriber;
   begin -- Subscriber declaration block
      Connect (Argument (2), Argument (3), Argument (4));
      if Argument_Count < 5 then
         loop -- receive and check for termination
            declare  -- receive declaration block
               Text : String := Receive;
            begin  -- receive declaration block
               Put_line ("Received string: """ & Text & '"');
            end; -- receive declaration block
            exit when Linux_Signals.Handlers.Signal_Stop or
              Linux_Signals.Ctrl_C_Stop;
         end loop; -- receive and check for termination
      elsif  Argument_Count >= 5 and then
        (Argument (5) = "B" or Argument (5) = "b")
      then
         loop -- receive and check for termination
            declare -- receive declaration block
               Data : MQTT_Data := Receive;
               --  16#xx#
               --  123456
               Hex : String (1 .. 6);
            begin -- receive declaration block
               Put ("Received hex: [");
               for I in MQTT_Indices range Data'First .. Data'Last loop
                  MQTT_Data_IO.Put (Hex, Data (I), 16);
                  Put (' ' & Hex (4 .. 5));
               end loop;
               Put_Line ("]");
            end; -- receive declaration block
            exit when Linux_Signals.Handlers.Signal_Stop or
              Linux_Signals.Ctrl_C_Stop;
         end loop; -- receive and check for termination
      end if; -- Argument_Count < 5
      Disconnect;
   end; -- Subscriber declaration block
   Linux_Signals.Handlers.Remove;
   Put_Line ("Normal exit");
end Test_MQTT_Subscriber;