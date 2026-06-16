--  Program to test MQTT_Client package.

--  Author    : David Haley
--  Created   : 12/03/2026
--  Last_Edit : 15/06/2026

--  20260524 : Connected functions included
--  20260427 : Updated to MQTT_Client changes
--  20260424 : Loop back test and the ability to set QoS added.

--  20250615 : Removal of some compiler warnings.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Real_Time; use Ada.Real_Time;
with MQTT_Client; use MQTT_Client;

procedure Test_MQTT_Client is

   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);

   Topic_1 : constant String := "development/test_1";
   Topic_2 : constant String := "development/test_2";
   Topic_Loop : constant String := "development/test_loop";
   Tx_1, Rx_1, Tx_2, Rx_2, Tx_Loop, Rx_Loop : MQTT_Handle;
   QoS : QoSs;
   Run_Test : Boolean := True;
   Command : Character;
   Discard_Time : Timeouts := Seconds (60);
   -- Message stale after 60 s for receive;
   Loop_Count, Loop_Limit : Positive;
   --  32 bit number represented in hex 16#...#
   Tx_Buffer : String (1 .. 11);
   Loop_Test_Result : Boolean;
   Connection_Delay : constant Duration := 0.001;

begin -- Test_MQTT_Client
   Put_Line ("Test MQTT client version 20260427");
   if Argument_Count < 3 then
      raise Program_Error with 
         "Missing Broker_Host_Name, User_Name and Password";
   end if; -- Argument_Count < 3
   if Argument_Count = 4 then
      QoS := QoSs'Value (Argument (4));
   else
      QoS := 0;
   end if; -- Argument_Count = 4
   Put_Line ("The broker " & Argument (1) & " must support topics " &
             Topic_1 & ", " & Topic_2 & " and " & Topic_Loop);
   Connect_Tx (Argument (1), Argument (2), Argument (3), Topic_1, Tx_1, QoS);
   while not Is_Connected_Tx (Tx_1) loop
      Put_Line ("Waiting for " & Topic_1 & " connection");
      delay Connection_Delay;
   end loop; -- not Is_Connected_Tx (Tx_1)
   Put_Line (Topic_1 & " connected for publish");
   Connect_Rx (Argument (1), Argument (2), Argument (3), Topic_1, Rx_1, QoS);
   while not Is_Connected_Rx (Rx_1) loop
      Put_Line ("Waiting for " & Topic_1 & " connection");
      delay Connection_Delay;
   end loop; -- not Is_Connected_Rx (Rx_1)
   Put_Line (Topic_1 & " connected for subscribe");
   Connect_Tx (Argument (1), Argument (2), Argument (3), Topic_2, Tx_2, QoS);
   while not Is_Connected_Tx (Tx_2) loop
      Put_Line ("Waiting for " & Topic_2 & " connection");
      delay Connection_Delay;
   end loop; -- not Is_Connected_Tx (Tx_2)
   Put_Line (Topic_2 & " connected for publish");
   Connect_Rx (Argument (1), Argument (2), Argument (3), Topic_2, Rx_2, QoS);
   while not Is_Connected_Rx (Rx_2) loop
      Put_Line ("Waiting for " & Topic_2 & " connection");
      delay Connection_Delay;
   end loop; -- not Is_Connected_Rx (Rx_2)
   Put_Line (Topic_1 & " connected for subscribe");
   --  For loop back testing QoS is set to two, verify message sent once only!
   Connect_Tx (Argument (1), Argument (2), Argument (3), Topic_Loop, Tx_Loop,
               QoS);
   while not Is_Connected_Tx (Tx_Loop) loop
      Put_Line ("Waiting for " & Topic_Loop & " connection");
      delay Connection_Delay;
   end loop; -- not Is_Connected_Tx (Tx_Loop)
   Put_Line (Topic_Loop & " connected for publish");
   Connect_Rx (Argument (1), Argument (2), Argument (3), Topic_Loop, Rx_Loop,
               QoS);
   while not Is_Connected_Rx (Rx_Loop) loop
      Put_Line ("Waiting for " & Topic_Loop & " connection");
      delay Connection_Delay;
   end loop; -- not Is_Connected_Rx (Rx_Loop)
   Put_Line (Topic_Loop & " connected for subscribe");
   while Run_Test loop
      Put_Line ("A: Send " & Topic_1);
      Put_Line ("B: Receive " & Topic_1);
      Put_Line ("C: Send " & Topic_2);
      Put_Line ("D: Receive " & Topic_2);
      Put_Line ("L: Loop test " & Topic_Loop);
      Put_Line ("0: Exit");
      Put ("Command >");
      Get_Immediate (Command);
      New_Line;
      Put_Line ("Command was: '" & Command & "'");
      case Command is
         when 'a' | 'A' =>
            Put ("Message to send to " & Topic_1 & " >");
            Send (Tx_1, Get_Line);
            New_Line;
         when 'b' | 'B' =>
            Put_Line ("Received message from " & Topic_1 & " """ &
                      Receive (Rx_1, Discard_Time) & """");
         when 'c' | 'C' =>
            Put ("Message to send to " & Topic_2 & " >");
            Send (Tx_2, Get_Line);
            New_Line;
         when 'd' | 'D' =>
            Put_Line ("Received message from " & Topic_2 & " """ &
                      Receive (Rx_2, Discard_Time) & """");
         when 'l' | 'L' =>
            Put ("Length of loop test: ");
            Positive_IO.Get (Loop_Limit);
            Loop_Count := 1;
            loop -- One loop test
               Positive_IO.Put (Tx_Buffer, Loop_Count, 16);
               Send (Tx_Loop, Tx_Buffer);
               declare -- Receive_String declation block
                  Receive_String : constant String :=
                    Receive_Blocking (Rx_Loop, Seconds (2), Seconds (1));
               begin -- Receive_String declation block
                  if Receive_String'Length = 0 then
                     Put_Line ("Recieve timeout");
                  end if; -- Receive_String'Length > 0
                  Loop_Test_Result := Tx_Buffer = Receive_String;
               end; -- Receive_String declation block
               if (Loop_Count * 80) mod Loop_Limit = 0 then
                  Put ('#');
               end if; -- (Count_Limit / 80) mod Count_Limit = 0
               exit when not Loop_Test_Result or Loop_Count = Loop_Limit;
               Loop_Count := @ + 1;
            end loop; -- One loop test
            New_Line;
            if Loop_Test_Result then
               Put_Line ("Loop test passed");
            else
               Put_Line ("Loop test failed at" & Loop_Count'Img);
            end if; -- Loop_Test_Result
         when '0' =>
            Run_Test := False;
         when others =>
            Put_Line ("invalid command '" & Command & "'");
      end case; -- Command
   end loop; -- Run_Test
   Disconnect (Tx_1);
   Put_Line (Topic_1 & " disconnected for publish");
   Disconnect ( Rx_1);
   Put_Line (Topic_1 & " unsubscribed");
   Disconnect (Tx_2);
   Put_Line (Topic_2 & " disconnected for publish");
   Disconnect (Rx_2);
   Put_Line (Topic_2 & " unsubscribed");
   Disconnect (Tx_Loop);
   Put_Line (Topic_Loop & " disconnected for publish");
   Disconnect (Rx_Loop);
   Put_Line (Topic_Loop & " unsubscribed");
end Test_MQTT_Client;