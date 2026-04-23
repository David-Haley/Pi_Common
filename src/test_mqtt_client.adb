--  Program to test MQTT_Client package.

--  Author    : David Haley
--  Created   : 12/03/2026
--  Last_Edit : 23/04/2026

--  20260426 : Loop back test added.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with MQTT_Client; use MQTT_Client;

procedure Test_MQTT_Client is

   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);

   Topic_1 : constant String := "development/test_1";
   Topic_2 : constant String := "development/test_2";
   Topic_Loop : constant String := "development/test_loop";
   Tx_1, Rx_1, Tx_2, Rx_2, Tx_Loop, Rx_Loop : MQTT_Handle;
   Run_Test : Boolean := True;
   Command : Character;
   Wait : Timeouts := 60.0; -- Wait 60 s for receive;
   Loop_Count, Loop_Limit, Returned_Value, Last : Positive;
   --  32 bit number tepresented in hex 16#...#
   Tx_Buffer : String (1 .. 11);

begin -- Test_MQTT_Client
   Put_Line ("Test MQTT client version 20260423");
   if Argument_Count < 3 then
      raise Program_Error with 
         "Missing Broker_Host_Name, User_Name and Password";
   end if; -- Argument_Count < 3
   Put_Line ("The broker " & Argument (1) & " must support topics " &
             Topic_1 & " and " & Topic_2);
   Connect_Tx (Argument (1), Argument (2), Argument (3), Topic_1, Tx_1);
   Put_Line (Topic_1 & " connected for publish");
   Connect_Rx (Argument (1), Argument (2), Argument (3), Topic_1, Rx_1);
   Put_Line (Topic_1 & " connected for subscribe");
   Connect_Tx (Argument (1), Argument (2), Argument (3), Topic_2, Tx_2);
   Put_Line (Topic_2 & " connected for publish");
   Connect_Rx (Argument (1), Argument (2), Argument (3), Topic_2, Rx_2);
   Put_Line (Topic_1 & " connected for subscribe");
   --  For loop back testing QoS is set to two, verify message sent once only!
   Connect_Tx (Argument (1), Argument (2), Argument (3), Topic_Loop, Tx_Loop);
   Put_Line (Topic_loop & " connected for publish");
   Connect_Rx (Argument (1), Argument (2), Argument (3), Topic_Loop, Rx_Loop);
   Put_Line (Topic_loop & " connected for subscribe");
   while Run_Test loop
      Put_Line ("A: Send " & Topic_1);
      Put_Line ("B: Receive " & Topic_1);
      Put_Line ("C: Send " & Topic_2);
      Put_Line ("D: Receive " & Topic_2);
      Put_Line ("L: Loop test " & Topic_Loop);
      Put_Line ("0: Exit");
      Get_Immediate (Command);
      Put_Line ("Command was: '" & Command & "'");
      case Command is
         when 'a' | 'A' =>
            Put ("Message to send to " & Topic_1 & " >");
            Send (Tx_1, Get_Line);
            New_Line;
         when 'b' | 'B' =>
            Put_Line ("Received message from " & Topic_1 & " """ &
                      Receive (Rx_1, Wait) & """");
         when 'c' | 'C' =>
            Put ("Message to send to " & Topic_2 & " >");
            Send (Tx_2, Get_Line);
            New_Line;
         when 'd' | 'D' =>
            Put_Line ("Received message from " & Topic_2 & " """ &
                      Receive (Rx_2, Wait) & """");
         when 'l' | 'L' =>
            Put ("Length of loop test: ");
            Positive_IO.Get (Loop_Limit);
            Loop_Count := 1;
            loop -- One loop test
               Positive_IO.Put (Tx_Buffer, Loop_Count, 16);
               Send (Tx_Loop, Tx_Buffer);
               declare -- Receive_String declation block
                  Receive_String : String := Receive (Rx_Loop, 2.0);
               begin -- Receive_String declation block
                  if Receive_String'Length > 0 then
                     Positive_IO.Get (Receive_String, Returned_Value, Last);
                  else
                     Put_Line ("Recieve timeout");
                     exit;
                  end if; -- Receive_String'Length > 0
               end; -- Receive_String declation block
               if Loop_Count mod (Loop_Count / 80) = 0 then
                  Put ('#');
               end if; -- Loop_Count mod (Loop_Count / 80) = 0
               exit when Loop_Count = Loop_Limit or else Returned_Value /= Loop_Count;
               Loop_Count := @ + 1;
            end loop; -- One loop test
            if Loop_Count = Loop_Limit and Returned_Value = Loop_Count then
               Put_Line ("Loop test passed");
            else
               Put_Line ("Loop test failed");
            end if; -- Loop_Count = Loop_Limit and Returned_Value = Loop_Count
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
   Disconnect (Tx_2);
   Put_Line (Topic_2 & " disconnected for publish");
   Disconnect (Rx_2);
   Put_Line (Topic_2 & " unsubscribed");
   Disconnect (Tx_Loop);
   Put_Line (Topic_Loop & " disconnected for publish");
   Disconnect (Rx_Loop);
   Put_Line (Topic_Loop & " unsubscribed");
end Test_MQTT_Client;