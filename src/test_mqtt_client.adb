--  Program to test MQTT_Client package.

--  Author    : David Haley
--  Created   : 12/03/2026
--  Last_Edit : 15/03/2026

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with MQTT_Client; use MQTT_Client;

procedure Test_MQTT_Client is
   Topic_1 : String := "development/test_1";
   Topic_2 : String := "development/test_2";
   Tx_1, Rx_1, Tx_2, Rx_2 : MQTT_Handle;
   Run_Test : Boolean := True;
   Command : Character;
   Wait : Timeouts := 60.0; -- Wait 60 s for receive;

begin -- Test_MQTT_Client
   Put_Line ("Test MQTT client version 20260312");
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
   Put_Line (Topic_2 & " connected for subscribe");
   while Run_Test loop
      Put_Line ("A: Send " & Topic_1);
      Put_Line ("B: Receive " & Topic_1);
      Put_Line ("C: Send " & Topic_2);
      Put_Line ("D: Receive " & Topic_2);
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
end Test_MQTT_Client;