--  Simple Subscriber that provides a blocking functions that returns a array
--  of Unsigned_8 (bytes) or a String;
--  Is dependent on libmosquitto.

--  Author    : David Haley
--  Created   : 15/02/2026
--  Last_Edit : 04/03/2026

generic

   Topic : String;

package MQTT.Subscriber is

   procedure Connect (Broker_Host : String;
                      User_Name : String;
                      Password : String;
                      QoS : QoSs := 0;
                      Keep_Alive_Time : Keep_Alive_Times := 60);
   --  connects to broker and subcribes to Topic.
   --  Can only be called once, unless there is a previous call to Disconnect.

   --  
   function Receive return MQTT_Data;
   --  Blocking function, will not return unless a message is received.
   --  Returns an array of binary data.

   function Receive return String;
   --  Blocking function, will not return unless a message is received.
   --  Returns a string.

   procedure Disconnect;
   --  Disconnects and cleans up.
   
end  MQTT.Subscriber;