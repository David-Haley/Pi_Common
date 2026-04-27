-- This package allows the creation of multiple publishers and subdcribers.

--  Author    : David Haley
--  Created   : 09/03/2026
--  Last_Edit : 26/04/2026

--  20260426 : Receive made non blocking, libmosquitto buffers subscribed
--  topics so strictly the buffering in this unit was not required, thus
--  eliminating the need of an additional thread per subscribed topic.

with Ada.Finalization; use Ada.Finalization;
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces.C; use Interfaces.C;

package MQTT_Client is

   MQTT_Error : exception;

   subtype QoSs is int range 0 .. 2; -- As defined by standards

   subtype Timeouts is Time_Span;

   subtype Keep_Alive_Times is int range 1 .. 3600;
   --  Longer values may be acceptable

   type MQTT_Handle is limited private;

   subtype MQTT_Data is String;
   subtype MQTT_Indices is Positive; -- index into MQTT_Data

   procedure Connect_Tx (Broker_Host : String;
                         User_Name : String;
                         Password : String;
                         Topic : String;
                         Handle : out MQTT_Handle;
                         QoS : QoSs := 0;
                         Keep_Alive_Time : Keep_Alive_Times := 60);
   --  Connects to broker to permit publication of Topic.
   --  Can only be called once, unless there is a previous call to Disconnect.
   --  Returns Handle which nust be used with Send and Disconnect

   procedure Send (Handle : MQTT_Handle;
                   Message : MQTT_Data);
   --  Sends a String. 

   procedure Connect_Rx (Broker_Host : String;
                         User_Name : String;
                         Password : String;
                         Topic : String;
                         Handle : out MQTT_Handle;
                         QoS : QoSs := 0;
                         Keep_Alive_Time : Keep_Alive_Times := 60);
   --  Connects to broker and subcribes to Topic.
   --  Can only be called once, unless there is a previous call to Disconnect.
   --  Returns Handle which nust be used with Receive and Disconnect

   function Receive (Handle : MQTT_Handle;
                     Timeout : Timeouts := Time_Span_Last) return String;

   --  The function, will return immediately. Either with a message a message
   --  or a zero lemgth string if no message is available.
   --  The Timeout is a stale data timeout. If the last received message
   --  is more than the timeout old, a zero length String is returned.
   --  Unread messages will be disposed of, only the most recent is available.

   procedure Disconnect (Handle : MQTT_Handle);
   --  Disconnects either a publisher or subscriber.

private

   type MQTT_Handle is new Natural;

   --  The following is requited close any open cnnections and clean up
   --  libmosquitto.

   type Controlled_Booleans is new Limited_Controlled with
      record
         State : Boolean := False;
      end record;

   overriding
   procedure Initialize (Controlled_Boolean : in out Controlled_Booleans);

   overriding
   procedure Finalize (Controlled_Boolean : in out Controlled_Booleans);
   
end MQTT_Client;