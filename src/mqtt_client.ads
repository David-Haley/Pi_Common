-- This package allows the creation of multiple publishers and subdcribers.

--  Author    : David Haley
--  Created   : 09/03/2026
--  Last_Edit : 13/03/2026

with Ada.Finalization; use Ada.Finalization;
with Interfaces.C; use Interfaces.C;

package MQTT_Client is

   MQTT_Error : exception;

   subtype QoSs is int range 0 .. 2; -- As defined by standards

   Forever : constant Duration := Duration'Last;

   subtype Timeouts is Duration range 0.0 .. Forever;

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
                     Timeout : Timeouts := Forever) return String;
   --  The function, will return immediately if a message has been received
   --  or after Timout.
   --  Returns a string, a zero length String is returned if the call times
   --  out.

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