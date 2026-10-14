-- This package allows the creation of multiple publishers and subscribers.

--  Author    : David Haley
--  Created   : 09/03/2026
--  Last_Edit : 14/10/2026

--  20261014 : Reliability changes. Connect_Tx and Connect_Rx no longer require
--  the broker to be reachable, libmosquitto reconnects automatically.
--  Is_Connected_Tx and Is_Connected_Rx track connection loss. Send and
--  Receive no longer raise MQTT_Error when the connection is down. Last_Error
--  added.
--  20260524 : Is_Connected_Tx and Is_Connected_Rx added.
--  20260427 : Receive_Blocking added;
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
   --  Returns Handle which must be used with Send and Disconnect.
   --  The connection is made in the background, an unreachable broker does
   --  not cause MQTT_Error to be raised; connection attempts are repeated
   --  until successful and after any subsequent loss of connection.

   function Is_Connected_Tx (Handle : MQTT_Handle)
                             return Boolean with Volatile_Function;
   --  Returns true if connected, may be false immediately after Connect_Tx
   --  called, as this is a non blocking call, or if the connection to the
   --  broker has been lost.

   procedure Send (Handle : MQTT_Handle;
                   Message : MQTT_Data);
   --  Sends a String. Raises MQTT_Error if Handle is not a publisher.
   --  If not connected a QoS 0 message is discarded, QoS 1 or 2 messages are
   --  queued by libmosquitto and sent when the connection is restored.

   procedure Connect_Rx (Broker_Host : String;
                         User_Name : String;
                         Password : String;
                         Topic : String;
                         Handle : out MQTT_Handle;
                         QoS : QoSs := 0;
                         Keep_Alive_Time : Keep_Alive_Times := 60);
   --  Connects to broker and subscribes to Topic.
   --  Returns Handle which must be used with Receive and Disconnect.
   --  The connection is made in the background, an unreachable broker does
   --  not cause MQTT_Error to be raised; connection attempts are repeated
   --  until successful and after any subsequent loss of connection, Topic is
   --  resubscribed on each connection.

   function Is_Connected_Rx (Handle : MQTT_Handle)
                             return Boolean with Volatile_Function;
   --  Returns true if connected, may be false immediately after Connect_Rx
   --  called, as this is a non blocking call, or if the connection to the
   --  broker has been lost.

   function Receive (Handle : MQTT_Handle;
                     Timeout : Timeouts := Time_Span_Last) return String;

   --  The function will return immediately, either with a message or a zero
   --  length string if no message is available.
   --  The Timeout is a stale data timeout. If the last received message
   --  is more than the timeout old, a zero length String is returned.
   --  Unread messages will be disposed of, only the most recent is available.
   --  Raises MQTT_Error if Handle is not a subscriber. Zero length messages
   --  are ignored.

   function Receive_Blocking (Handle : MQTT_Handle;
                              Wait_Time : Time_Span;
                              Stale_Time : Time_Span)
                              return String;

   --  The function will return immediately if a message has been received
   --  and not yet returned, otherwise when either a message is received or
   --  Wait_Time expires. In general a zero length string will be returned if
   --  a Wait_Time timeout occurs. A zero length string will also be returned
   --  if a message has been received before the function is called and
   --  Stale_Time has expired. If Stale_Time is greater than Wait_Time, the
   --  same message may be returned more than once when the function is
   --  called repeatedly. Raises MQTT_Error if Handle is not a subscriber.
   --  Disconnect must not be called for Handle while another task is blocked
   --  in Receive_Blocking for the same Handle.

   function Last_Error (Handle : MQTT_Handle) return Integer;
   --  Diagnostic, returns the most recent non zero code reported by the
   --  broker or libmosquitto for Handle, zero if none. For example 5 if the
   --  broker refused the connection as not authorised.

   procedure Disconnect (Handle : MQTT_Handle);
   --  Disconnects either a publisher or subscriber. Does not raise an
   --  exception if Handle has already been disconnected.

private

   type MQTT_Handle is new Natural with Default_Value => 0;
   --  Valid handles start from 1, thus a default initialised handle is never
   --  valid.

   --  The following is required to close any open connections and clean up
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