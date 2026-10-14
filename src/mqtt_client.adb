-- This package allows the creation of multiple publishers and subscribers.

--  Author    : David Haley
--  Created   : 09/03/2026
--  Last_Edit : 14/10/2026

--  20261014 : Reliability changes. Call backs no longer propagate exceptions
--  into libmosquitto. Disconnect call back added so that loss of connection
--  is detected. Connection made asynchronously with automatic reconnection.
--  Disconnect no longer hangs if not connected and always releases resources.
--  Resources released if Connect_Tx or Connect_Rx fail. Client Ids made
--  unique. Handles start from 1. Finalize disconnects any open connections.
--  Send and Receive no longer require connection.
--  20260526 : Initialize code corrected.
--  20260524 : Is_Connected_Tx and Is_Connected_Rx added.
--  20260526 : Removal of various compiler warnings.
--  20260426 : Receive made non blocking, libmosquitto buffers subscribed
--  topics so strictly the buffering in this unit was not required, thus
--  eliminating the need of an additional thread per subscribed topic.
--  20260424 : Follow official Mosquitto example
--  20260423 : Removed potential memory leak in Send, Data_Pointer is now
--  Explicetly feed.
--  20260422 : Changes to remove compiler warnings, largely style.
--  Functional changes for more correct pointer and address conversions.
--  20260317: Redundant with for clause MQTT removed

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Machine_Properties;
with MQTT_Library; use MQTT_Library;

package body MQTT_Client is

   --  Package private data structures

   Default_Port : constant int := 1883;
   Reconnect_Delay : constant unsigned := 1; -- seconds
   Reconnect_Delay_Max : constant unsigned := 30; -- seconds

   type Connection_Types is (Publish, Subscribe);

   protected type Message_Semaphores is
      entry Wait;
      procedure Signal;
      procedure Clear;
   private
      Received : Boolean := False;
   end Message_Semaphores;

   protected body Message_Semaphores is

      entry Wait when Received is

      begin -- Wait
         Received := False;
      end Wait;

      procedure Signal is

      begin -- Signal;
         Received := True;
      end Signal;

      procedure Clear is

      begin -- Clear
         Received := False;
      end Clear;

   end Message_Semaphores;

   type Semaphore_Pointers is access Message_Semaphores;

   procedure Free is new
     Ada.Unchecked_Deallocation (Message_Semaphores, Semaphore_Pointers);

   type Mosq_Pointers is access all mosquitto;

   type Connections is record
      Connection_Type : Connection_Types := Publish;
      Topic : Unbounded_String := Null_Unbounded_String;
      Topic_Pointer : chars_ptr := Null_Ptr;
      QoS : QoSs := 0;
      Mosq : Mosq_Pointers := null;
      Connected : Boolean := False;
      Last_Error : int := MOSQ_ERR_SUCCESS;
      Message : Unbounded_String := Null_Unbounded_String;
      Time_Stamp : Time;
      Semaphore_Pointer : Semaphore_Pointers := null;
   end record; -- Connections

   package Connection_Stores is new
       Ada.Containers.Ordered_Maps (MQTT_Handle, Connections);
   use Connection_Stores;

   --  The Handle itself is passed to libmosquitto as the user data (Obj) for
   --  call backs, thus no memory has to be kept valid for libmosquitto.

   function To_Obj (Handle : MQTT_Handle) return System.Address is
     (To_Address (Integer_Address (Handle)));

   function To_Handle (Obj : System.Address) return MQTT_Handle is
     (MQTT_Handle (To_Integer (Obj)));

   procedure Tx_On_Connect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) with Convention => C;

   procedure Rx_On_Connect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) with Convention => C;

   procedure On_Disconnect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) with Convention => C;

   type Connect_Pointers is access procedure (Mosq : access mosquitto;
                                              Obj : System.Address;
                                              Return_Code : int)
                                              with Convention => C;

   procedure On_Message (Mosq : access mosquitto;
                         Obj : System.Address;
                         Message : access constant mosquitto_message)
                         with Convention => C;

   type Message_Pointers is access procedure (Mosq : access mosquitto;
                                              Obj : System.Address;
                                              Message : access constant
                                                mosquitto_message)
                                              with Convention => C;

   function getpid return int
     with Import => True, Convention => C, External_Name => "getpid";

   --  Package global data

   protected Store is

      procedure New_Handle (Handle : out MQTT_Handle);

      procedure Insert (Handle : MQTT_Handle;
                        Connection_Type : Connection_Types;
                        Topic : String;
                        QoS : QoSs;
                        Mosq : Mosq_Pointers);

      procedure Set_Connected (Handle : MQTT_Handle;
                               Connected : Boolean;
                               Return_Code : int);
      --  Ignored if Handle is not in the store, Return_Code is recorded if
      --  not success.

      procedure Set_Error (Handle : MQTT_Handle; Return_Code : int);

      procedure Set_Message (Handle : MQTT_Handle;
                             Message : access constant mosquitto_message);
      --  Ignored if Handle is not in the store.

      procedure Remove (Handle : MQTT_Handle;
                        Connection : out Connections;
                        Found : out Boolean);
      --  Removes Handle from the store, the caller is responsible for
      --  releasing the resources referenced by Connection.

      function Is_Valid (Handle : MQTT_Handle;
                         Connection_Type : Connection_Types) return Boolean;

      function Is_Connected (Handle : MQTT_Handle;
                             Connection_Type : Connection_Types)
                             return Boolean;

      function First_Handle return MQTT_Handle;
      --  Returns 0 (not a valid handle) if the store is empty.

      function Get_Topic_Pointer (Handle : MQTT_Handle) return chars_ptr;

      function Get_Qos (Handle : MQTT_Handle) return QoSs;

      function Get_Mosq (Handle : MQTT_Handle) return Mosq_Pointers;

      function Get_Last_Error (Handle : MQTT_Handle) return int;

      function Get_Message (Handle : MQTT_Handle;
                            Timeout : Timeouts) return String;

      function Get_Semaphore (Handle : MQTT_Handle) return Semaphore_Pointers;

   private
      Handle_Store : MQTT_Handle := 1;
      Connection_Store : Connection_Stores.Map;
   end Store;

   protected body Store is

      procedure New_Handle (Handle : out MQTT_Handle) is

      begin -- New_Handle
         Handle := Handle_Store;
         Handle_Store := @ + 1;
      end New_Handle;

      procedure Insert (Handle : MQTT_Handle;
                        Connection_Type : Connection_Types;
                        Topic : String;
                        QoS : QoSs;
                        Mosq : Mosq_Pointers) is

         Connection : Connections;

      begin -- Insert
         Connection.Connection_Type := Connection_Type;
         Connection.Topic := To_Unbounded_String (Topic);
         Connection.Topic_Pointer := New_String (Topic);
         Connection.QoS := QoS;
         Connection.Mosq := Mosq;
         Connection.Time_Stamp := Clock;
         Connection.Semaphore_Pointer := new Message_Semaphores;
         Insert (Connection_Store, Handle, Connection);
      end Insert;

      procedure Set_Connected (Handle : MQTT_Handle;
                               Connected : Boolean;
                               Return_Code : int) is

      begin -- Set_Connected
         if Contains (Connection_Store, Handle) then
            Connection_Store (Handle).Connected := Connected;
            if Return_Code /= MOSQ_ERR_SUCCESS then
               Connection_Store (Handle).Last_Error := Return_Code;
            end if; -- Return_Code /= MOSQ_ERR_SUCCESS
         end if; -- Contains (Connection_Store, Handle)
      end Set_Connected;

      procedure Set_Error (Handle : MQTT_Handle; Return_Code : int) is

      begin -- Set_Error
         if Contains (Connection_Store, Handle) then
            Connection_Store (Handle).Last_Error := Return_Code;
         end if; -- Contains (Connection_Store, Handle)
      end Set_Error;

      procedure Set_Message (Handle : MQTT_Handle;
                             Message : access constant mosquitto_message) is

         Data : MQTT_Data (1 .. MQTT_Indices (Message.payloadlen));
         for Data'Address use Message.payload;
         pragma Import (Ada, Data);

      begin -- Set_Message
         --  As well as converting from a string to an Unbounded_String the
         --  string is copied, making a copy is essential because Message is
         --  allocated in libmosquitto and is deallocated on return;
         if Contains (Connection_Store, Handle) then
            Connection_Store (Handle).Message := To_Unbounded_String (Data);
            Connection_Store (Handle).Time_Stamp := Clock;
            Connection_Store (Handle).Semaphore_Pointer.Signal;
         end if; -- Contains (Connection_Store, Handle)
      end Set_Message;

      procedure Remove (Handle : MQTT_Handle;
                        Connection : out Connections;
                        Found : out Boolean) is

      begin -- Remove
         Found := Contains (Connection_Store, Handle);
         if Found then
            Connection := Connection_Store (Handle);
            Delete (Connection_Store, Handle);
         end if; -- Found
      end Remove;

      function Is_Valid (Handle : MQTT_Handle;
                         Connection_Type : Connection_Types) return Boolean is
        (Contains (Connection_Store, Handle) and then
         Connection_Store (Handle).Connection_Type = Connection_Type);

      function Is_Connected (Handle : MQTT_Handle;
                             Connection_Type : Connection_Types)
                             return Boolean is
        (Is_Valid (Handle, Connection_Type) and then
         Connection_Store (Handle).Connected);

      function First_Handle return MQTT_Handle is
        (if Is_Empty (Connection_Store) then 0
         else First_Key (Connection_Store));

      function Get_Topic_Pointer (Handle : MQTT_Handle) return chars_ptr is
        (Connection_Store (Handle).Topic_Pointer);

      function Get_Qos (Handle : MQTT_Handle) return QoSs is
        (Connection_Store (Handle).QoS);

      function Get_Mosq (Handle : MQTT_Handle) return Mosq_Pointers is
        (Connection_Store (Handle).Mosq);

      function Get_Last_Error (Handle : MQTT_Handle) return int is
        (Connection_Store (Handle).Last_Error);

      function Get_Message (Handle : MQTT_Handle;
                            Timeout : Timeouts) return String is

      begin -- Get_Message
         if Connection_Store (Handle).Message = Null_Unbounded_String then
            return "";
         elsif Clock - Connection_Store (Handle).Time_Stamp < Timeout then
            return To_String (Connection_Store (Handle).Message);
         else
            return "";
         end if; -- Connected_Store (Handle).Message = Null_Unbounded_String
      end Get_Message;

      function Get_Semaphore (Handle : MQTT_Handle) return Semaphore_Pointers is
        (Connection_Store (Handle).Semaphore_Pointer);

   end Store;

   procedure Release (Connection : in out Connections) is

      --  Releases the resources referenced by a connection removed from
      --  Store, the libmosquitto thread must already have been stopped or
      --  never started.

   begin -- Release
      mosquitto_destroy (Connection.Mosq);
      Connection.Mosq := null;
      Free (Connection.Topic_Pointer);
      Free (Connection.Semaphore_Pointer);
   end Release;

   function Client_Id (Topic : String;
                       Connection_Type : Connection_Types;
                       Handle : MQTT_Handle) return String is

      --  Client Ids must be unique for a broker, the process Id and Handle
      --  ensure uniqueness for the machine.

      Type_Image : constant array (Connection_Types) of String (1 .. 2) :=
        [Publish => "Tx", Subscribe => "Rx"];

   begin -- Client_Id
      return Machine_Properties.Machine_Name & '_' &
        Translate (Topic, To_Mapping ("/#+ ", "____")) & '_' &
        Type_Image (Connection_Type) & '_' &
        Trim (getpid'Image, Ada.Strings.Both) & '_' &
        Trim (Handle'Image, Ada.Strings.Both);
   end Client_Id;

   procedure Connect (Connection_Type : Connection_Types;
                      Broker_Host : String;
                      User_Name : String;
                      Password : String;
                      Topic : String;
                      Handle : out MQTT_Handle;
                      QoS : QoSs;
                      Keep_Alive_Time : Keep_Alive_Times) is

      --  Common implementation of Connect_Tx and Connect_Rx.

      Connect_Pointers_Table : constant array (Connection_Types) of
        Connect_Pointers :=
          [Publish => Tx_On_Connect'Access,
           Subscribe => Rx_On_Connect'Access];
      Disconnect_Pointer : constant Connect_Pointers := On_Disconnect'Access;
      Message_Pointer : constant Message_Pointers := On_Message'Access;
      Mosq : Mosq_Pointers;
      Return_Code : int;
      Client_Id_Pointer, Broker_Pointer : chars_ptr;
      User_Pointer, Password_Pointer : chars_ptr := Null_Ptr;
      Connection : Connections;
      Found : Boolean;

   begin -- Connect
      Store.New_Handle (Handle);
      Client_Id_Pointer :=
        New_String (Client_Id (Topic, Connection_Type, Handle));
      Mosq := Mosq_Pointers
        (mosquitto_new (Client_Id_Pointer, True, To_Obj (Handle)));
      Free (Client_Id_Pointer);
      if Mosq = null then
         raise MQTT_Error with "failed to create mosquitto";
      end if; -- Mosq = null
      Store.Insert (Handle, Connection_Type, Topic, QoS, Mosq);
      begin -- Store entry exists, release on failure
         if User_Name /= "" then
            User_Pointer := New_String (User_Name);
            if Password /= "" then
               Password_Pointer := New_String (Password);
            end if; -- Password /= ""
         end if; -- User_Name /= ""
         Return_Code :=
           mosquitto_username_pw_set (Mosq, User_Pointer, Password_Pointer);
         Free (User_Pointer);
         Free (Password_Pointer);
         if Return_Code /= MOSQ_ERR_SUCCESS then
            raise MQTT_Error with
              "failed setting User_Name and Password, code:" & Return_Code'Img;
         end if; -- Return_Code /= MOSQ_ERR_SUCCESS
         mosquitto_connect_callback_set
           (Mosq, Connect_Pointers_Table (Connection_Type));
         mosquitto_disconnect_callback_set (Mosq, Disconnect_Pointer);
         if Connection_Type = Subscribe then
            mosquitto_message_callback_set (Mosq, Message_Pointer);
         end if; -- Connection_Type = Subscribe
         Return_Code := mosquitto_reconnect_delay_set
           (Mosq, Reconnect_Delay, Reconnect_Delay_Max, True);
         if Return_Code /= MOSQ_ERR_SUCCESS then
            raise MQTT_Error with
              "failed setting reconnect delay, code:" & Return_Code'Img;
         end if; -- Return_Code /= MOSQ_ERR_SUCCESS
         Broker_Pointer := New_String (Broker_Host);
         Return_Code := mosquitto_connect_async
           (Mosq, Broker_Pointer, Default_Port, Keep_Alive_Time);
         Free (Broker_Pointer);
         --  Failure to reach the broker (MOSQ_ERR_ERRNO) or to resolve its
         --  name (MOSQ_ERR_EAI) is not fatal, the network loop retries.
         if Return_Code not in MOSQ_ERR_SUCCESS | MOSQ_ERR_ERRNO | MOSQ_ERR_EAI
         then
            raise MQTT_Error with "connect failed, code:" & Return_Code'Img;
         elsif Return_Code /= MOSQ_ERR_SUCCESS then
            Store.Set_Error (Handle, Return_Code);
         end if; -- Return_Code not in ...
         Return_Code := mosquitto_loop_start (Mosq);
         if Return_Code /= MOSQ_ERR_SUCCESS then
            raise MQTT_Error with "start loop failed, code:" & Return_Code'Img;
         end if; -- Return_Code /= MOSQ_ERR_SUCCESS
      exception
         when others =>
            Store.Remove (Handle, Connection, Found);
            Release (Connection);
            raise;
      end; -- Store entry exists, release on failure
   end Connect;

   procedure Connect_Tx (Broker_Host : String;
                         User_Name : String;
                         Password : String;
                         Topic : String;
                         Handle : out MQTT_Handle;
                         QoS : QoSs := 0;
                         Keep_Alive_Time : Keep_Alive_Times := 60) is

      --  Connects to broker to permit publication of Topic.
      --  Returns Handle which must be used with Send and Disconnect.

   begin -- Connect_Tx
      Connect (Publish, Broker_Host, User_Name, Password, Topic, Handle, QoS,
               Keep_Alive_Time);
   end Connect_Tx;

   function Is_Connected_Tx (Handle : MQTT_Handle) return Boolean is

      --  Returns true if connected, may be false immediately after Connect_Tx
      --  called, as this is a non blocking call, or if the connection to the
      --  broker has been lost.

      (Store.Is_Connected (Handle, Publish));

   procedure Send (Handle : MQTT_Handle;
                   Message : MQTT_Data) is

      --  Sends a String. Raises MQTT_Error if Handle is not a publisher.
      --  If not connected a QoS 0 message is discarded, QoS 1 or 2 messages
      --  are queued by libmosquitto and sent when the connection is restored.

      Return_Code : int;

   begin -- Send
      if not Store.Is_Valid (Handle, Publish) then
         raise MQTT_Error with "Send invalid Handle";
      end if; -- not Store.Is_Valid (Handle, Publish)
      Return_Code := mosquitto_publish (Store.Get_Mosq (Handle),
                                        null,
                                        Store.Get_Topic_Pointer (Handle),
                                        Message'Length,
                                        Message'Address,
                                        Store.Get_Qos (Handle),
                                        False);
      if Return_Code not in MOSQ_ERR_SUCCESS | MOSQ_ERR_NO_CONN then
         raise MQTT_Error with "Send failed, code:" & Return_Code'Img;
      end if; -- Return_Code not in MOSQ_ERR_SUCCESS | MOSQ_ERR_NO_CONN
   end Send;

   procedure Connect_Rx (Broker_Host : String;
                         User_Name : String;
                         Password : String;
                         Topic : String;
                         Handle : out MQTT_Handle;
                         QoS : QoSs := 0;
                         Keep_Alive_Time : Keep_Alive_Times := 60) is

      --  Connects to broker and subscribes to Topic.
      --  Returns Handle which must be used with Receive and Disconnect.

   begin -- Connect_Rx
      Connect (Subscribe, Broker_Host, User_Name, Password, Topic, Handle,
               QoS, Keep_Alive_Time);
   end Connect_Rx;

   function Is_Connected_Rx (Handle : MQTT_Handle) return Boolean is

      --  Returns true if connected, may be false immediately after Connect_Rx
      --  called, as this is a non blocking call, or if the connection to the
      --  broker has been lost.

      (Store.Is_Connected (Handle, Subscribe));

   function Receive (Handle : MQTT_Handle;
                     Timeout : Timeouts := Time_Span_Last) return String is

      --  The function will return immediately, either with a message or a
      --  zero length string if no message is available.
      --  The Timeout is a stale data timeout. If the last received message
      --  is more than the timeout old, a zero length String is returned.
      --  Unread messages will be disposed of, only the most recent is
      --  available.

   begin -- Receive
      if not Store.Is_Valid (Handle, Subscribe) then
         raise MQTT_Error with "Receive invalid Handle";
      end if; -- not Store.Is_Valid (Handle, Subscribe)
      --  Cleared before reading, a message arriving in between may be
      --  returned again by Receive_Blocking but is never missed.
      Store.Get_Semaphore (Handle).Clear;
      return Store.Get_Message (Handle, Timeout);
   end Receive;

   function Receive_Blocking (Handle : MQTT_Handle;
                              Wait_Time : Time_Span;
                              Stale_Time : Time_Span)
                              return String is

      --  The function will return immediately if a message has been received
      --  and not yet returned, otherwise when either a message is received or
      --  Wait_Time expires. In general a zero length string will be returned
      --  if a Wait_Time timeout occurs. A zero length string will also be
      --  returned if a message has been received before the function is
      --  called and Stale_Time has expired. If Stale_Time is greater than
      --  Wait_Time, the same message may be returned more than once when the
      --  function is called repeatedly.

      Receive_Semaphore : Semaphore_Pointers;

   begin -- Receive_Blocking
      if not Store.Is_Valid (Handle, Subscribe) then
         raise MQTT_Error with "Receive_Blocking invalid Handle";
      end if; -- not Store.Is_Valid (Handle, Subscribe)
      Receive_Semaphore := Store.Get_Semaphore (Handle);
      select
         Receive_Semaphore.Wait;
      or
         delay To_Duration (Wait_Time);
      end select;
      return Store.Get_Message (Handle, Stale_Time);
   end Receive_Blocking;

   function Last_Error (Handle : MQTT_Handle) return Integer is

      --  Diagnostic, returns the most recent non zero code reported by the
      --  broker or libmosquitto for Handle, zero if none.

   begin -- Last_Error
      if not Store.Is_Valid (Handle, Publish) and
        not Store.Is_Valid (Handle, Subscribe)
      then
         raise MQTT_Error with "Last_Error invalid Handle";
      end if; -- not Store.Is_Valid (Handle, Publish) and ...
      return Integer (Store.Get_Last_Error (Handle));
   end Last_Error;

   procedure Disconnect (Handle : MQTT_Handle) is

      --  Disconnects either a publisher or subscriber. Does not raise an
      --  exception if Handle has already been disconnected.

      Connection : Connections;
      Found : Boolean;
      Disconnect_Code, Stop_Code : int;

   begin -- Disconnect
      --  Removing the Store entry first ensures that concurrent calls cannot
      --  release the same connection twice; call backs ignore removed handles.
      Store.Remove (Handle, Connection, Found);
      if Found then
         --  mosquitto_disconnect must always be called, even if not connected,
         --  otherwise mosquitto_loop_stop waits for a thread that continues
         --  attempting to reconnect.
         Disconnect_Code := mosquitto_disconnect (Connection.Mosq);
         Stop_Code := mosquitto_loop_stop (Connection.Mosq, False);
         Release (Connection);
         if Disconnect_Code not in MOSQ_ERR_SUCCESS | MOSQ_ERR_NO_CONN then
            raise MQTT_Error with "Disconnect failed, code:" &
              Disconnect_Code'Img;
         end if; -- Disconnect_Code not in MOSQ_ERR_SUCCESS | MOSQ_ERR_NO_CONN
         if Stop_Code /= MOSQ_ERR_SUCCESS then
            raise MQTT_Error with "Loop stop failed, code:" & Stop_Code'Img;
         end if; -- Stop_Code /= MOSQ_ERR_SUCCESS
      end if; -- Found
   end Disconnect;

   --  Call backs execute in the libmosquitto thread, exceptions must not be
   --  propagated into libmosquitto, errors are recorded in Store instead.

   pragma Warnings (Off, "-gnatwf");

   procedure Tx_On_Connect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) is

      --  Warning that Mosq is not used is suppressed, Mosq is necessary for
      --  conformance with libary call back.

   pragma Warnings (On, "-gnatwf");

   begin -- Tx_On_Connect
      Store.Set_Connected (To_Handle (Obj), Return_Code = MOSQ_ERR_SUCCESS,
                           Return_Code);
   exception
      when others =>
         null;
   end Tx_On_Connect;

   procedure Rx_On_Connect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) is

      Handle : constant MQTT_Handle := To_Handle (Obj);
      Local_Rc : int;

   begin -- Rx_On_Connect
      Store.Set_Connected (Handle, Return_Code = MOSQ_ERR_SUCCESS,
                           Return_Code);
      if Return_Code = MOSQ_ERR_SUCCESS and Store.Is_Valid (Handle, Subscribe)
      then
         --  Clean session, subscription is required for every connection.
         Local_Rc := mosquitto_subscribe (Mosq, null,
                                          Store.Get_Topic_Pointer (Handle),
                                          Store.Get_Qos (Handle));
         if Local_Rc /= MOSQ_ERR_SUCCESS then
            Store.Set_Error (Handle, Local_Rc);
         end if; -- Local_Rc /= MOSQ_ERR_SUCCESS
      end if; -- Return_Code = MOSQ_ERR_SUCCESS and ...
   exception
      when others =>
         null;
   end Rx_On_Connect;

   pragma Warnings (Off, "-gnatwf");

   procedure On_Disconnect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) is

      --  Warning that Mosq is not used is suppressed, Mosq is necessary for
      --  conformance with libary call back.

   pragma Warnings (On, "-gnatwf");

   begin -- On_Disconnect
      --  Return_Code is zero only if Disconnect was called.
      Store.Set_Connected (To_Handle (Obj), False, Return_Code);
   exception
      when others =>
         null;
   end On_Disconnect;

   pragma Warnings (Off, "-gnatwf");

   procedure On_Message (Mosq : access mosquitto;
                         Obj : System.Address;
                         Message : access constant mosquitto_message) is

      --  Warning that Mosq is not used is suppressed, Mosq is necessary for
      --  conformance with libary call back.

   pragma Warnings (On, "-gnatwf");

   begin -- On_Message
      if Message.payloadlen > 0 then
         Store.Set_Message (To_Handle (Obj), Message);
      end if; -- Message.payloadlen > 0
   exception
      when others =>
         null;
   end On_Message;

   procedure Finalize (Controlled_Boolean : in out Controlled_Booleans) is

      --  Ensures that libmosquitto is cleaned up automatically when
      --  MQTT_Client goes out of scope, without an explicit call to this
      --  package. Disconnects any open connections

      Handle : MQTT_Handle;
      Junk : int;

   begin -- Finalize
      -- Allow for finalisation being called more than once
      if Controlled_Boolean.State then
         Controlled_Boolean.State := False;
         loop -- disconnect all
            Handle := Store.First_Handle;
            exit when Handle = 0;
            begin -- disconnect one
               Disconnect (Handle);
            exception
               when others =>
                  null; -- Handle is removed from Store even if exception
            end; -- disconnect one
         end loop; -- disconnect all
         Junk := mosquitto_lib_cleanup;  -- Always returns success
      end if; -- Controlled_Boolean.State
   end Finalize;

   procedure Initialize (Controlled_Boolean : in out Controlled_Booleans) is

      Return_Code, Version : int;
      Major, Minor, Revision : aliased int;

   begin -- Initialize
      --  libmosquitto initialisation
      Return_Code := mosquitto_lib_init;
      if Return_Code /= MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Library initiation failed, code:" &
           Return_Code'Img;
      end if; -- Return_Code /= mosquitto_h.MOSQ_ERR_SUCCESS
      Version := mosquitto_lib_version (Major'Access, Minor'Access,
                                        Revision'Access);
      if Version < LIBMOSQUITTO_VERSION_NUMBER then
         raise MQTT_Error with "Old library, version:" & Major'Img & Minor'Img &
           Revision'Img;
      end if; -- Version < LIBMOSQUITTO_VERSION_NUMBER
      Controlled_Boolean.State := True;
   end Initialize;

   pragma Warnings (Off, "-gnatwu");
   Library_Initialised : Controlled_Booleans;
   --  Required to ensure Initialize and Finalize called
   --  Warning with respect to unreferenced variable suppresed.
   pragma Warnings (On, "-gnatwu");

end MQTT_Client;
