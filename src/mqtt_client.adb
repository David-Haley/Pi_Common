-- This package allows the creation of multiple publishers and subdcribers.

--  Author    : David Haley
--  Created   : 09/03/2026
--  Last_Edit : 24/05/2026

--  20260526 : Removal of various compiler warnings.
--  20260426 : Receive made non blocking, libmosquitto buffers subscribed
--  topics so strictly the buffering in this unit was not required, thus
--  eliminating the need of an additional thread per subscribed topic.
--  20260424 : Follow official Mosquitto example
--  20260423 : Removed potential memory leak in Send, Data_Pointer is now
--  Explicetly feeed.
--  20260422 : Changes to remove compiler warnings, largely style.
--  Functional changes for more correct pointer and address conversions.
--  20260317: Redundant with for clause MQTT removed

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with System.Address_To_Access_Conversions;
with Machine_Properties;
with MQTT_Library; use MQTT_Library;

package body MQTT_Client is

   --  Package private data structures

   Default_Port : constant int := 1883;

   package Data_Conversions is new
     System.Address_To_Access_Conversions (MQTT_Data);
   use Data_Conversions;

   --  The to_Pointer conversion causes a compiler warning becuse MQTT_Data is
   --  an unconstrained array. To_Pointer instantiated here is not used;

   subtype Data_Pointers is Data_Conversions.Object_Pointer;
   procedure Free is new Ada.Unchecked_Deallocation (MQTT_Data, Data_Pointers);

   package Handle_Conversions is new
     System.Address_To_Access_Conversions (MQTT_Handle);
   use Handle_Conversions;

   subtype Handle_Pointers is Handle_Conversions.Object_Pointer;
   procedure Free is new
     Ada.Unchecked_Deallocation (MQTT_Handle, Handle_Pointers);

   type Connection_Types is (Publish, Subscribe);

   protected type Message_Semaphores is
      entry Wait;
      procedure Signal;
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
   end Message_Semaphores;

   type Semaphore_Pointers is access Message_Semaphores;

   type Connections is record
      Connection_Type : Connection_Types := Publish;
      Handle_Pointer : Handle_Pointers := null;
      Topic : Unbounded_String := Null_Unbounded_String;
      QoS : QoSs := 0;
      Mosq : access mosquitto := null;
      Connected : Boolean := False;
      Message : Unbounded_String := Null_Unbounded_String;
      Time_Stamp : Time;
      Semaphore_Pointer : Semaphore_Pointers := null;
   end record; -- Connections

   package Connection_Stores is new
       Ada.Containers.Ordered_Maps (MQTT_Handle, Connections);
   use Connection_Stores;

   procedure Tx_On_Connect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) with Convention => C;

   procedure Rx_On_Connect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) with Convention => C;
   
   type Connect_Pointers is access procedure (Mosq : access mosquitto;
                                              Obj : System.Address;
                                              Return_Code : int) 
                                              with Export => True,
                                              Convention => C;

   procedure On_Message (Mosq : access mosquitto;
                         Obj : System.Address;
                         Message : access constant mosquitto_message)
                         with Convention => C;

   type Message_Pointers is access procedure (Mosq : access mosquitto;
                                              Obj : System.Address;
                                              Message : access constant
                                                mosquitto_message)
                                              with Convention => C;

   --  Package global data

   protected Store is

      procedure Create (Handle : out MQTT_Handle;
                        Connection_Type : Connection_Types;
                        Topic : String;
                        QoS : QoSs;
                        Client_Id_Pointer : chars_ptr);

      procedure Set_Connected (Handle : MQTT_Handle);

      procedure Unset_Connected (Handle : MQTT_Handle);

      procedure Set_Message (Handle : MQTT_Handle;
                             Message : access constant mosquitto_message);

      procedure Delete (Handle : MQTT_Handle);

      function Is_Valid (Handle : MQTT_Handle;
                         Connection_Type : Connection_Types) return Boolean;

      function Contains (Handle : MQTT_Handle) return Boolean;
      
      function Get_Handle_Address (Handle : MQTT_Handle) return System.Address;

      function Get_Topic (Handle : MQTT_Handle) return String;

      function Get_Qos (Handle : MQTT_Handle) return QoSs;

      function Get_Mosq (Handle : MQTT_Handle) return access mosquitto;

      function Is_Connected (Handle : MQTT_Handle) return Boolean;

      function Get_Mesage (Handle : MQTT_Handle;
                           Timeout : Timeouts) return String;

      function Get_Semaphore (Handle : MQTT_Handle) return Semaphore_Pointers;

   private
      Handle_Store : MQTT_Handle := MQTT_Handle'First;
      Connection_Store : Connection_Stores.Map;
   end Store;

   protected body Store is

      procedure Create (Handle : out MQTT_Handle;
                        Connection_Type : Connection_Types;
                        Topic : String;
                        QoS : QoSs;
                        Client_Id_Pointer : chars_ptr) is

         Connection : Connections;

      begin -- Create
         Handle := Handle_Store;
         Connection.Handle_Pointer := new MQTT_Handle'(Handle);
         Connection.Connection_Type := Connection_Type;
         Connection.Topic := To_Unbounded_String (Topic);
         Connection.Mosq :=
           mosquitto_new (Client_Id_Pointer,
                          True,
                          To_Address (Connection.Handle_Pointer));
         if Connection.Mosq = null then
            raise MQTT_Error with "failed to create mosquitto";
         end if; -- Connection.Mosq = null
         Connection.QoS := QoS;
         Connection.Time_Stamp := Clock;
         Connection.Semaphore_Pointer := new Message_Semaphores;
         Insert (Connection_Store, Handle_Store, Connection);
         Handle_Store := @ + 1;
      end Create;

      procedure Set_Connected (Handle : MQTT_Handle) is

      begin -- Set_Connected
         Connection_Store (Handle).Connected := True;
      end Set_Connected;

      procedure Unset_Connected (Handle : MQTT_Handle) is

      begin -- Unset_Connected
         Connection_Store (Handle).Connected := False;
      end Unset_Connected;

      procedure Set_Message (Handle : MQTT_Handle;
                             Message : access constant mosquitto_message) is

         Data : MQTT_Data (1 .. MQTT_Indices (Message.payloadlen));
         for Data'Address use Message.payload;
         pragma Import (Ada, Data);

      begin -- Set_Message
         --  As well as converting from a string to an Unbounded_String the
         --  string is copied, making a copy is essential because Message is
         --  allocated in libmosquitto and is deallocated on return;
         Connection_Store (Handle).Message := To_Unbounded_String (Data);
         Connection_Store (Handle).Time_Stamp := Clock;
         Connection_Store (Handle).Semaphore_Pointer.Signal;
      end Set_Message;

      procedure Delete (Handle : MQTT_Handle) is

      begin -- Delete
         Free (Connection_Store (Handle).Handle_Pointer);
         Delete (Connection_Store, Handle);
      end Delete;

      function Is_Valid (Handle : MQTT_Handle;
                         Connection_Type : Connection_Types) return Boolean is
        (Contains (Connection_Store, Handle) and then
        (Connection_Store (Handle).Connection_Type = Connection_Type and
         Connection_Store (Handle).Connected));

      function Contains (Handle : MQTT_Handle) return Boolean is
        (Contains (Connection_Store, Handle));
      
      function Get_Handle_Address (Handle : MQTT_Handle) return System.Address
        is
        (To_Address (Connection_Store (Handle).Handle_Pointer));


      function Get_Topic (Handle : MQTT_Handle) return String is
        (To_String (Connection_Store (Handle).Topic));

      function Get_Qos (Handle : MQTT_Handle) return QoSs is
        (Connection_Store (Handle).QoS);

      function Get_Mosq (Handle : MQTT_Handle) return access mosquitto is
        (Connection_Store (Handle).Mosq);

      function Is_Connected (Handle : MQTT_Handle) return Boolean is
        (Connection_Store (Handle).Connected);

      function Get_Mesage (Handle : MQTT_Handle;
                           Timeout : Timeouts) return String is

      begin -- Get_Mesage
         if Connection_Store (Handle).Message = Null_Unbounded_String then
            return "";
         elsif Clock - Connection_Store (Handle).Time_Stamp < Timeout then
            return To_String (Connection_Store (Handle).Message);
         else
            return "";
         end if; -- Connected_Store (Handle).Message = Null_Unbounded_String
      end Get_Mesage;

      function Get_Semaphore (Handle : MQTT_Handle) return Semaphore_Pointers is
        (Connection_Store (Handle).Semaphore_Pointer);

   end Store;

   procedure Connect_Tx (Broker_Host : String;
                         User_Name : String;
                         Password : String;
                         Topic : String;
                         Handle : out MQTT_Handle;
                         QoS : QoSs := 0;
                         Keep_Alive_Time : Keep_Alive_Times := 60) is

      --  Connects to broker to permit publication of Topic.
      --  Can only be called once, unless there is a previous call to
      --  Disconnect.
      --  Returns Handle which nust be used with Send and Disconnect

      Return_Code : int;
      Broker_Pointer, User_Pointer, Password_Pointer,
        Client_Id_Pointer : chars_ptr;
      Client_Id : constant String := Machine_Properties.Machine_Name & '_' &
         Topic & "_Tx_" & Image (Ada.Task_Identification.Current_Task);
      Connect_Pointer : constant Connect_Pointers := Tx_On_Connect'Access;

   begin -- Connect_Tx
      Client_Id_Pointer := New_String (Client_Id);
      Store.Create (Handle, Publish, Topic, QoS, Client_Id_Pointer);
      Free (Client_Id_Pointer);
      User_Pointer := New_String (User_Name);
      Password_Pointer := New_String (Password);
      Return_Code := mosquitto_username_pw_set (Store.Get_Mosq (Handle),
                                                User_Pointer, Password_Pointer);
      Free (User_Pointer);
      Free (Password_Pointer);
      if Return_Code /= MOSQ_ERR_SUCCESS then
         raise MQTT_Error with
           "Tx failed seting User_Name and Password, code:" & Return_Code'Img;
      end if; -- Return_Code /= MOSQ_ERR_SUCCESS
      mosquitto_connect_callback_set (Store.Get_Mosq (Handle), Connect_Pointer);
      Broker_Pointer := New_String (Broker_Host);
      Return_Code := mosquitto_connect (Store.Get_Mosq (Handle),
                                        Broker_Pointer,
                                        Default_Port,
                                        Keep_Alive_Time);
      Free (Broker_Pointer);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Tx connect failed, code:" & Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
      Return_Code := mosquitto_loop_start (Store.Get_Mosq (Handle));
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Start Tx loop failed, code:" & 
           Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
   end Connect_Tx;

   procedure Send (Handle : MQTT_Handle;
                   Message : MQTT_Data) is

      --  Sends a String.

      Return_Code : int;
      Data_Pointer : Data_Pointers := new MQTT_Data'(Message);
      Topic_Pointer : chars_ptr;

   begin -- Send
      if not Store.Is_Valid (Handle, Publish) then
         raise MQTT_Error with "Send invalid Handle";
      end if; -- not Store.Is_Valid (Handle, Publish)
      Topic_Pointer := New_String (Store.Get_Topic (Handle));
      Return_Code := mosquitto_publish (Store.Get_Mosq (Handle),
                                        null,
                                        Topic_Pointer,
                                        Message'Length,
                                        To_Address (Data_Pointer),
                                        Store.Get_Qos (Handle),
                                        False);
      Free (Topic_Pointer);
      Free (Data_Pointer);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Send failed, code:" & Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
   end Send;

   procedure Connect_Rx (Broker_Host : String;
                         User_Name : String;
                         Password : String;
                         Topic : String;
                         Handle : out MQTT_Handle;
                         QoS : QoSs := 0;
                         Keep_Alive_Time : Keep_Alive_Times := 60) is

      --  Connects to broker and subcribes to Topic.
      --  Can only be called once, unless there is a previous call to
      --  Disconnect. Returns Handle which nust be used with Receive and
      --  Disconnect

      Return_Code : int;
      Broker_Pointer, User_Pointer, Password_Pointer : chars_ptr;
      Client_Id : constant String := Machine_Properties.Machine_Name & '_' &
         Topic & "_Rx_" & Image (Ada.Task_Identification.Current_Task);
      Client_Id_Pointer : chars_ptr;
      Connect_Pointer : constant Connect_Pointers := Rx_On_Connect'Access;
      Message_Pointer : constant Message_Pointers := On_Message'Access;

   begin -- Connect_Rx
      Client_Id_Pointer := New_String (Client_Id);
      Store.Create (Handle, Subscribe, Topic, QoS, Client_Id_Pointer);
      Free (Client_Id_Pointer);
      User_Pointer := New_String (User_Name);
      Password_Pointer := New_String (Password);
      Return_Code := mosquitto_username_pw_set (Store.Get_Mosq (Handle),
                                                User_Pointer,
                                                Password_Pointer);
      Free (User_Pointer);
      Free (Password_Pointer);
      if Return_Code /= MOSQ_ERR_SUCCESS then
         raise MQTT_Error with
           "Rx failed seting User_Name and Password, code:" & Return_Code'Img;
      end if; -- Return_Code /= MOSQ_ERR_SUCCESS
      mosquitto_connect_callback_set (Store.Get_Mosq (Handle), Connect_Pointer);
      mosquitto_message_callback_set (Store.Get_Mosq (Handle), Message_Pointer);
      Broker_Pointer := New_String (Broker_Host);
      Return_Code := mosquitto_connect (Store.Get_Mosq (Handle),
                                        Broker_Pointer,
                                        Default_Port,
                                        Keep_Alive_Time);
      Free (Broker_Pointer);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Rx connect failed, code:" & Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
      Return_Code := mosquitto_loop_start (Store.Get_Mosq (Handle));
      if Return_Code /= MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Start Rx loop failed, code:" & 
           Return_Code'Img;
      end if; -- Return_Code /= MOSQ_ERR_SUCCESS
   end Connect_Rx;

   function Receive (Handle : MQTT_Handle;
                     Timeout : Timeouts := Time_Span_Last) return String is

      --  The function, will return immediately. Either with a message a message
      --  or a zero lemgth string if no message is available.
      --  The Timeout is a stale data timeout. If the last received message
      --  is more than the timeout old, a zero length String is returned.
      --  Unread messages will be disposed of, only the most recent is
      --  available.

   begin -- Receive
      if not Store.Is_Valid (Handle, Subscribe) then
         raise MQTT_Error with "Receive invalid Handle";
      end if; -- not Store.Is_Valid (Handle, Subscribe)
      return Store.Get_Mesage (Handle, Timeout);
   end Receive;

   function Receive_Blocking (Handle : MQTT_Handle;
                              Wait_Time : Time_Span;
                              Stale_Time : Time_Span)
                              return String is

      --  The function, will return immediately if a message is available or
      --  later when either a message is received or Wait_Time expires. In
      --  general a zero length string will be returned if a Wait_Time timeout
      --  occurs. A zero length string will also be returned if a message has
      --  been received before the function is called and Stale_Time has
      --  expired. If Stale_Time is greater than Wait_Time, the same message
      --  may be returned more than once when the function is called repeatedly.

      Receive_Semaphore : constant Semaphore_Pointers :=
        Store.Get_Semaphore (Handle);

   begin -- Receive_Blocking
      if not Store.Is_Valid (Handle, Subscribe) then
         raise MQTT_Error with "Receive_Blocking invalid Handle";
      end if; -- not Store.Is_Valid (Handle, Subscribe)
      select
         Receive_Semaphore.Wait;
      or
         delay To_Duration (Wait_Time);
      end select;
      return Store.Get_Mesage (Handle, Stale_Time);
   end Receive_Blocking;

   procedure Disconnect (Handle : MQTT_Handle) is

      --  Disconnects either a publisher or subscriber.

      Return_Code : int;

   begin -- Disconnect
      if Store.Contains (Handle) then
         --  Does not raise an exception if already dicconnected!
         if Store.Is_Connected (Handle) then
            Store.Unset_Connected (Handle);
            Return_Code := mosquitto_disconnect (Store.Get_Mosq (Handle));
            if Return_Code /=  MOSQ_ERR_SUCCESS then
               raise MQTT_Error with "Disconnect failed, code:" &
                 Return_Code'Img;
            end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
         end if; -- Store.Is_Connected (Handle)
         Return_Code := mosquitto_loop_stop (Store.Get_Mosq (Handle), False);
         if Return_Code /=  MOSQ_ERR_SUCCESS then
            raise MQTT_Error with "Loop stop failed, code:" &
              Return_Code'Img;
         end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
         mosquitto_destroy (Store.Get_Mosq (Handle));
         Store.Delete (Handle);
      end if; -- Store.Contains (Handle)
   end Disconnect;

   pragma Warnings (Off, "-gnatwf");

   procedure Tx_On_Connect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) is

      --  Warning that Mosq is not used is suppressed, Mosq is necessary for
      --  conformance with libary call back.

   pragma Warnings (On, "-gnatwf");

      Handle_Pointer : constant Handle_Pointers := To_Pointer (Obj);

   begin -- Tx_On_Connec
      if Return_Code = MOSQ_ERR_SUCCESS then
         Store.Set_Connected (Handle_Pointer.all);
      else
         raise MQTT_Error with "Tx call back, connection failed, code:" &
           Return_Code'Img;
      end if; -- Return_Code = MOSQ_ERR_SUCCESS
   end Tx_On_Connect;

   procedure Rx_On_Connect (Mosq : access mosquitto;
                            Obj : System.Address;
                            Return_Code : int) is

      Topic_Pointer : chars_ptr;
      Local_Rc : int;
      Handle_Pointer : constant Handle_Pointers := To_Pointer (Obj);

   begin -- Rx_On_Connect
      if Return_Code = MOSQ_ERR_SUCCESS then
         Store.Set_Connected (Handle_Pointer.all);
      else
         raise MQTT_Error with "Rx call back, connection failed, code:" &
           Return_Code'Img;
      end if; -- Return_Code = MOSQ_ERR_SUCCESS
      Topic_Pointer :=
        New_String (Store.Get_Topic (Handle_Pointer.all));
      Local_Rc := mosquitto_subscribe (Mosq, null, Topic_Pointer,
        Store.Get_QoS (Handle_Pointer.all));
      Free (Topic_Pointer);
      if Local_Rc /= MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Subscription failed, code:" & Local_Rc'Img;
      end if; -- Local_Rc /= MOSQ_ERR_SUCCESS
   end Rx_On_Connect;

   pragma Warnings (Off, "-gnatwf");

   procedure On_Message (Mosq : access mosquitto;
                         Obj : System.Address;
                         Message : access constant mosquitto_message) is

      --  Warning that Mosq is not used is suppressed, Mosq is necessary for
      --  conformance with libary call back.

   pragma Warnings (On, "-gnatwf");

      Handle_Pointer : constant Handle_Pointers := To_Pointer (Obj);

   begin -- On_Message
      if Message.payloadlen > 0 then
         Store.Set_Message (Handle_Pointer.all, Message);
      end if; -- Message.payloadlen > 0
   end On_Message;

   procedure Finalize (Controlled_Boolean : in out Controlled_Booleans) is

      --  Ensures that libmosquitto is cleaned up automatically when
      --  MQTT_Client goes out of scope, without an explicit call to this
      --  package. Disconnects any open connections

      Junk : int;

   begin -- Finalize
      -- Allow for finalisation being called more than once
      if Controlled_Boolean.State then
         Controlled_Boolean.State := False;
         Junk := mosquitto_lib_cleanup;  -- Always returns success
      end if; -- Controlled_Boolean.State
   end Finalize;

   procedure Initialize (Controlled_Boolean : in out Controlled_Booleans) is

   begin -- Initialize
      Controlled_Boolean.State := True;
   end Initialize;

   --  Data structures associated with library initialisation only.

   Return_Code, Version : int;
   Major, Minor, Revision : aliased int;
   Library_Initialised : Controlled_Booleans;

begin -- MQTT_Client
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

   pragma Warnings (Off, "-gnatwm" );
      Initialize (Library_Initialised);
   --  Variable used to ensure single shot finalisation, it is set here and
   --  unset by Finalize.
   pragma Warnings (On, "-gnatwm" );  
end MQTT_Client;