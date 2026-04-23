-- This package allows the creation of multiple publishers and subdcribers.

--  Author    : David Haley
--  Created   : 09/03/2026
--  Last_Edit : 23/08/2026

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

   task type Rx_Mailbox is
      entry Deposit (Item : in Data_Pointers);
      entry Collect (Item : out Data_Pointers);
      entry Stop;
   end Rx_Mailbox;

   type Rx_Pointers is access Rx_Mailbox;
   procedure Free is new Ada.Unchecked_Deallocation (Rx_Mailbox, Rx_Pointers);

   type Connections is record
      Topic : Unbounded_String := Null_Unbounded_String;
      QoS : QoSs := 0;
      Mosq : access mosquitto := null;
      Handle_Pointer : Handle_Pointers := null;
      Rx_Pointer : Rx_Pointers := null;
   end record; -- Connections

   procedure On_Connect (Mosq : access mosquitto;
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

   package Connection_Stores is new
     Ada.Containers.Ordered_Maps (MQTT_Handle, Connections);
   use Connection_Stores;

   --  Package global data

   protected Next_Handle is
      procedure Get (Handle : out MQTT_Handle);
   private
      Handle_Store : MQTT_Handle := MQTT_Handle'First;
   end Next_Handle;

   protected body Next_Handle is

      procedure Get (Handle : out MQTT_Handle) is

      begin -- Get
         Handle := Handle_Store;
         Handle_Store := @ + 1;
      end Get;

   end Next_Handle;

   Connection_Store : Connection_Stores.Map := Connection_Stores.Empty_Map;

   procedure Connect_Tx (Broker_Host : String;
                         User_Name : String;
                         Password : String;
                         Topic : String;
                         Handle : out MQTT_Handle;
                         QoS : QoSs := 0;
                         Keep_Alive_Time : Keep_Alive_Times := 60) is

   --  Connects to broker to permit publication of Topic.
   --  Can only be called once, unless there is a previous call to Disconnect.
   --  Returns Handle which nust be used with Send and Disconnect

      Return_Code : int;
      Connection : Connections;
      Broker_Pointer, User_Pointer, Password_Pointer : chars_ptr;
      Client_Id : constant String := Machine_Properties.Machine_Name & '_' &
         Topic & "_Tx_" & Image (Ada.Task_Identification.Current_Task);
      Client_Id_Pointer : chars_ptr;

   begin -- Connect_Tx
      Connection.Topic := To_Unbounded_String (Topic);
      Connection.QoS := QoS;
      Next_Handle.Get (Handle);
      Connection.Handle_Pointer := new MQTT_Handle'(Handle);
      Client_Id_Pointer := New_String (Client_Id);
      Connection.Mosq :=
        mosquitto_new (Client_Id_Pointer, True,
                       To_Address (Connection.Handle_Pointer));
      Free (Client_Id_Pointer);
      if Connection.Mosq = null then
         raise MQTT_Error with "Tx failed to create mosquitto";
      end if; -- Connection.Mosq = null
      Insert (Connection_Store, Handle, Connection);
      User_Pointer := New_String (User_Name);
      Password_Pointer := New_String (Password);
      Return_Code := mosquitto_username_pw_set (Connection.Mosq, User_Pointer,
                                                Password_Pointer);
      Free (User_Pointer);
      Free (Password_Pointer);
      if Return_Code /= MOSQ_ERR_SUCCESS then
         raise MQTT_Error with
           "Tx failed seting User_Name and Password, code:" & Return_Code'Img;
      end if; -- Return_Code /= MOSQ_ERR_SUCCESS
      Broker_Pointer := New_String (Broker_Host);
      Return_Code := mosquitto_connect (Connection.Mosq, Broker_Pointer, 1883,
        Keep_Alive_Time);
      Free (Broker_Pointer);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Tx connect failed, code:" & Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
   end Connect_Tx;

   procedure Send (Handle : MQTT_Handle;
                   Message : MQTT_Data) is

      --  Sends a String.

      Return_Code : int;
      Data_Pointer : Data_Pointers := new MQTT_Data'(Message);
      Topic_Pointer : chars_ptr;

   begin -- Send
      if not Contains (Connection_Store, Handle) or else
        Connection_Store (Handle).Rx_Pointer /= null
      then
         raise MQTT_Error with "Send invalid Handle";
      end if; -- not Contains (Connection_Store, Handle) or else ...
      Topic_Pointer := New_String (To_String (Connection_Store (Handle).Topic));
      Return_Code := mosquitto_publish (Connection_Store (Handle).Mosq, null,
                                        Topic_Pointer, Message'Length,
                                        To_Address (Data_Pointer),
                                        Connection_Store (Handle).QoS, False);
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
      --  Can only be called once, unless there is a previous call to Disconnect.
      --  Returns Handle which nust be used with Receive and Disconnect

      Return_Code : int;
      Connection : Connections;
      Broker_Pointer, User_Pointer, Password_Pointer : chars_ptr;
      Client_Id : constant String := Machine_Properties.Machine_Name & '_' &
         Topic & "_Rx_" & Image (Ada.Task_Identification.Current_Task);
      Client_Id_Pointer : chars_ptr;
      Connect_Pointer : constant Connect_Pointers := On_Connect'Access;
      Message_Pointer : constant Message_Pointers := On_Message'Access;

   begin -- Connect_Rx
      Connection.Topic := To_Unbounded_String (Topic);
      Connection.QoS := QoS;
      Next_Handle.Get (Handle);
      Connection.Handle_Pointer := new MQTT_Handle'(Handle);
      Connection.Rx_Pointer := new Rx_Mailbox;
      Client_Id_Pointer := New_String (Client_Id);
      Connection.Mosq :=
        mosquitto_new (Client_Id_Pointer, True,
                       To_Address (Connection.Handle_Pointer));
      Free (Client_Id_Pointer);
      if Connection.Mosq = null then
         raise MQTT_Error with "Rx failed to create mosquitto";
      end if; -- Connection.Mosq = null
      Insert (Connection_Store, Handle, Connection);
      User_Pointer := New_String (User_Name);
      Password_Pointer := New_String (Password);
      Return_Code := mosquitto_username_pw_set (Connection.Mosq, User_Pointer,
                                                Password_Pointer);
      Free (User_Pointer);
      Free (Password_Pointer);
      if Return_Code /= MOSQ_ERR_SUCCESS then
         raise MQTT_Error with
           "Rx failed seting User_Name and Password, code:" & Return_Code'Img;
      end if; -- Return_Code /= MOSQ_ERR_SUCCESS
      mosquitto_connect_callback_set (Connection.Mosq, Connect_Pointer);
      mosquitto_message_callback_set (Connection.Mosq, Message_Pointer);
      Broker_Pointer := New_String (Broker_Host);
      Return_Code := mosquitto_connect (Connection.Mosq, Broker_Pointer, 1883,
        Keep_Alive_Time);
      Free (Broker_Pointer);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Rx connect failed, code:" & Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
      Return_Code := mosquitto_loop_start (Connection.Mosq);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Start Rx loop failed, code:" & 
           Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
   end Connect_Rx;

   function Receive (Handle : MQTT_Handle;
                     Timeout : Timeouts := Forever) return String is

      --  The function, will return immediately if a message has been received
      --  or after Timout.
      --  Returns a string, a zero length String is returned if the call times
      --  out.

      Data_Pointer : Data_Pointers := null;

   begin -- Receive
      if not Contains (Connection_Store, Handle) or else
        Connection_Store (Handle).Rx_Pointer = null
      then
         raise MQTT_Error with "Receive invalid Handle";
      end if; -- not Contains (Connection_Store, Handle) or else ...
      select
         Connection_Store (Handle).Rx_Pointer.Collect (Data_Pointer);
      or
         delay Timeout;
      end select;
      if Data_Pointer /= null then
         declare -- Result declaration block
            Result : constant String := Data_Pointer.all;
         begin -- Result declaration block
            Free (Data_Pointer); 
            return Result;
         end; -- Result declaration block
      else
         return "";
      end if; -- Data_Pointer /= null then
   end Receive;

   procedure Disconnect (Handle : MQTT_Handle) is

      --  Disconnects either a publisher or subscriber.

      Return_Code : int;

   begin -- Disconnect
      if Contains (Connection_Store, Handle) then
         --  Does not raise an exception if already dicconnected!
         Return_Code := mosquitto_disconnect (Connection_Store (Handle).Mosq);
         if Return_Code /=  MOSQ_ERR_SUCCESS then
            raise MQTT_Error with "Disconnect failed, code:" &
              Return_Code'Img;
         end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
         if Connection_Store (Handle).Rx_Pointer /= null then
            Return_Code := mosquitto_loop_stop (Connection_Store (Handle).Mosq,
            False);
            if Return_Code /=  MOSQ_ERR_SUCCESS then
               raise MQTT_Error with "Loop stop failed, code:" & Return_Code'Img;
            end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
         end if; -- Connection_Store (Handle).Rx_Pointer /= null
         mosquitto_destroy (Connection_Store (Handle).Mosq);
         if Connection_Store (Handle).Rx_Pointer /= null then
            select
               Connection_Store (Handle).Rx_Pointer.Stop;
            or
               delay 1.0;
               --  Allow 1 s for clean termination, sufficient unless waiting
               --  for Collect.
               Abort_Task (Connection_Store (Handle).Rx_Pointer'Identity);
            end select;
            Free (Connection_Store (Handle).Rx_Pointer);
         end if; -- Connection_Store (Handle).Rx_Pointer /= null
         Free (Connection_Store (Handle).Handle_Pointer);
         Delete (Connection_Store, Handle);
      end if; -- Contains (Connection_Store, Handle)
   end Disconnect;

   procedure On_Connect (Mosq : access mosquitto;
                         Obj : System.Address;
                         Return_Code : int) is

      Topic_Pointer : chars_ptr;
      Local_Rc : int;
      Handle_Pointer : constant Handle_Pointers := To_Pointer (Obj);

   begin -- On_Connect
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Rx connection failed, code:" &
           Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
      Topic_Pointer :=
        New_String (To_String (Connection_Store (Handle_Pointer.all).Topic));
      Local_Rc := mosquitto_subscribe (Mosq, null, Topic_Pointer,
        Connection_Store (Handle_Pointer.all).QoS);
      Free (Topic_Pointer);
      if Local_Rc /= MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Subscription failed, code:" & Local_Rc'Img;
      end if; -- Local_Rc /= MOSQ_ERR_SUCCESS
   end On_Connect;

   pragma Warnings (Off, "-gnatwf");

   procedure On_Message (Mosq : access mosquitto;
                         Obj : System.Address;
                         Message : access constant mosquitto_message) is

      --  Warning that Mosq is not used is suppressed, Mosq is necessary for
      --  conformance with libary call back.

   pragma Warnings (On, "-gnatwf");

      Data : MQTT_Data (1 .. MQTT_Indices (Message.payloadlen));
      for Data'Address use Message.payload;
      Handle_Pointer : constant Handle_Pointers := To_Pointer (Obj);
      Data_Pointer : Data_Pointers := null;

   begin -- On_Message
      --  The data has to be copied because the memory was allocated in
      --  libmosquitto and will be dealocated after the return of On_Message.
      if Message.payloadlen > 0 then
         Data_Pointer := new MQTT_Data'(Data);
      end if; -- Message.payloadlen > 0
      Connection_Store (Handle_Pointer.all).Rx_Pointer.Deposit (Data_Pointer);
   end On_Message;

   task body Rx_Mailbox is

      Connected : Boolean := True;
      Item_Store : Data_Pointers := null;

   begin -- Rx_Mailbox
      while Connected loop
         select -- Deposit / Stop
            accept Deposit (Item : in Data_Pointers) do
               Free (Item_Store);
               --  Delete old message, even if it wasn't delivered.
               Item_Store := Item;
               accept Collect (Item : out Data_Pointers) do
                  if Item_Store /= null then
                     Item := new MQTT_Data'(Item_Store.all);
                     Free (Item_Store);
                  else
                     Item := null;
                  end if; -- Item_Store /= null
               end Collect;
            end Deposit;
         or
            accept Stop do
               Connected := False;
               Free (Item_Store);
            end Stop; -- Stop
         end select; -- Deposit / Stop
      end loop; -- Connected
   end Rx_Mailbox;

   procedure Finalize (Controlled_Boolean : in out Controlled_Booleans) is

      --  Ensures that libmosquitto is cleaned up automatically when
      --  MQTT_Client goes out of scope, without an explicit call to this
      --  package. Disconnects any open connections

      Junk : int;

   begin -- Finalize
      -- Allow for finalisation being called more than once
      if Controlled_Boolean.State then
         Controlled_Boolean.State := False;
         for H in Iterate (Connection_Store) loop
            begin -- Disconnect exception block
               Disconnect (Key (H));
            exception
               when others =>
                  null;
                  --  Finalisation code is not permitted to propagate exceptions.
            end; -- Disconnect exception block
         end loop; -- H in Iterate (Connection_Store)
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