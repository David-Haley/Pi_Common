--  Simple Subscriber that provides a blocking functions that returns a array
--  of Unsigned_8 (bytes) or a String;
--  Is dependent on libmosquitto, ported from C example
--  https://github.com/Johannes4Linux/libmosquitto_examples

--  Author    : David Haley
--  Created   : 15/02/2026
--  Last_Edit : 04/03/2026

with Ada.Task_Identification;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with MQTT_Library; use MQTT_Library;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C.Extensions;
with System;
with Machine_Properties;

package body MQTT.Subscriber is

   package Q_Int is new
     Ada.Containers.Synchronized_Queue_Interfaces (Data_Pointers);

   package Message_Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Q_Int);
   use Message_Queues;

   --  Call back procedures, called from libmosquitto

   Saved_QoS : int; -- Copy of QoS from Connect

   procedure On_Connect (Mosq : access mosquitto;
                         Obj : System.Address;
                         Return_Code : int) with Convention => C;
   
   type Connect_Pointers is access procedure (Mosq : access mosquitto;
                                              Obj : System.Address;
                                              Return_Code : int) 
                                              with Export => True,
                                              Convention => C;

   procedure On_Connect (Mosq : access mosquitto;
                         Obj : System.Address;
                         Return_Code : int) is

      Topic_Pointer : chars_ptr;
      Local_Rc : int;

   begin -- On_Connect
      If Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Connection failed, code:" & Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
      Topic_Pointer := New_String (Topic);
      Local_Rc := mosquitto_subscribe (Mosq, Null, Topic_Pointer, Saved_QoS);
      Free (Topic_Pointer);
      If Local_Rc /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Subscription failed, code:" & Local_Rc'Img;
      end if; -- Local_Rc /=  MOSQ_ERR_SUCCESS
   end On_Connect;

   procedure On_Message (Mosq : access mosquitto;
                         Obj : System.Address;
                         Message : access constant mosquitto_message)
                         with Convention => C;

   type Message_Pointers is access procedure (Mosq : access mosquitto;
                                              Obj : System.Address;
                                              Message : access constant
                                                mosquitto_message)
                                              with Convention => C;

   Message_Queue : Message_Queues.Queue;

   procedure On_Message (Mosq : access mosquitto;
                         Obj : System.Address;
                         Message : access constant mosquitto_message) is

      Data : MQTT_Data (1 .. MQTT_Indices (Message.payloadlen));
      for Data'Address use Message.payload;

      Data_Pointer : Data_Pointers :=
        new MQTT_Data (1 .. MQTT_Indices (Message.payloadlen));

   begin -- On_Message
      --  The data has to be copied because the memory was allocated in
      --  libmosquitto and will be dealocated after the return of On_Message.
      for I in MQTT_Indices range 1 ..Positive (Message.payloadlen) loop
         Data_Pointer (I) := Data (I);
      end loop; -- I in MQTT_Indices range 1 ..Positive (Message.payloadlen)
      Message_Queue.Enqueue (Data_Pointer);
   end On_Message;
   
   Mosq : access mosquitto;
   Obj : System.Address;

   procedure Connect (Broker_Host : String;
                      User_Name : String;
                      Password : String;
                      QoS : QoSs := 0;
                      Keep_Alive_Time : Keep_Alive_Times := 60) is
   
      --  connects to broker and subcribes to Topic.
      --  Can only be called once, unless there is a previous call to Disconnect.

      Major, Minor, Revision : aliased int;
      Return_Code, Version : int;
      Broker_Pointer, User_Pointer, Password_Pointer : chars_ptr;
      Client_Id : constant String := Machine_Properties.Machine_Name & '_' &
         Topic &
         Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task);
      Client_Id_Pointer : chars_ptr;
      Connect_Pointer : Connect_Pointers := On_Connect'Access;
      Message_Pointer : Message_Pointers := On_Message'Access;

   begin -- Connect
      Saved_QoS := int (QoS); -- make QoS available to 
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
      Client_Id_Pointer := New_String (Client_Id);
      Mosq := mosquitto_new (Client_Id_Pointer, True, Obj);
      Free (Client_Id_Pointer);
      if Mosq = Null then
         raise MQTT_Error with "Failed to create mosquitto";
      end if; -- Mosq = Null
      User_Pointer := New_String (User_Name);
      Password_Pointer := New_String (Password);
      Return_Code := mosquitto_username_pw_set (Mosq, User_Pointer,
                                                Password_Pointer);
      Free (User_Pointer);
      Free (Password_Pointer);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Failed seting User_Name and Password, code:" &
           Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
      mosquitto_connect_callback_set (Mosq, Connect_Pointer);
      mosquitto_message_callback_set (Mosq, Message_Pointer);
      Broker_Pointer := New_String (Broker_Host);
      Return_Code := mosquitto_connect (Mosq, Broker_Pointer, 1883, 10);
      Free (Broker_Pointer);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Connect failed, code:" & Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
      Return_Code := mosquitto_loop_start (Mosq);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Start loop failed, code:" & Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
   end Connect;

   function Receive return MQTT_Data is

      --  Blocking function, will not return unless a message is received.
      --  Returns an array of binary data.

      Data_Pointer : Data_Pointers := Null;

   begin -- Receive
      Message_Queue.Dequeue (Data_Pointer);
      declare -- Result declaration block
         Result : MQTT_Data := Data_Pointer.all;
      begin -- Result declaration block
         Free (Data_Pointer);
         return Result;
      end; -- Result declaration block
   end Receive;

   function Receive return String is
   
      --  Blocking function, will not return unless a message is received.
      --  Returns a string.

      Data_Pointer : Data_Pointers := Null;

   begin -- Receive
      Message_Queue.Dequeue (Data_Pointer);
      declare -- Result declaration block
         Result : String  (Data_Pointer'Range);
      begin -- Result declaration block
         for I in Data_Pointer'Range loop
            Result (I) := Character'Val (Data_Pointer (I));
         end loop; -- I in Data_Pointer'Range
         Free (Data_Pointer); 
         return Result;
      end; -- Result declaration block
   end Receive;

   procedure Disconnect is

      --  Disconnects and cleans up.
      Return_Code : int;

   begin -- Disconnect
      Return_Code := mosquitto_disconnect (Mosq);
      if Return_Code /=  MOSQ_ERR_SUCCESS then
         raise MQTT_Error with "Disconnect failed, code:" & Return_Code'Img;
      end if; -- Return_Code /=  MOSQ_ERR_SUCCESS
      mosquitto_destroy (Mosq);
      Return_Code := mosquitto_lib_cleanup;
   end Disconnect;
   
end MQTT.Subscriber;