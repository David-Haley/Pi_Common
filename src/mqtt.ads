-- Parent pscjage for MQTT, contains various declarations.

--  Author    : David Haley
--  Created   : 02/03/2026
--  Last_Edit : 04/03/2026

with Interfaces;
with Unchecked_Deallocation;
with Ada.Text_IO;

package MQTT is

   MQTT_Error : exception;

   subtype MQTT_Datum is Interfaces.Unsigned_8;
   subtype MQTT_Indices is Positive;
   type MQTT_Data is array (MQTT_Indices range <>) of MQTT_Datum;

   subtype QoSs is Natural range 0 .. 2; -- As defined by standards

   subtype Keep_Alive_Times is Positive range 1 .. 3600;
   --  Longer values may be acceptable

   package MQTT_Data_IO is new Ada.Text_IO.Modular_IO (MQTT_Datum);

   private

      type Data_Pointers is access all MQTT_Data;

      procedure Free is new Unchecked_Deallocation (MQTT_Data, Data_Pointers);
   
end MQTT;