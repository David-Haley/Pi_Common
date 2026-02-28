--  This package supports the retreival if some machine properties.
--  Author    : David Haley
--  Created   : 28/02/2026
--  Last Edit : 01/03/2026

pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings; 
with System; use System;

package body Machine_Properties is

   function get_host_name
      (Name : chars_ptr;
       Length : size_t) return int  -- /usr/include/mosquitto.h:3270
      with Import => True, 
         Convention => C, 
         External_Name => "gethostname";

   function Machine_Name return String is
      --  Returns the name of the host machine.

      Name_length : constant := 256;

      Buffer : char_array (0 .. Name_length - 1) := (others => ' ');
      --  Not Actually used, just sets the size of returned result
      Result_Pointer : chars_ptr;
      Return_Code : int;

   begin -- Machine_Name
      Result_Pointer := New_Char_array (Buffer);
      Return_Code := get_host_name (Result_Pointer, Name_Length);
      if Return_Code = 0 then
         declare
            Result : String := Value (Result_Pointer);
         begin
            Free (Result_Pointer);
            return Result;
         end;
      else
         Free (Result_Pointer);
         return "";
      end if; -- Return_Code = 0
   end Machine_Name;

end Machine_Properties;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");