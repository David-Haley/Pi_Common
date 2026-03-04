--  This package supports the retreival if some machine properties.
--  Author    : David Haley
--  Created   : 28/02/2026
--  Last Edit : 04/03/2026

pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Ada.Strings.Fixed;
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

      --  Returns the name of the host machine or a null string on failure.

      Name_Length : constant := 256;

      Result_Pointer : chars_ptr;
      Return_Code : int;

   begin -- Machine_Name
      Result_Pointer :=
         New_String (Ada.Strings.Fixed."*" (Name_Length, ' '));
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