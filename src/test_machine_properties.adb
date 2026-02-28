--  Tests Machine_Properties package.
--  Author    : David Haley
--  Created   : 01/03/2026
--  Last Edit : 01/03/2026

with Ada.Text_IO; use Ada.Text_IO;
with Machine_Properties; use Machine_Properties;

procedure Test_Machine_Properties is
   
begin -- Test_Machine_Properties
   Put_Line ("Machine_Name: """ & Machine_Name & '"');
end Test_Machine_Properties;