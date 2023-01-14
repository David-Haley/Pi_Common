-- Author    : David Haley
-- Created   : 18/09/2017
-- Last Edit : 1/09/2017
-- Watchdog interface based on hot water controller requirements. made generic
-- to allow other GPIO pins to be used.

with RPi_GPIO; use RPi_GPIO;

generic
   Kick_Watchdog_Pin   :  GPIO_Pins := Gen0; -- Cycle to prove operation
   Enable_Watchdog_Pin :  GPIO_Pins := Gen5; -- Enables watchdog when high

package RPi_Watchdog is

   procedure Enable_Watchdog;
   -- Sets the watchdog active. Do not call until watchdog has been cycled
   -- multiple times! Current pump requires several cycles in quicj succession

   procedure Disable_Watchdog;
   -- Makes the watchdog inactive, may be required for an orderley shutdown

   procedure Kick_Watchdog_High;
   -- Must be called regulary and alternately with Kick_Watchdog_Low to prevent
   -- watchdog "biting".

   procedure Kick_Watchdog_Low;
   -- Must be called regulary and alternately with Kick_Watchdog_Low to prevent
   -- watchdog "biting".

end RPi_Watchdog;
