-- Watchdog interface based on hot water controller requirements. made generic
-- to allow other GPIO pins to be used.

-- Author    : David Haley
-- Created   : 18/09/2017
-- Last Edit : 18/09/2026

--  20260618: Compiler warnings removed.

package body RPi_Watchdog is

   procedure Enable_Watchdog is
      -- Sets the watchdog active. Do not call until watchdog has been cycled
      -- multiple times! Current pump requires several cycles in quicj succession

   begin -- Enable_Watchdog
      Write_Pin (Pin_High, Enable_Watchdog_Pin);
   end Enable_Watchdog;

   procedure Disable_Watchdog is
      -- Makes the watchdog inactive, may be required for an orderley shutdown

   begin -- Disable_Watchdog
      Write_Pin (Pin_Low, Enable_Watchdog_Pin);
   end Disable_Watchdog;

   procedure Kick_Watchdog_High is
      -- Must be called regulary and alternately with Kick_Watchdog_Low to prevent
      -- watchdog "biting".

   begin -- Kick_Watchdog_High
      Write_Pin (Pin_High, Kick_Watchdog_Pin);
   end Kick_Watchdog_High;

   procedure Kick_Watchdog_Low is
      -- Must be called regulary and alternately with Kick_Watchdog_Low to prevent
      -- watchdog "biting".

   begin -- Kick_watchdog_Low
      Write_Pin (Pin_Low, Kick_Watchdog_Pin);
   end Kick_Watchdog_Low;

begin -- RPi_Watchdog
      Bind_Pin (Kick_Watchdog_Pin, Out_Pin);
      Write_Pin (Pin_Low, Kick_Watchdog_Pin);
      Bind_Pin (Enable_Watchdog_Pin, Out_Pin);
      Write_Pin (Pin_Low, Enable_Watchdog_Pin);
end RPi_Watchdog;
