-- This package provides for the interception of the SIGTERM signal and ctrl c
-- to allow an orderly shutdown of a program when these requests are received.
-- Author    : David Haley
-- Created   : 23/05/2022
-- Last Edit : 21/04/2026

--  20260421 : Updated to remove compiler style warnings.

package Linux_Signals is
   
   protected Handlers is

      procedure Install;
      -- Installs both interrupt handelers
      
      function Signal_Stop return Boolean;
      -- Returns True after SIGTERM is received.
      
      procedure Remove;
      -- Unnstalls both interrupt handelers
      
   private
   
      procedure SIGTERM_Handler;
      
      pragma Interrupt_Handler (SIGTERM_Handler);
   
      SIGTERM_Received : Boolean := False;
      
   end Handlers;

   function Ctrl_C_Stop return Boolean;
   -- Returns True after Ctrl_C is received.
   
end Linux_Signals;
