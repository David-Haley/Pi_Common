-- This package provides for the interception of the SIGTERM signal and ctrl c
-- to allow an orderly shutdown of a program when these requests are received.
-- Author    : David Haley
-- Created   : 23/05/2022
-- Last Edit : 23/05/2022

package body Linux_Signals is

   Ctrl_C_Received : Boolean := False;
   
   procedure Ctrl_C_Handler is
   
      -- SIGTERM handler allows shutdown from systemd
   
   begin -- Ctrl_C_Handler
      Ctrl_C_Received := True;
   end Ctrl_C_Handler;
   
   function Ctrl_C_Stop return Boolean is (Ctrl_C_Received);
   -- Returns True after Ctrl_C is received.
   
   protected body Handlers is

      procedure Install is
      
         -- Installs both interrupt handelers.
      
      begin -- Install
         Attach_Handler (Handlers.SIGTERM_Handler'Access, SIGTERM);
         Install_Handler (Ctrl_C_Handler'Unrestricted_Access);
      end Install;
   
      procedure SIGTERM_Handler is
      
         -- SIGTERM handler allows shutdown from systemd
      
      begin -- SIGTERM_Handler
         SIGTERM_Received := True;
      end SIGTERM_Handler;
      
      function Signal_Stop return Boolean is (SIGTERM_Received);
      -- Returns True after SIGTERM is received.
   
      procedure Remove is
      
         -- Unnstalls both interrupt handelers
      
      begin -- Remove
         Detach_Handler (SIGTERM); -- remove SIGTERM handler
         Uninstall_Handler; -- remove ctrl c handler
      end Remove;
     
   end Handlers;
   
end Linux_Signals;
   
   
