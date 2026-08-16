--  Package to support DFRobot 0555 (2 x 16) display (Versions 1.0 and 1.1).
--  Author:    David Haley
--  Created:   04/03/2023
--  Last Edit: 16/08/2026

--  20260816 : Support for DFR0555 version 1.1 added, LED driver changed from
--  PCA9633 to SN3193. Provides automativ Identification of the LED driver.
--  20260615 : Some compiler warnings removed.
--  20251008 : Backlight_Brighness added.

with Interfaces.C; use Interfaces.C;
with I2C_Interface; use I2C_Interface;

package body DFR0555_Display is

   type LED_Drivers is (PCA9633, SN3193);

   --  Addresses and values for LED driver NXP PCA9633DP2 IC (version 1.0)
   type PCA9633_Control_Bytes is new Unsigned_8;
   subtype PCA9633_Register_Addresses is
     PCA9633_Control_Bytes range 0 .. 2#00001100#;
   PCA9633_Driver : constant IC_Addresses := 16#60#;
   PCA9633_Register_Mode_1 : constant PCA9633_Register_Addresses := 2#00000000#;
   PCA9633_Register_Mode_2 : constant PCA9633_Register_Addresses := 2#00000001#;
   PCA9633_Register_Brightness_0 : constant PCA9633_Register_Addresses :=
     2#00000010#;
   PCA9633_Register_Group_Duty_Cycle : constant PCA9633_Register_Addresses :=
     2#00000110#;
   PCA9633_Register_Output_State : constant PCA9633_Register_Addresses :=
     2#00001000#;
   --  Registers 00001001 to 00001100 only affect subaddresses and all call
   --  address not relevant to this application
   PCA9633_No_Auto_Increment : constant PCA9633_Control_Bytes := 2#00000000#;
   PCA9633_Value_Mode_1 : constant unsigned_char := 2#00000000#;
   -- Not sleep mode, does not respond to sub addresses or all call
   PCA9633_Value_Mode_2 : constant unsigned_char := 2#00000000#;
   -- Group dimming, non inverted output, output changes on stop, outputs open
   -- drain and not opuput enable pin, outputs high or high Z.

   --  Addresses and values for LED Driver SN3193I310E IC (version 1.1)
   subtype SN3193_Register_Addresses is unsigned_char range 0 .. 16#2F#;
   subtype SN3193_Control_Bytes is unsigned_char;
   SN3193_Driver : constant IC_Addresses := 16#6B#;
   SN3193_Shutdown_Register : constant SN3193_Register_Addresses := 16#00#;
   SN3193_Shutdown_Value : constant SN3193_Control_Bytes := 2#00100000#;
   --  N.B. The data sheet is at best confusing possibly in error. For the IC to
   --  do snything useful the LSB must be set to 0. Table 3 notes state that
   --  SSD "1 Normal operation"! All channels enabled, normal operation.
   SN3193_Current_Set_Register : constant SN3193_Register_Addresses := 16#03#;
   SN3193_Current_Value : constant SN3193_Control_Bytes := 2#00010000#;
   --  D4 .. D2 1xx (100) 17.5 mA
   SN3193_PWM_Register_1 : constant SN3193_Register_Addresses := 16#04#;
   SN3193_PWM_Transfer_Register : constant SN3193_Register_Addresses := 16#07#;
   --  Writing here causes the transfer of all the PWM data and LED control
   --  register.
   SN3193_Transfer : constant SN3193_Control_Bytes := 16#00#;
   --  Uncertain if the value written should be 0 or is an don't care.
   SN3193_LED_Control_Regieter : constant SN3193_Register_Addresses := 16#1D#;
   SN3193_LED_1_Enable : constant SN3193_Control_Bytes := 2#00000001#;
   SN3193_All_Disable : constant SN3193_Control_Bytes := 2#00000000#;
   --  SN3193_Reset_Register : constant SN3193_Register_Addresses := 16#2F#;

   -- Addresses and values for LCD driver AiP31068
   LCD_Driver : constant IC_Addresses := 16#3E#;
   type LCD_Control_Bytes is new Unsigned_8;
   LCD_Data_Byte : constant LCD_Control_Bytes := 2#01000000#;
   LCD_Instruction_Byte : constant LCD_Control_Bytes := 2#00000000#;
   LCD_Data : constant unsigned_char := 2#01000000#;
   LCD_Clear : constant unsigned_char := 2#00000001#;
   LCD_Entry : constant unsigned_char := 2#00000100#;
   LCD_Entry_Right : constant unsigned_char := 2#00000010#; 
   LCD_ON : constant unsigned_char := 2#00001100#;
   LCD_Cursor_On : constant unsigned_char := 2#00000010#;
   LCD_Cursor_Flash : constant unsigned_char := 2#00000001#;
   LCD_Function : constant unsigned_char := 2#00101000#;
   -- Two line, 5 * 8 characters
   LCD_Set_DDRam : constant unsigned_char := 2#10000000#;

   Display_Ram : constant array (LCD_Lines) of unsigned_char := [0, 16#40#];

   I2C_Device : constant I2C_Devices := 1;

   Command_Length : constant int := 2;
   subtype Command_Indices is Natural range 0 .. Natural (Command_Length) - 1;
   type Commands is array (Command_Indices) of aliased unsigned_char;

   subtype Cursor_Positions is Natural range 0 .. LCD_Columns'Last + 1;
   type Cursor_States is record
      Cursor_Position : Cursor_Positions;
      Line : LCD_Lines;
      Is_Visible : Boolean;
   end record; -- Cursor_States

   LED_Driver : LED_Drivers;
   Cursor_State : Cursor_States; 

   procedure Send_Command (IC_Address : in IC_Addresses;
                           Command : in out Commands;
                           Caller, Operation : in String) is
                            
      Command_Ptr : access unsigned_char;
      Return_Value : int;

   begin -- Send_Command
      Return_Value := Set_IC_Address (IC_Address);
      if Return_Value /= 0 then
         if IC_Address = PCA9633_Driver or  IC_Address = LCD_Driver then
            raise LED_Error with Caller & ", " & Operation &
              ", setting IC address";
         else
            raise Program_Error with Caller & ", " & Operation &
              ", Unknown IC address";
         end if; -- IC_Address = PCA9633_Driver
      end if; -- Return_Value /= 0
      Command_Ptr := Command (Command_Indices'First)'Access;
      Return_Value := I2C_Write (Command_Ptr, unsigned_short (Command_Length));
      if Return_Value /= Command_Length then
         raise LED_Error with Caller & ", " & Operation;
      end if; -- Return_Value /= Command_Length
   end Send_Command;

   function Identify_LED_Driver return LED_Drivers is

      --  Identifies which LED driver is used and hence which version module.

      Caller : constant String := "Identify_LED_Driver";
      Return_Value : int;
      Dummy : aliased unsigned_char;
      Found : Boolean := False;
      Identity : LED_Drivers;

   begin -- Identify_LED_Driver
         Return_Value := Set_IC_Address (PCA9633_Driver);
         if Return_Value /= 0 then
            raise LED_Error with Caller & " Setting PCA9633 address";
         end if; -- Return_Value /= 0
         Return_Value := I2C_Write (Dummy'Access, 0);
         if Return_Value = 0 then
            Identity := PCA9633;
            Found := True;
         end if; -- Return_Value = 0
         Return_Value := Set_IC_Address (SN3193_Driver);
         if Return_Value /= 0 then
            raise LED_Error with Caller & " Setting SN3193 address";
         end if; -- Return_Value /= 0
         Return_Value := I2C_Write (Dummy'Access, 0);
         if Return_Value = 0 then
            Identity := SN3193;
            Found := True;
         end if; -- Return_Value = 0
         if not Found then
            raise LED_Error with Caller & " No LED driver found";
         end if; -- not Found
      return Identity;
   end Identify_LED_Driver;

   procedure Enable_Display is

      -- Opens I2C device and initialise the display

      Command : Commands;
      Return_Value : int;
      Caller : constant String := "Enable_Display";
    
   begin -- Enable_Display
      Return_Value := I2C_Open (I2C_Device);
      if Return_Value /= 0 then
         raise LED_Error with "Opening I2C device" & I2C_Device'Img;
      end if; -- Return_Value /= 0
      LED_Driver := Identify_LED_Driver;
      case LED_Driver is
      when PCA9633 =>
         Command :=
         [unsigned_char (PCA9633_No_Auto_Increment or PCA9633_Register_Mode_1),
                           PCA9633_Value_Mode_1];
         Send_Command (PCA9633_Driver, Command, Caller, "Mode_1 register");
         Command :=
         [unsigned_char (PCA9633_No_Auto_Increment or PCA9633_Register_Mode_2),
                           PCA9633_Value_Mode_2];
         Send_Command (PCA9633_Driver, Command, Caller, "Mode_2 register");
      when SN3193 =>
         Command := [SN3193_Shutdown_Register, SN3193_Shutdown_Value];
         Send_Command (SN3193_Driver, Command, Caller, "Shutdown register");
      end case; -- LED_Driver
      Command := [unsigned_char (LCD_Instruction_Byte), LCD_Function];
      Send_Command (LCD_Driver, Command, Caller, "LCD_Function");
      Command := [unsigned_char (LCD_Instruction_Byte),
                  LCD_ON or LCD_Cursor_On or LCD_Cursor_Flash];
      Send_Command (LCD_Driver, Command, Caller, "LCD_On");
      Cursor_State.Is_Visible := True;
      Clear;
      Command := [unsigned_char (LCD_Instruction_Byte),
                                 LCD_Entry or LCD_Entry_Right];
      Send_Command (LCD_Driver, Command, Caller, "LCD_Entry");
   end Enable_Display;

   procedure Set_Brightness (Brightness : Backlight_Brightness) is
                                
      -- Sets brightness of back light LED, must be called before turning on the
      -- backlight.

      Command : Commands;
      Caller : constant String :="Set_Brightness";

   begin -- Set_Brightness
      case LED_Driver is
      when PCA9633 =>
         Command := [unsigned_char (PCA9633_No_Auto_Increment or
                                    PCA9633_Register_Group_Duty_Cycle),
                     unsigned_char (Backlight_Brightness'Last)];
         Send_Command (PCA9633_Driver, Command, Caller,
                       "Register_Group_Duty_Cycle");
         Command := [unsigned_char (PCA9633_No_Auto_Increment or
                                    PCA9633_Register_Brightness_0),
                     unsigned_char (Brightness)];
         Send_Command (PCA9633_Driver, Command, Caller,
                       "Register_Brightness_0");
      when SN3193 =>
         Command := [SN3193_Current_Set_Register, SN3193_Current_Value];
         Send_Command (SN3193_Driver, Command, Caller, "Set current");
         Command := [SN3193_PWM_Register_1, unsigned_char (Brightness)];
         Send_Command (SN3193_Driver, Command, Caller, "Set PWM");
         Command := [SN3193_PWM_Transfer_Register, SN3193_Transfer];
         Send_Command (SN3193_Driver, Command, Caller, "PWM transfer");
      end case; -- LED_Driver
   end Set_Brightness;

   procedure Backlight_On is
   
      -- Turns on the backlight.

      Command : Commands;
      Caller : constant String := "Backlight_On";

   begin -- Backlight_On
      case LED_Driver is
      when PCA9633 =>
         Command := [unsigned_char (PCA9633_No_Auto_Increment or
                                    PCA9633_Register_Output_State),
                     2#00000011#];
         Send_Command (PCA9633_Driver, Command, Caller,
                       "Register_Output_State");
      when SN3193 =>
         Command := [SN3193_LED_Control_Regieter, SN3193_LED_1_Enable];
         Send_Command (SN3193_Driver, Command, Caller, "Control_Register");
         Command := [SN3193_PWM_Transfer_Register, SN3193_Transfer];
         Send_Command (SN3193_Driver, Command, Caller, "PWM transfer");
      end case; -- LED_Driver
   end Backlight_On;

   procedure Backlight_Off is
   
      -- Turns off the backlight.

      Command : Commands;
      Caller : constant String := "Backlight_Off";

   begin -- Backlight_Off
      case LED_Driver is
      when PCA9633 =>
         Command := [unsigned_char (PCA9633_No_Auto_Increment or
                                    PCA9633_Register_Output_State), 2#00000000#];
         Send_Command (PCA9633_Driver, Command, Caller,
                       "Register_Output_State");
      when SN3193 =>
         Command := [SN3193_LED_Control_Regieter, SN3193_All_Disable];
         Send_Command (SN3193_Driver, Command, Caller, "Control_Register");
         Command := [SN3193_PWM_Transfer_Register, SN3193_Transfer];
         Send_Command (SN3193_Driver, Command, Caller, "PWM transfer");
      end case; -- LED_Driver
   end Backlight_Off;

   procedure Clear is

      -- Clears all display text

      Command : Commands;
      
   begin -- Clear
      Command := [unsigned_char (LCD_Instruction_Byte), LCD_Clear];
      Send_Command
       (LCD_Driver, Command, "Clear", "writing clear instruction");
      Cursor_State.Cursor_Position := LCD_Columns'First;
      Cursor_State.Line := LCD_Lines'First;
   end Clear;

   procedure Put_Line (LCD_Line : in LCD_Lines;
                       Display_String : in Display_Strings) is
                       
      -- Send text to fill one lines of the display.

      subtype Buffer_Indices is Natural range 0 .. Display_Strings'Last;

      To_Write : constant unsigned_short :=
        unsigned_short (Buffer_Indices'Last + 1);
        
      Buffer : array (Buffer_Indices) of aliased unsigned_char;
      Buffer_Ptr : constant access unsigned_char :=
        Buffer (Buffer_Indices'First)'Access;
      Return_Value : int;

   begin -- Put_Line
      Return_Value := Set_IC_Address (LCD_Driver);
      if Return_Value /= 0 then
         raise LED_Error with "Put_Line, setting LCD_Driver address";
      end if; -- Return_Value /= 0
      Buffer (0) := unsigned_char (LCD_Instruction_Byte);
      Buffer (1) := LCD_Set_DDRam or Display_Ram (LCD_Line);
      Return_Value := I2C_Write (Buffer_Ptr, 2);
      if Return_Value /= 2 then
         raise LCD_Error with "Put_Line, setting DDRam address";
      end if; -- Return_Value /= 2
      Buffer (0) := unsigned_char (LCD_Data_Byte);
      for I in Buffer_Indices range 1 .. Buffer_Indices'Last loop
         Buffer (I) := unsigned_char (Character'Pos (Display_String (I)));
      end loop; -- I in Buffer_Indices range 1 .. Buffer_Indices'Last
      Return_Value := I2C_Write (Buffer_Ptr, To_Write);
      if Return_Value /= int (To_Write) then
         raise LCD_Error with "Put_Line, writing Text";
      end if; -- Return_Value /= int (To_Write)
   end Put_Line;

   procedure Position_Cursor (Column : in LCD_Columns;
                              Line : in LCD_Lines;
                              Visible : in Boolean := True) is
                              
      -- Positions cursor to the specified character position and controles
      -- visibility.

      Command : Commands;

   begin -- Position_Cursor
      Command := [unsigned_char (LCD_Instruction_Byte),
                  LCD_Set_DDRam or
                  (Display_Ram (Line)) + unsigned_char (Column)];
      Send_Command (LCD_Driver, Command, "Position_Cursor",
                    "(" & Column'Img & "," & Line'Img & ")");
      if Cursor_State.Is_Visible /= Visible then
         Command (0) := unsigned_char (LCD_Instruction_Byte);
         if Visible then
            Command (1) := LCD_ON or LCD_Cursor_On or LCD_Cursor_Flash;
         else
            Command (1) := LCD_ON;
         end if; -- Visible
         Send_Command (LCD_Driver, Command, "Position_Cursor",
           "setting visiability");
      end if; -- Cursor_State.Is_Visible /= Visible
      Cursor_State := (Column, Line, Visible);
   end Position_Cursor;

   procedure Put (Item : Character) is
   
      -- Puts one characer (Item) at the current cursor position. Raises an
      -- exception if an attempt is made to write beyond LCD_Columns'Last.

      Command : Commands;

   begin -- Put
      if Cursor_State.Cursor_Position <= LCD_Columns'Last then
         Command := [LCD_Data, unsigned_char (Character'Pos (Item))];
         Send_Command (LCD_Driver, Command, "Put", "character'" & Item & "'");
         Cursor_State.Cursor_Position := Cursor_State.Cursor_Position + 1;
      else
         raise LCD_Error with
           "Attempt to write character beyond LCD_Columns'Last";
      end if; -- Cursor_State.Cursor_Position <= LCD_Columns'Last
   end Put;

   procedure Put (Item : String) is
   
      -- Puts string (Item) atarting at the current cursor position. Raises an
      -- exception if an attempt is made to write beyond LCD_Columns'Last.

      subtype Buffer_Indices is Natural range 0 .. Item'Length;

      To_Write : constant unsigned_short :=
        unsigned_short (Buffer_Indices'Last + 1);
        
      Buffer : array (Buffer_Indices) of aliased unsigned_char;
      Buffer_Ptr : constant access unsigned_char :=
        Buffer (Buffer_Indices'First)'Access;
      Return_Value : int;

   begin -- Put
      if Cursor_State.Cursor_Position + Item'Length - 1 <= LCD_Columns'Last then
         Return_Value := Set_IC_Address (LCD_Driver);
         if Return_Value /= 0 then
            raise LED_Error with "Put, setting LCD_Driver address";
         end if; -- Return_Value /= 0
         Buffer (0) := unsigned_char (LCD_Data_Byte);
         for I in Buffer_Indices range 1 .. Buffer_Indices'Last loop
            Buffer (I) := unsigned_char (Character'Pos (Item (I)));
         end loop; -- I in Buffer_Indices range 1 .. Buffer_Indices'Last
         Return_Value := I2C_Write (Buffer_Ptr, To_Write);
         if Return_Value /= int (To_Write) then
            raise LCD_Error with "Put, writing Text";
         end if; -- Return_Value /= int (To_Write)
      else
         raise LCD_Error with
           "Attempt to write string beyond LCD_Columns'Last";
      end if; -- Cursor_State.Cursor_Position + Item'Length - 1 <= ...
   end Put;

   procedure Disable_Display is

      -- Blanks display and closes I2C device

      Return_Value : int;

   begin -- Disable_Display
      Backlight_Off;
      Clear;
      Return_Value := I2C_Close;
      if Return_Value /= 0 then
         raise LED_Error with
           " Disable_Display, call to I2C_Close";
      end if; -- Return_Value /= 0
   end Disable_Display;

end DFR0555_Display;
