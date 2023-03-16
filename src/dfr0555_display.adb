-- Package to support DFRobot 0555 (2 x 16) display.
-- Author:    David Haley
-- Created:   04/03/2023
-- Last Edit: 16/03/2023

with Interfaces.C; use Interfaces.C;
with I2C_Interface; use I2C_Interface;

package body DFR0555_Display is

   -- Addresses and values for LED driver NXP PCA9633DP2 IC
   LED_Driver : constant IC_Addresses := 16#60#;
   type LED_Control_Bytes is new unsigned_8;
   subtype LED_Register_Addresses is LED_Control_Bytes range 0 .. 2#00001100#;
   LED_Register_Mode_1 : constant LED_Register_Addresses := 2#00000000#;
   LED_Register_Mode_2 : constant LED_Register_Addresses := 2#00000001#;
   LED_Register_Brightness_0 : constant LED_Register_Addresses := 2#00000010#;
   LED_Register_Brightness_1 : constant LED_Register_Addresses := 2#00000011#;
   LED_Register_Brightness_2 : constant LED_Register_Addresses := 2#00000100#;
   LED_Register_Brightness_3 : constant LED_Register_Addresses := 2#00000101#;
   LED_Register_Group_Duty_Cycle : constant LED_Register_Addresses := 2#00000110#;
   LED_Register_Group_Frequency : constant LED_Register_Addresses := 2#00000111#;
   LED_Register_Output_State : constant LED_Register_Addresses := 2#00001000#;
   -- Registers 00001001 to 00001100 only affect subaddresses and all call address
   -- not relevant to this application
   LED_No_Auto_Increment : constant LED_Control_Bytes := 2#00000000#;
   LED_Auto_Increment_Mode_0 : constant LED_Control_Bytes := 2#10000000#;
   -- Auto-Increment for all registers. D3, D2, D1, D0 roll over to ‘0000’ after
   -- the last register (1100) is accessed.
   LED_Auto_Increment_Mode_1: constant LED_Control_Bytes := 2#10100000#;
   -- Auto-Increment for individual brightness registers only. D3, D2, D1, D0
   -- roll over to ‘0010’ after the last register (0101) is accessed.
   LED_Auto_Increment_Mode_2: constant LED_Control_Bytes := 2#11000000#;
   -- Auto-Increment for global control registers only. D3, D2, D1, D0 roll over
   -- to ‘0110’ after the last register (0111) is accessed.
   LED_Auto_Increment_Mode_3: constant LED_Control_Bytes := 2#11100000#;
   -- Auto-Increment for individual and global control registers only. D3, D2,
   -- D1, D0 roll over to ‘0010’ after the last register (0111) is accessed.

   LED_Value_Mode_1 : constant unsigned_char := 2#00000000#;
   -- Not sleep mode, does not respond to sub addresses or all call
   LED_Value_Mode_2 : constant unsigned_char := 2#00000000#;
   -- Group dimming, non inverted output, output changes on stop, outputs open
   -- drain and not opuput enable pin, outputs high or high Z.
   LED_Group_Brightness : constant unsigned_8 :=2#11111111#;

   -- Addresses and values for LCD driver AiP31068
   LCD_Driver : constant IC_Addresses := 16#3E#;
   type LCD_Control_Bytes is new unsigned_8;
   LCD_Data_Byte : constant LCD_Control_Bytes := 2#01000000#;
   LCD_Instruction_Byte : constant LCD_Control_Bytes := 2#00000000#;
   LCD_Continue : constant LCD_Control_Bytes := 2#10000000#;
   LCD_Data : constant unsigned_char := 2#01000000#;
   LCD_Clear : constant unsigned_char := 2#00000001#;
   LCD_Entry : constant unsigned_char := 2#00000100#;
   LCD_Entry_Right : constant unsigned_char := 2#00000010#;
   LCD_Entry_Shift_Display : constant unsigned_char := 2#00000001#; 
   LCD_ON : constant unsigned_char := 2#00001100#;
   LCD_Cursor_On : constant unsigned_char := 2#00000010#;
   LCD_Cursor_Flash : constant unsigned_char := 2#00000001#;
   LCD_OFF : constant unsigned_char := 2#00001100#;
   LCD_Function : constant unsigned_char := 2#00101000#;
   -- Two line, 5 * 8 characters
   LCD_Set_DDRam : constant unsigned_char := 2#10000000#;

   Display_Ram : constant array (LCD_Lines) of unsigned_char := (0, 16#40#);

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

   Cursor_State : Cursor_States; 

   procedure Send_Command (IC_Address : in IC_Addresses;
                           Command : in out Commands;
                           Caller, Operation : in String) is
                            
      Command_Ptr : access Unsigned_char;
      Return_Value : int;

   begin -- Send_Command
      Return_Value := Set_IC_Address (IC_Address);
      if Return_Value /= 0 then
         if IC_Address = LED_Driver then
            raise LED_Error with Caller & ", " & Operation &
              ", setting IC address";
         elsif IC_Address = LCD_Driver then
            raise LCD_Error with Caller & ", " & Operation &
              ", setting IC address";
         else
            raise Program_Error with Caller & ", " & Operation &
              ", Unknown address";
         end if; -- IC_Address = LED_Driver
      end if; -- Return_Value /= 0
      Command_Ptr := Command (Command_Indices'First)'Access;
      Return_Value := I2C_Write (Command_Ptr, unsigned_short (Command_Length));
      if Return_Value /= Command_Length then
         raise LED_Error with Caller & ", " & Operation;
      end if; -- Return_Value /= Command_Length
   end Send_Command;

   procedure Enable_Display is

      -- Opens I2C device and initialise the display

      subtype Buffer_Indices is Natural range 0 .. 1;

      Command : Commands;
      Return_Value : int;
      Caller : constant String := "Enable_Display";
    
   begin -- Enable_Display
      Return_Value := I2C_Open (I2C_Device);
      if Return_Value /= 0 then
         raise LED_Error with "Opening I2C device" & I2C_Device'img;
      end if; -- Return_Value /= 0
      Command := (unsigned_char (LED_No_Auto_Increment or LED_Register_Mode_1),
                  LED_Value_Mode_1);
      Send_Command (LED_Driver, Command, Caller, "Mode_1 register");
      Command := (unsigned_char (LED_No_Auto_Increment or LED_Register_Mode_2),
                  LED_Value_Mode_2);
      Send_Command (LED_Driver, Command, Caller, "Mode_2 register");
      Command := (unsigned_char (LCD_Instruction_Byte), LCD_Function);
      Send_Command (LCD_Driver, Command, Caller, "LCD_Function");
      Command := (unsigned_char (LCD_Instruction_Byte),
                  LCD_ON or LCD_Cursor_On or LCD_Cursor_Flash);
      Send_Command (LCD_Driver, Command, Caller, "LCD_On");
      Cursor_State.Is_Visible := True;
      Clear;
      Command := (unsigned_char (LCD_Instruction_Byte),
                                 LCD_Entry or LCD_Entry_Right);
      Send_Command (LCD_Driver, Command, Caller, "LCD_Entry");
   end Enable_Display;

   procedure Set_Brightness (Brightness : Unsigned_8;
                             Group_Brightness : Unsigned_8 := Unsigned_8'Last) is
                                
      -- Sets brightness of back light LED, must be called before turning on the
      -- backlight.

      Command : Commands;

   begin -- Set_Brightness
      Command := (unsigned_char (LED_No_Auto_Increment or
                                 LED_Register_Group_Duty_Cycle),
                  unsigned_Char (Group_Brightness));
      Send_Command (LED_Driver, Command, "Set_Brightness",
                    "writing LED_Register_Group_Duty_Cycle");
      Command := (unsigned_char (LED_No_Auto_Increment or
                                   LED_Register_Brightness_0),
                  unsigned_char (Brightness));
      Send_Command (LED_Driver, Command, "Set_Brightness",
                    "writing LED_Register_Brightness_0");
   end Set_Brightness;

   procedure Backlight_On is
   
      -- Turns on the backlight.

      Command : Commands;

   begin -- Backlight_On
      Command := (unsigned_char (LED_No_Auto_Increment or
                  LED_Register_Output_State), 2#00000011#);
      Send_Command (LED_Driver, Command, "Backlight_On",
                    "writing LED_Register_Output_State");
   end Backlight_On;

   procedure Backlight_Off is
   
      -- Turns off the backlight.

      Command : Commands;

   begin -- Backlight_Off
      Command := (unsigned_char (LED_No_Auto_Increment or
                                 LED_Register_Output_State), 2#00000000#);
      Send_Command (LED_Driver, Command, "Backlight_Off",
                    "writing LED_Register_Output_State");
   end Backlight_Off;

   procedure Clear is

      -- Clears all display text

      Command : Commands;
      
   begin -- Clear
      Command := (unsigned_char (LCD_Instruction_Byte), LCD_Clear);
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
      Buffer_Ptr : constant access Unsigned_char :=
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
      Command := (unsigned_char (LCD_Instruction_Byte),
                  LCD_Set_DDRam or
                  (Display_Ram (Line)) + unsigned_char (Column));
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
         Command := (LCD_Data, Unsigned_Char (Character'Pos (Item)));
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
      Buffer_Ptr : constant access Unsigned_char :=
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
