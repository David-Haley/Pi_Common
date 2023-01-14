-- This is an package provides a user space driver for the Texas Instruments
-- TLC5940 16 channel LED driver with dot correction and grey scale PWM control.
-- This is a generic package that can be dimensioned to support a specified
-- number of cascaded ICs to support N * 16 LEDs. The range allows the package
-- to be instanciated with an enumerated type giving a symbolic name to each
-- chip. Tne number of devices which may be cascaded is ultimately limited by
-- practical hardware limits for example fanout on clock signals. The data for
-- each chip is transferred as a seperate SPI operation with the result that no
-- more than 24 bytes are transferred in a single operation. Hence there is no
-- software imposed limit on the number of devices that may be cascaded. The
-- default correction value equates to full brightness amd the default
-- Greyscale is no output. The package depends on the SPI_Interface and rpi-gpio
-- packages. Assumes that hardware is active low to blank and active low to
-- transfer data. Unless otherwise desirable explicit blanking is not required
-- for a gerysacle update the hardware ensures that all updates occur during
-- blanking. No support for dot correction EEPROM, DCPRG assumed high

-- Author    : David Haley
-- Created   : 08/08/2017
-- Last Edit : 09/06/2022

-- 20220609 : Converted to use SPI_Interface directly called C.
-- 20220127 : Calculation of optocoupler time constant resulted in the one bit
-- settling time being changed to 1.424 ms.
-- 20220118 : Implementation of protection on display state, potentially makes
-- this package thread safe.
-- 20221017 : comment corrected
-- 20190720 : Setting_Time changedd to 875ms.
-- 20190716 : Default comparitor pin changed to Gen4 to match Clock Hardware.
-- No Decimal implemented for Set_Digit
-- 20190326 : Ambient light functionality added.
-- 20190324 : Order in which cascaded chip data sent corrected (reversed).
-- Repeated Grefscale SPI transfer added after correction setting see data
-- sheet.
-- 20190323 : Note with respect to concurrance added and other corrections to
-- package desctiption.
-- 20190323 : Generic seven segment support added must be at least 1?
-- 20190320 : Set_Correction corrected and Get_Correction implemented, delay
-- inserted in set correction.
-- 20190319 : Set_Greyscale corrected and Get_Grescale implemented
-- 20190116 : Greyscale clock, Blanking and Xlate managed by hardware to
-- ensure consistent brightness
-- 20181001 : Changed to Ada.Real_Time to get more accurate delay.
-- 20170909 : Task to manage blanking and transfer of LED data

with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with SPI_Interface; use SPI_Interface;
with RPi_GPIO; use RPi_GPIO;

package  body TLC5940 is

   subtype GS_Byte_Indices is unsigned_short range 0 .. 23;
   -- Grey Scale shift register buffer 16 * 12 bits
   type GS_Buffers is array (GS_Byte_Indices) of aliased Unsigned_8;
   subtype DC_Byte_Indices is unsigned_short range 0 .. 11;
   -- Dot correction shift register buffer 16 * 6 bits
   type DC_Buffers is array (DC_Byte_Indices) of aliased Unsigned_8;

   type Buffers is record
      GS_TX : GS_Buffers := (others=> 0);
      DC_TX : DC_Buffers := (others => Unsigned_8'Last);
      -- Dot Correction Shift Register Buffer
      -- initialised to full brightness
      GS_RX : GS_Buffers; -- required for SPI_Transfer
      DC_RX : DC_Buffers; -- required for SPI_Transfer
   end record; -- Buffers

   type Buffer_Arrays is array (Chips) of Buffers;

   type GS_Table_Element is
      record
         Byte_Address, Nibble_Address : GS_Byte_Indices;
         High_Nibble : Boolean;
      end record; -- GS_Table_Element

   GS_Table : constant array (LED_Channels) of GS_Table_Element :=
     (
      (23, 22, True), (21, 22, False),
      (20, 19, True), (18, 19, False),
      (17, 16, True), (15, 16, False),
      (14, 13, True), (12, 13, False),
      (11, 10, True), (09, 10, False),
      (08, 07, True), (06, 07, False),
      (05, 04, True), (03, 04, False),
      (02, 01, True), (00, 01, False)
     );
   -- High_Nibble True means that the four most significant bits of the
   -- greyscale value for that LED occupy the low Nibble. Conversly
   -- High_Nibble False means that the four least significant bits of the
   -- greyscale value for that LED occupy the high Nibble

   type DC_Table_Element is record
      First_Byte, Second_Byte : DC_Byte_Indices;
   end record; -- DC_Table_Element

   Byte_Mask        : constant Unsigned_16 := 2#0000000011111111#;
   Low_Nibble_Mask  : constant Unsigned_16 := 2#0000000000001111#;
   High_Nibble_Mask : constant Unsigned_16 := 2#0000000011110000#;

   DC_Table : constant array (LED_Channels) of DC_Table_Element :=
     (
        (11, 11), (11, 10), (10, 09), (09, 09),
      (08, 08), (08, 07), (07, 06), (06, 06),
      (05, 05), (05, 04), (04, 03), (03, 03),
      (02, 02), (02, 01), (01, 00), (00, 00)
     );

   DC_0_Mask    : constant Unsigned_8 := 2#00111111#;
   DC_1_Lo_Mask : constant Unsigned_8 := 2#11000000#;
   DC_1_Hi_Mask : constant Unsigned_8 := 2#00001111#;
   DC_2_Lo_Mask : constant Unsigned_8 := 2#11110000#;
   DC_2_Hi_Mask : constant Unsigned_8 := 2#00000011#;
   DC_3_Mask    : constant Unsigned_8 := 2#11111100#;

   Settling_Time : constant Time_Span := Milliseconds (1424);
   -- Settling time was recalculated based on a time constant of 2.4s giving a
   -- 1/2048 decay of approximately 17 s or 1.424 s.
   Start_Mask : constant  Greyscales := 2#0000100000000000#;

   type Displays is record
      Chip : Chips;
      Segment_Array : Segment_Arrays;
      Number : Hex_Digits := 0;
      Decimal_Lit : Boolean := False;
      Initialised : Boolean := False;
   end record; -- Displays

   type Display_Buffers is array (Display_7) of Displays;

   protected Display_State is

      procedure Set_Correction (Chip : in Chips;
                                LED_Channel : in LED_Channels;
                                Correction : in Corrections);
      -- Sets the dot correction value for a single LED channel

      function Get_Correction (Chip : in Chips;
                               LED_Channel : in LED_Channels)
                               return Corrections;
      -- Reads back the dot correction value for a single LED channel

      procedure Set_Greyscale (Chip : in Chips;
                               LED_Channel : in LED_Channels;
                               Greyscale : in GreyScales);
      -- Sets the greyscale value for a single LED channel

      function Get_Greyscale (Chip : in Chips;
                              LED_Channel : in LED_Channels) return Greyscales;
      -- Reads back the greyscale value for a single LED channel

      procedure Write_Corrections;
      -- Transfers dot correction data to all the cascaded chips.

      procedure Write_LEDs;
      -- Transfers the greyscale data to all the cascaded chips.

      procedure Blank_LEDs;
      -- Turns off the drive to all LEDs

      procedure Light_LEDs;
      -- Turns on the drive to all LEDs proportional to greyscale settings

      function Get_Blanking return Boolean;
      -- Returns True if blanking is currently effective, that is, no light.

      -- Ambient light management subprograms

      procedure AL_Initalise (Chip : in Chips;
                              Channel : in LED_Channels;
                              Comparitor : in GPIO_Pins);

      procedure AL_Write_LEDs;

      function AL_Current_Result return Greyscales;

      -- Seven segment -isplay management subprograms

      procedure Initialise_Digit (Display : in Display_7;
                                  Chip : in Chips;
                                  Segment_Array : in Segment_Arrays);

      procedure Set_Digit (Display : in Display_7;
                           Number : in Hex_Digits;
                           Greyscale : in GreyScales := Greyscales'Last;
                           Decimal_Lit : in Boolean := False;
                           No_Decimal : Boolean := False);
      -- Allows for LED_Chanel assigned to the DP to be used for another purpose

      function Get_Digit (Display : in Display_7) return Hex_Digits;

      function Get_Decimal (Display : in Display_7) return Boolean;

   private

      Buffer_Array : Buffer_Arrays;
      Is_Blanked : Boolean := True; -- Current state of blanking
      Corrections_Changed : Boolean := True;
      -- extra Greyscale set required after corections are changed see datasheet

      -- Variables associeted with reading of ambient light;
      Device : Chips;
      Output_Channel : LED_Channels;
      Input_Pin : GPIO_Pins;
      Output_Enabled : Boolean := False;
      Initialised : Boolean := False;
      First_LED_Write : Boolean := True;
      Settled : Time;
      Light_Value : Greyscales := Greyscales'Last / 2;
      -- Default value available even if the measurement system has not been
      -- iniatialised.
      Test_Value, Test_Mask: Greyscales;

      -- Seven Segment Display buffer
      Display_Buffer : Display_Buffers;

   end Display_State;

   procedure Set_Correction (Chip : in Chips;
                             LED_Channel : in LED_Channels;
                             Correction : in Corrections) is

      -- Sets the dot correction value for a single LED channel

   begin -- Set_Correction
      Display_State.Set_Correction (Chip, LED_Channel, Correction);
   end Set_Correction;

   function Get_Correction (Chip : in Chips;
                            LED_Channel : in LED_Channels) return Corrections is

      -- Reads back the dot correction value for a single LED channel

   begin -- Get_Correction
      return Display_State.Get_Correction (Chip, LED_Channel);
   end Get_Correction;

   procedure Set_Greyscale (Chip : in Chips;
                            LED_Channel : in LED_Channels;
                            Greyscale : in GreyScales) is

      -- Sets the greyscale value for a single LED channel

   begin -- Set_Greyscale
      Display_State.Set_Greyscale (Chip, LED_Channel, Greyscale);
   end Set_Greyscale;

   function Get_Greyscale (Chip : in Chips;
                           LED_Channel : in LED_Channels) return Greyscales is

      -- Reads back the greyscale value for a single LED channel

   begin -- Get_Greyscale
      return Display_State.Get_Greyscale (Chip, LED_Channel);
   end Get_Greyscale;

   procedure Write_Corrections is

      -- Transfers dot correction data to all the cascaded chips.

   begin -- Write_Corrections
      Display_State.Write_Corrections;
   end Write_Corrections;

   procedure Write_LEDs is

      -- Transfers the greyscale data to all the cascaded chips.

   begin -- Write_LEDs
      Display_State. Write_LEDs;
   end Write_LEDs;

   procedure Blank_LEDs is

      -- Turns off the drive to all LEDs

   begin -- Blank_LEDs
      Display_State.Blank_LEDs;
   end Blank_LEDs;

   procedure Light_LEDs is

      -- Turns on the drive to all LEDs proportional to greyscale settings

   begin -- Light_LEDs
      Display_State.Light_LEDs;
   end Light_LEDs;

   function Get_Blanking return Boolean is

      -- Returns True if blanking is currently effective, that is, no light.

   begin -- Get_Blanking
      return Display_State.Get_Blanking;
   end Get_Blanking;

   procedure Initialise_Ambient_Light (Chip : in Chips;
                                       Channel : in LED_Channels;
                                       Comparitor : GPIO_Pins := Gen4) is
      -- Initialises the measurement system Chip and Channel define the PWM output
      -- used to implement the neasurement. Comparitor defined the RPI input pin
      -- used to read the output from ambient light measurement bridge comparitor.

   begin -- Initialise_Ambient_Light
      Display_State.AL_Initalise (Chip, Channel, Comparitor);
   end Initialise_Ambient_Light;

   function Get_Ambient_Light return Greyscales is

      -- Returns the greyscale value required to balance the ambient light
      -- measurement bridge. Returns Greyscales'Last / 2 if called before the
      -- first measurement is completed or called before the measurement system
      -- has been initialised.

   begin -- Get_Ambient_Light
      return Display_State.AL_Current_Result;
   end Get_Ambient_Light;

   procedure Initialise_Digit (Display : in Display_7;
                               Chip : in Chips;
                               Segment_Array : in Segment_Arrays) is
   begin -- Initialise_Digit
      Display_State.Initialise_Digit (Display, Chip, Segment_Array);
   end Initialise_Digit;

   procedure Set_Digit (Display : in Display_7;
                        Number : in Hex_Digits;
                        Greyscale : in GreyScales := Greyscales'Last;
                        Decimal_Lit : in Boolean := False;
                        No_Decimal : Boolean := False) is

      -- Allows for LED_Chanel assigned to the DP to be used for another purpose

   begin -- Set_Digit
      Display_State.Set_Digit (Display, Number, Greyscale, Decimal_Lit,
                               No_Decimal);
   end Set_Digit;

   function Get_Digit (Display : in Display_7) return Hex_Digits is

   begin -- Get_Digit
      return Display_State.Get_Digit (Display);
   end Get_Digit;

   function Get_Decimal (Display : in Display_7) return Boolean is

   begin -- Get_Decimal
      return Display_State.Get_Decimal (Display);
   end Get_Decimal;

   protected body Display_State is

      procedure Set_Correction (Chip : in Chips;
                                LED_Channel : in LED_Channels;
                                Correction : in Corrections) is

         -- Sets the dot correction value for a single LED channel

         First_Byte : Unsigned_8 :=
           Unsigned_8 (Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).
                           First_Byte));
         Second_Byte : Unsigned_8 :=
           Unsigned_8 (Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).
                           Second_Byte));

      begin -- Set_Correction
         case LED_Channel mod 4 is
         when 0 =>
            First_Byte := (First_Byte and DC_1_Lo_Mask) or Correction;
            Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).First_Byte) :=
              First_Byte;
         when 1 =>
            First_Byte := (First_Byte and DC_0_Mask) or
              Shift_Left (Correction, 6);
            Second_Byte := (Second_Byte and DC_2_Lo_Mask) or
              Shift_Right (Correction, 2);
            Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).First_Byte) :=
              First_Byte;
            Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).Second_Byte) :=
              Second_Byte;
         when 2 =>
            First_Byte := (First_Byte and DC_1_Hi_Mask) or
              Shift_Left (Correction, 4);
            Second_Byte := (Second_Byte and DC_3_Mask) or
              Shift_Right (Correction, 4);
            Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).First_Byte) :=
              First_Byte;
            Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).Second_Byte) :=
              Second_Byte;
         when 3 =>
            First_Byte := (First_Byte and DC_2_Hi_Mask) or
              Shift_Left (Correction, 2);
            Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).First_Byte) :=
              First_Byte;
         when others =>
            null; -- should be unreachable
         end case; -- LED_Channel mod 4
      end Set_Correction;

      function Get_Correction (Chip : in Chips;
                               LED_Channel : in LED_Channels)
                               return Corrections is
         -- Reads back the dot correction value for a single LED channel

         First_Byte : constant Unsigned_8 :=
           Unsigned_8 (Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).
                           First_Byte));
         Second_Byte : constant Unsigned_8 :=
           Unsigned_8 (Buffer_Array (Chip).DC_TX (DC_Table (LED_Channel).
                           Second_Byte));
         Result : Corrections;

      begin -- Get_Correction
         case LED_Channel mod 4 is
         when 0 =>
            Result := Corrections (First_Byte and DC_0_Mask);
         when 1 =>
            Result := Corrections
              (Shift_Right (First_Byte, 6) or
                   Shift_Left (Second_Byte and DC_1_Hi_Mask, 2));
         when 2 =>
            Result := Corrections
              (Shift_Right (First_Byte, 4) or
                   Shift_Left (Second_Byte and DC_2_Hi_Mask, 4));
         when 3 =>
            Result := Corrections (Shift_Right (First_Byte, 2));
         when others =>
            null; -- should be unreachable
         end case; -- LED_Channel mod 4
         return Result;
      end Get_Correction;

      procedure Set_Greyscale (Chip : in Chips;
                               LED_Channel : in LED_Channels;
                               Greyscale : in GreyScales) is

         -- Sets the greyscale value for a single LED channel

         Two_Nibble_Store : Unsigned_8;

         Start_Mask : constant  Greyscales := 2#0000100000000000#;
      begin -- Set_Greyscale
         Two_Nibble_Store := Unsigned_8
           (Buffer_Array (Chip).GS_TX (GS_Table (Led_Channel).Nibble_Address));
         if GS_Table (LED_Channel).High_Nibble then
            Two_Nibble_Store := Two_Nibble_Store and
              Unsigned_8 (High_Nibble_Mask);
            -- Preserve the high nibble, four least least sifgnificant bits for
            -- another LED channel.
            -- Clear low nibble where the four most significant bits of
            -- Greyscale value are to be written.
            Two_Nibble_Store := Two_Nibble_Store or Unsigned_8
              (Shift_Right (Greyscale, 8) and Low_Nibble_Mask);
            Buffer_Array (Chip).GS_TX (GS_Table (Led_Channel).Byte_Address) :=
              Unsigned_8 (Greyscale and Byte_Mask);
            -- Eight least significant bits of the Greyscale value form the byte
            -- field of the buffer.
         else
            Two_Nibble_Store := Two_Nibble_Store and
              Unsigned_8 (Low_Nibble_Mask);
            -- Preserve the low nibble, four most significant bits for another
            -- another LED channel.
            -- Clear nibble where the four least significant bits of Greyscale
            -- value are to be written
            Two_Nibble_Store := Two_Nibble_Store or
              Unsigned_8 (Shift_Left (Greyscale, 4) and High_Nibble_Mask);
            Buffer_Array (Chip).GS_TX (GS_Table (Led_Channel).Byte_Address) :=
              Unsigned_8 (Shift_Right (Greyscale, 4) and Byte_Mask);
         end if; -- GS_Table (LED_Channel).High_Nibble
         Buffer_Array (Chip).GS_TX (GS_Table (Led_Channel).Nibble_Address) :=
           Two_Nibble_Store;
      end Set_Greyscale;

      function Get_Greyscale (Chip : in Chips;
                              LED_Channel : in LED_Channels)
                              return Greyscales is

         -- Reads back the greyscale value for a single LED channel

         Result : Greyscales;

      begin -- Get_Greyscale
         if GS_Table (LED_Channel).High_Nibble then
            Result :=  Low_Nibble_Mask and Unsigned_16
              (Buffer_Array (Chip).GS_TX (GS_Table (Led_Channel).
                   Nibble_Address));
            Result := Shift_Left (Result, 8);
            Result := Result or Unsigned_16
              (Buffer_Array (Chip).GS_TX (GS_Table (Led_Channel).Byte_Address));
         else
            Result :=
              Shift_Left (Unsigned_16 (Buffer_Array (Chip).GS_TX
                          (GS_Table (Led_Channel).Byte_Address)), 4);
            Result :=  Result or
              Shift_Right (High_Nibble_Mask and
                             Unsigned_16 (Buffer_Array (Chip).
                                   GS_TX (GS_Table (Led_Channel).
                                   Nibble_Address)), 4);
         end if; -- GS_Table (LED_Channel).High_Nibble
         return Result;
      end Get_Greyscale;

      procedure Write_Corrections is

         -- Transfers dot correction data to all the cascaded chips.

         C_Return : Int;
         Transfer : Buffers;
         TX_Ptr : constant access Unsigned_8 := Transfer.DC_TX (0)'Access;
         Rx_Ptr : constant access Unsigned_8 := Transfer.DC_RX (0)'Access;

      begin -- Write_Corrections
         Corrections_Changed := True;
         Write_Pin (Pin_High, VPRG_Pin); -- select dot correction SR
         for I in reverse Chips loop
            -- fiest chip in string, last sent!
            Transfer.DC_TX := Buffer_Array (I).DC_TX;
            C_Return := SPI_Transfer (TX_Ptr, RX_Ptr, DC_Byte_Indices'Last + 1);
            if C_Return /= 0 then
               raise SPI_Error with "Failed Write_Corrections, returned:" &
                 C_Return'Img;
            end if; -- C_Return /= 0
            Buffer_Array (I).DC_RX := Transfer.DC_RX;
         end loop; -- for I in Chips
         Write_Pin (Pin_Low, Blank_Pin);
         -- Unconditionally blank the display hardware design requires blankinng
         -- to effective for the XLATE pin to be driven.
         Write_Pin (Pin_Low, XLAT_Pin);
         Write_Pin (Pin_High, XLAT_Pin);
         if not Is_Blanked then
            Write_Pin (Pin_High, Blank_Pin);
            -- Restore previous blanking state
         end if; -- not Is_Blanked
         Write_Pin (Pin_Low, VPRG_Pin); -- set back to greyscale;
      end Write_Corrections;

      procedure Write_LEDs is

         -- Transfers the greyscale data to all the cascaded chips.

         C_Return : Int;
         Transfer : Buffers;
         TX_Ptr : constant access Unsigned_8 := Transfer.GS_TX (0)'Access;
         Rx_Ptr : constant access Unsigned_8 := Transfer.GS_RX (0)'Access;

      begin -- Write_LEDs
         -- Assumes VPRG_Pin and XLAT_Pin low on entry
         AL_Write_LEDs;
         loop -- repeat transfer
            for I in reverse Chips loop
               -- first chip in string, last sent!
               Transfer.GS_TX := Buffer_Array (I).GS_TX;
               C_Return := SPI_Transfer (TX_Ptr, RX_Ptr,
               GS_Byte_Indices'Last + 1);
            if C_Return /= 0 then
               raise SPI_Error with "Failed Write_LEDs, returned:" &
                 C_Return'Img;
            end if; -- C_Return /= 0
               Buffer_Array (I).GS_RX := Transfer.GS_RX;
            end loop; -- for I in Chips
            exit when not Corrections_Changed;
            -- transfer twice if corrections were applied in the last cycle;
            Corrections_Changed := False;
         end loop; -- repeat transfer
         Write_Pin (Pin_Low, XLAT_Pin);
         Write_Pin (Pin_High, XLAT_Pin);
      end Write_LEDs;

      procedure Blank_LEDs is

         -- Turns off the drive to all LEDs

      begin -- Blank_LEDs
         Output_Enabled := False;
         First_LED_Write := True;
         -- Abandon any ambient light measurement in progress
         Write_Pin (Pin_Low, Blank_Pin);
         Is_Blanked := True;
      end Blank_LEDs;

      procedure Light_LEDs is
         -- Turns on the drive to all LEDs proportional to greyscale settings

      begin -- Light_LEDs
         Write_Pin (Pin_High, Blank_Pin);
         Is_Blanked := False;
         -- Restart ambient light measurement
         Output_Enabled := True;
      end Light_LEDs;

      function Get_Blanking return Boolean is

         -- Returns True if blanking is currently effective, that is, no light.

      begin -- Get_Blanking
         return Is_Blanked;
      end Get_Blanking;

      -- Ambient light mamagement subprograms

      procedure AL_Initalise (Chip : in Chips;
                              Channel : in LED_Channels;
                              Comparitor : in GPIO_Pins) is

      begin --  AL_Initalise
         Device := Chip;
         Output_Channel := Channel;
         Input_Pin := Comparitor;
         Bind_Pin (Input_Pin, In_Pin);
         Initialised := True;
      end  AL_Initalise;

      procedure AL_Write_LEDs is

      begin -- AL_Write_LEDs
         if Initialised and Output_Enabled then
            if First_LED_Write then
               Test_Value := Start_Mask;
               Test_Mask := Start_Mask;
               Set_Greyscale (Device, Output_Channel, Test_Value);
               Settled := Clock + Settling_Time;
               First_LED_Write := False;
            elsif Clock >= Settled then
               if Read_Pin (Input_Pin) then
                  Test_Value := Test_Value xor Test_Mask;
                  -- previous Test_Value was too high, unset last bit set
               end if; -- Read_Pin (Input_Pin)
               Test_Mask := Shift_Right (Test_Mask, 1);
               if Test_Mask = 0 then
                  -- measurement complete
                  Light_Value := Test_Value;
                  Test_Value := Start_Mask;
                  Test_Mask := Start_Mask;
               else
                  Test_Value := Test_Value or Test_Mask;
               end if; -- Test_Mask = 0
               Set_Greyscale (Device, Output_Channel, Test_Value);
               Settled := Clock + Settling_Time;
            end if; -- First_LED_Write
            -- Do nothing if it was not the first write and insufficient
            -- settling time has elapsed
         end if; -- Initialied and Output_Enabled
         -- do nothing if there has been no previous output setup
      end AL_Write_LEDs;

      function AL_Current_Result return Greyscales is

      begin -- AL_Current_Result
         return Light_Value;
      end AL_Current_Result;

      procedure Initialise_Digit (Display : in Display_7;
                                  Chip : in Chips;
                                  Segment_Array : in Segment_Arrays) is

      begin -- Initialise_Digit
         Display_Buffer (Display).Chip := Chip;
         Display_Buffer (Display).Segment_Array := Segment_Array;
         Display_Buffer (Display).Initialised := True;
      end Initialise_Digit;

      procedure Set_Digit (Display : in Display_7;
                           Number : in Hex_Digits;
                           Greyscale : in GreyScales := Greyscales'Last;
                           Decimal_Lit : in Boolean := False;
                           No_Decimal : Boolean := False) is

         -- Allows for LED_Chanel assigned to the DP to be used for another
         -- purpose

         Light : array (Segments) of Boolean := (others => False);

         --   -a-
         --   f b
         --   -g-
         --   e c
         --   -d-Dp

      begin -- Set_Digit
         if Display_Buffer (Display).Initialised then
            Display_Buffer (Display).Number := Number;
            Display_Buffer (Display).Decimal_Lit := Decimal_Lit;
            case Number is
            when 0 =>
               Light (Segment_a) := True;
               Light (Segment_b) := True;
               Light (Segment_c) := True;
               Light (Segment_d) := True;
               Light (Segment_e) := True;
               Light (Segment_f) := True;
            when 1 =>
               Light (Segment_b) := True;
               Light (Segment_c) := True;
            when 2 =>
               Light (Segment_a) := True;
               Light (Segment_b) := True;
               Light (Segment_d) := True;
               Light (Segment_e) := True;
               Light (Segment_g) := True;
            when 3 =>
               Light (Segment_a) := True;
               Light (Segment_b) := True;
               Light (Segment_c) := True;
               Light (Segment_d) := True;
               Light (Segment_g) := True;
            when 4 =>
               Light (Segment_b) := True;
               Light (Segment_c) := True;
               Light (Segment_f) := True;
               Light (Segment_g) := True;
            when 5 =>
               Light (Segment_a) := True;
               Light (Segment_c) := True;
               Light (Segment_d) := True;
               Light (Segment_f) := True;
               Light (Segment_g) := True;
            when 6 =>
               Light (Segment_a) := True;
               Light (Segment_c) := True;
               Light (Segment_d) := True;
               Light (Segment_e) := True;
               Light (Segment_f) := True;
               Light (Segment_g) := True;
            when 7 =>
               Light (Segment_a) := True;
               Light (Segment_b) := True;
               Light (Segment_c) := True;
            when 8 =>
               Light (Segment_a) := True;
               Light (Segment_b) := True;
               Light (Segment_c) := True;
               Light (Segment_d) := True;
               Light (Segment_e) := True;
               Light (Segment_f) := True;
               Light (Segment_g) := True;
            when 9 =>
               Light (Segment_a) := True;
               Light (Segment_b) := True;
               Light (Segment_c) := True;
               Light (Segment_d) := True;
               Light (Segment_f) := True;
               Light (Segment_g) := True;
            when 16#A# =>
               Light (Segment_a) := True;
               Light (Segment_b) := True;
               Light (Segment_c) := True;
               Light (Segment_e) := True;
               Light (Segment_f) := True;
               Light (Segment_g) := True;
            when 16#b# =>
               Light (Segment_c) := True;
               Light (Segment_d) := True;
               Light (Segment_e) := True;
               Light (Segment_f) := True;
               Light (Segment_g) := True;
            when 16#C# =>
               Light (Segment_a) := True;
               Light (Segment_d) := True;
               Light (Segment_e) := True;
               Light (Segment_f) := True;
            when 16#d# =>
               Light (Segment_b) := True;
               Light (Segment_c) := True;
               Light (Segment_d) := True;
               Light (Segment_e) := True;
               Light (Segment_g) := True;
            when 16#E# =>
               Light (Segment_a) := True;
               Light (Segment_d) := True;
               Light (Segment_e) := True;
               Light (Segment_f) := True;
               Light (Segment_g) := True;
            when 16#F# =>
               Light (Segment_a) := True;
               Light (Segment_e) := True;
               Light (Segment_f) := True;
               Light (Segment_g) := True;
            end case; -- Number
            Light (Segment_DP) := Decimal_Lit;
            for S in Segments range Segment_a .. Segment_g loop
               if Light (S) then
                  Set_Greyscale (Display_Buffer (Display).Chip,
                                 Display_Buffer (Display).Segment_Array (S),
                                 Greyscale);
               else
                  Set_Greyscale (Display_Buffer (Display).Chip,
                                 Display_Buffer (Display).Segment_Array (S),
                                 Greyscales'First);
               end if; -- Light (S)
            end loop; -- S in Segments
            if not No_Decimal then
               if Light (Segment_DP) then
                  Set_Greyscale (Display_Buffer (Display).Chip,
                                 Display_Buffer (Display).
                                   Segment_Array (Segment_DP),
                                 Greyscale);
               else
                  Set_Greyscale (Display_Buffer (Display).Chip,
                                 Display_Buffer (Display).
                                   Segment_Array (Segment_DP),
                                 Greyscales'First);
               end if; -- Light (Segment_DP)
            end if; -- not No_Decimal
         else
            raise Uninitialised_Digit with "Set_Digit:" &
              Display_7'Image (Display);
         end if; -- Display_Buffer (Display).Initialised
      end Set_Digit;

      function Get_Digit (Display : in Display_7) return Hex_Digits is

      begin -- Get_Digit
         if Display_Buffer (Display).Initialised then
            return Display_Buffer (Display).Number;
         else
            raise Uninitialised_Digit with "Get_Digit:" &
              Display_7'Image (Display);
         end if; -- Display_Buffer (Display).Initialised
      end Get_Digit;

      function Get_Decimal (Display : in Display_7) return Boolean is

      begin -- Get_Decimal
         if Display_Buffer (Display).Initialised then
            return Display_Buffer (Display).Decimal_Lit;
         else
            raise Uninitialised_Digit with "Get_Decimal:" &
              Display_7'Image (Display);
         end if; -- Display_Buffer (Display).Initialised
      end Get_Decimal;

   end Display_State;

   procedure Initialize (SPI_State : in out SPI_States) is
      -- Initialises both the GPIO pins used to interface to the  TIC5940s and
      -- SPI device. SPI_State is controlled so that the SPI device will be
      -- closed when this package goes out of scope;
   
      C_Return : int;

   begin -- Initialize
      Bind_Pin (VPRG_Pin, Out_Pin);
      Bind_Pin (XLAT_Pin, Out_Pin);
      Bind_Pin (Blank_Pin, Out_Pin);
      Write_Pin (Pin_Low, VPRG_Pin); -- select greyscale SR
      Write_Pin (Pin_High, XLAT_Pin);
      -- ready to shift in greyscale values
      Write_Pin (Pin_Low, Blank_Pin); -- initially blanks LEDs
      -- Note Is_Blanked is initialised to True to match this
      C_Return := SPI_Open (SPI_State.Device, 0, SPI_State.Speed);
      if C_Return /= 0 then
         raise SPI_Error with "Failed open, returned:" & C_Return'Img;
      end if; -- C_Return /= 0
      SPI_State.Enabled := True;
      Write_Corrections;
      Write_LEDs;
   end Initialize;

   procedure Finalize (SPI_State : in out SPI_States) is
      -- Closes SPI device when package goes out of scope.
      
      C_Return : int;

   begin -- Finalize
      if SPI_State.Enabled then
         C_Return := SPI_Close;
         if C_Return /= 0 then
            raise SPI_Error with "Failed close, returned:" & C_Return'Img;
         end if; -- C_Return /= 0
      end if; -- SPI_State.Enabled
      SPI_State.Enabled := False;
   end Finalize;

   SPI_State : SPI_States;
   -- Initialize will be called by the creation of SPI_State.

end TLC5940;
