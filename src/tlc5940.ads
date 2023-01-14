
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
-- Created   : 07/08/2017
-- Last Edit : 09/06/2022

-- 20220609 : Converted to use SPI_Interface directly called C.
-- 20220118 : Implementation of protection on display state, potentially makes
-- this package thread safe.
-- 20190718 : No Decimal made in parameter for Set_Digit
-- 20190716 : Default comparitor pin changed to Gen4 to match Clock Hardware.
-- No Decimal implemented for Set_Digit
-- 20190628 : some declarations transferred to Driver_Types
-- 20190325 : Ambient light functionality added.
-- 20190323 : Note with respect to concurrancy added and other corrections to
-- package desctiption.
-- 20190321 : Generic seven segment support added must be at least 1?
-- 20190320 : Corrections and Greyscales made subtypes of Unsigned_8 amd
-- Unsigned_16 to symplify packing and unpacking operations.
-- 16/01/2019 : Greyscale clock, Blanking and Xlate managed by hardware to
-- ensure consistent brightness
-- 01/10/2018 : Greyscale_Clock_Frequency converted to an integer
-- 10/09/2017 : Greyscale made modular

with Ada.Finalization; use Ada.Finalization;
with RPi_GPIO; use RPi_GPIO;
with TLC5940_Driver_Types; Use TLC5940_Driver_Types;
with SPI_Interface; use SPI_Interface;

generic
   type Chips is (<>); -- The range of cascaded TLC5940 devices
   type Display_7 is (<>); -- Ramge of Seven segment displays
   VPRG_Pin  : GPIO_Pins := Gen0; -- selects Correction/Greyscale
   XLAT_Pin  : GPIO_Pins := Gen1; -- latches data, transfer when high
   Blank_Pin : GPIO_Pins := Gen2; -- resets grey scale counter, blanks LEDs
   SPI_Device : SPI_Devices := 0; -- SPI channel to be used
   SPI_Speed : SPI_Speeds := 12500000;
   -- 12.5 Mhz half maximum clock for AD7091R2 and exactly settable in RPI

package TLC5940 is

   procedure Set_Correction (Chip : in Chips;
                             LED_Channel : in LED_Channels;
                             Correction : in Corrections);
   -- Sets the dot correction value for a single LED channel

   function Get_Correction (Chip : in Chips;
                            LED_Channel : in LED_Channels) return Corrections;
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

   -- Ambient light level functionality
   -- Measures light level by a brige measurement, balancing a PWM output
   -- against a value dependent on ambient light intensity. The value is
   -- determined by a successive approximation, which is effectively a binary
   -- search for the measured value. A new approximation is tried when
   -- Write_LEDs is called, provided that approximately one second has elapsed
   -- since the last call to Write_LEDs. This means that a new measurement will
   -- require at least twelve calls to Write_LEDs and a minimum of 12 s. The
   -- process is deliberatly slow to allow for long time constants in light
   -- dependent resistors (LDRs).
   -- Explicetly blanking or setting of Corrections will restart the measurement
   -- process.

   procedure Initialise_Ambient_Light (Chip : in Chips;
                                       Channel : in LED_Channels;
                                       Comparitor : in GPIO_Pins := Gen4);
   -- Initialises the measurement system Chip and Channel define the PWM output
   -- used to implement the neasurement. Comparitor defined the RPI input pin
   -- used to read the output from ambient light measurement bridge comparitor.

   function Get_Ambient_Light return Greyscales;
   -- Returns the greyscale value required to balance the ambient light
   -- measurement bridge. Returns Greyscales'Last / 2  if called before the
   -- first measurement is completed or called before the measurement system
   -- has been initialised.

   -- Seven segment display functionality

   subtype Hex_Digits is Natural range 0 .. 16#0F#;

   Uninitialised_Digit : Exception;
   -- Exception raised if Set_Digit, Get_Digit or Get_Decimal is called for
   -- a Display which has not been initialised by a call to Initialise_Digit;

   procedure Initialise_Digit (Display : in Display_7;
                               Chip : in Chips;
                               Segment_Array : in Segment_Arrays);

   procedure Set_Digit (Display : in Display_7;
                        Number : in Hex_Digits;
                        Greyscale : in GreyScales := Greyscales'Last;
                        Decimal_Lit : in Boolean := False;
                        No_Decimal : in Boolean := False);
   -- Allows for LED_Chanel assigneb to the DP to be used for another purpose

   function Get_Digit (Display : in Display_7) return Hex_Digits;

   function Get_Decimal (Display : in Display_7) return Boolean;

   SPI_Error : exception;

   private

   type SPI_States is new Limited_Controlled with record
      Enabled : Boolean := False;
      Device : SPI_Devices := SPI_Device;
      Speed : SPI_Speeds := SPI_Speed;
   end record; -- SPI_States

   overriding
   procedure Initialize (SPI_State : in out SPI_States);

   overriding
   procedure Finalize (SPI_State : in out SPI_States);

end TLC5940;
