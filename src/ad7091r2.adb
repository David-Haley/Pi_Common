-- Driver for Analogue devices AD7091R-2
-- provides a function for reading both channels of the A/D.
-- Author    : David Haley
-- Created   : 22/09/2017
-- Last Edit : 07/05/2017
-- 20220507: Converted to use SPI_Interface.ads, implemented in C and callable
-- from Ada.

package body AD7091R2 is

   --                                                FEDCBA9876543210
   Result_Address        : constant Unsigned_16 := 2#0000000000000000#;
   Channel_Address       : constant Unsigned_16 := 2#0000100000000000#;
   Configuration_Address : constant Unsigned_16 := 2#0001000000000000#;
   Register_Write        : constant Unsigned_16 := 2#0000010000000000#;
   Power_Down            : constant Unsigned_16 := 2#0000000000000001#;
   Channels_to_Read      : constant Unsigned_16 := 2#0000000000000011#;

   function AD_Transfer (Data : in Unsigned_16) return Unsigned_16 is
      -- Transfers Data to the ADC via the SPI interface and returns the result
      -- received on the SPI interface.
   
      Byte_Mask : constant Unsigned_16 := 2#0000000011111111#;
      Byte_Shift : constant Natural := 8;
      MSB : constant Natural := 0; -- MSB transmitted first
      LSB : constant Natural := 1;
      subtype Buffer_Indices is Natural range MSB .. LSB;
      type Buffers is array (Buffer_Indices) of aliased Unsigned_8;
      TX_Buffer, RX_Buffer : Buffers;
      TX_Buffer_Ptr : constant access Unsigned_8 := TX_Buffer (MSB)'Access;
      Rx_Buffer_Ptr : constant access Unsigned_8 := RX_Buffer (MSB)'Access;
      C_Return : int;

   begin -- AD_Transfer
      TX_Buffer (MSB) :=
        unsigned_8 (Byte_Mask and Shift_Right (Data, Byte_Shift));
      TX_Buffer (LSB) := unsigned_8 (Byte_Mask and Data);
      C_Return := SPI_Transfer (TX_Buffer_Ptr, RX_Buffer_Ptr,
                                unsigned_short (LSB - MSB + 1));
      if C_Return /= 0 then
         raise SPI_Error with "Failed transfer, returned: " & C_Return'Img;
      end if; -- C_Return /= 0
      return Shift_Left (Unsigned_16 (RX_Buffer (MSB)), Byte_Shift) or
        Unsigned_16 (RX_Buffer (LSB));
   end AD_Transfer;

   function AD_Read return A_Volt_Arrays is
      -- reads both channels of the A/D

      Result_Mask  : constant Unsigned_16 := 2#0000111111111111#;
      Channel_Mask : constant Unsigned_16 := 2#1110000000000000#;
      -- allows for 4 and 8 channel versions
      Channel_Shift : constant Natural := 13;
      Channel : A_Channels;
      A_Volt : A_Volt_Arrays;
      Channels_Read : array (A_Channels) of Boolean := (others => False);
      All_Channels_Read : Boolean := False;
      RX_Word : Unsigned_16;

   begin --  AD_Read
      while not All_Channels_Read loop
         -- Initiate A/D conversion
         Write_Pin (Pin_Low, AD_Sample_Pin);
         Write_Pin (Pin_High, AD_Sample_Pin);
         -- Read conversion result
         RX_Word := AD_Transfer (Result_Address);
         -- channel read is uncertain due to on chip sequencer; however by
         -- repeatedly reading all channels should be read
         Channel :=
           A_Channels (Shift_Right (RX_Word and Channel_Mask, Channel_Shift));
         Channels_Read (Channel) := True;
         A_Volt (Channel) := A_Volts (RX_Word and Result_Mask);
         All_Channels_Read := True;
         for I in A_Channels loop
            All_Channels_Read := All_Channels_Read and Channels_Read (I);
         end loop; -- I in A_Channels
      end loop; -- not All_Channels_Read
      return A_Volt;
   end AD_Read;

   procedure Initialize (SPI_State : in out SPI_States) is
      -- Initialises both the GPIO pins used to interface to the ADC and SPI
      -- device. SPI_State is controlled so that the SPI device will be closed
      -- when this package goes out of scope;
   
      Discarded_Result : Unsigned_16;
      C_Return : int;

   begin -- Initialize
      Bind_Pin (SPI_State.Reset_Pin, Out_Pin);
      Write_Pin (Pin_High, SPI_State.Reset_Pin);
      Write_Pin (Pin_Low, SPI_State.Reset_Pin);
      -- reset A/D must be more than 10ns
      Write_Pin (Pin_High, SPI_State.Reset_Pin);
      Bind_Pin (SPI_State.Sample_Pin, Out_Pin);
      Write_Pin (Pin_High, SPI_State.Sample_Pin); -- /CONVST is active low
      C_Return := SPI_Open (SPI_State.Device, 0, SPI_State.Speed);
      if C_Return /= 0 then
         raise SPI_Error with "Failed open, returned: " & C_Return'Img;
      end if; -- C_Return /= 0
      SPI_State.Enabled := True;
      Discarded_Result := AD_Transfer (Configuration_Address or
                                         Register_Write or
                                           Power_Down);
      Discarded_Result := AD_Transfer (Channel_Address or
                                         Register_Write or
                                           Channels_to_Read);
   end Initialize;

   procedure Finalize (SPI_State : in out SPI_States) is
      -- Closes SPI device when package goes out of scope.
      
      C_Return : int;

   begin -- Finalize
      if SPI_State.Enabled then
         C_Return := SPI_Close;
         if C_Return /= 0 then
            raise SPI_Error with "Failed close, returned: " & C_Return'Img;
         end if; -- C_Return /= 0
      end if; -- SPI_State.Enabled
      SPI_State.Enabled := False;
   end Finalize;

   SPI_State : SPI_States;
   -- Initialize will be called by the creation of SPI_State.

end AD7091R2;
