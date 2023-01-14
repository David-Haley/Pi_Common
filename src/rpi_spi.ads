with Ada.Streams; use Ada.Streams;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Finalization; use Ada.Finalization;

-- Author    : David Haley
-- Created   : 01/07/2017
-- Last Edit : 02/07/2017
-- Transfers data to and from the Raspberry Pi SPI interface using the
-- spi_udp_server. The server must be started before the first call to
-- SPI_Transfer.

package RPI_SPI is

   procedure SPI_Transfer (TX_Buffer : in Stream_Element_Array;
                           RX_Buffer : out Stream_Element_Array);
   -- Writes contents of TX_Buffer to SPI interface and transfers data read
   -- from the SPI interface into the RX_Buffer. The TX_Buffer and the RX_Buffer
   -- must be the same length and the length limited to 256 bytes.

   Bad_SPI_Request, Request_Failed, No_SPI_Server : exception;

private

   type Controlled_Boolean is new controlled with
      record
         State : Boolean := True;
      end record;

   procedure Finalize (Object :in out Controlled_Boolean);

end RPI_SPI;
