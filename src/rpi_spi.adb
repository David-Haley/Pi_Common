with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Finalization; use Ada.Finalization;

-- Author    : David Haley
-- Created   : 01/07/2017
-- Last Edit : 02/07/2017
-- Transfers data to and from the Raspberry Pi SPI interface using the
-- spi_udp_server. The server must be started before the first call to
-- SPI_Transfer.

package body RPI_SPI is

   Server_Address : Sock_Addr_Type;
   Client_Socket : Socket_Type;
   SPI_Server_Port : constant Port_Type := 5000;

   Finalisation_Required : Controlled_Boolean;

   procedure SPI_Transfer (TX_Buffer : in Stream_Element_Array;
                           RX_Buffer : out Stream_Element_Array) is
      -- Writes contents of TX_Buffer to SPI interface and transfers data read
      -- from the SPI interface into the RX_Buffer. The TX_Buffer and the RX_Buffer
      -- must be the same length and the length limited to 256 bytes.
      TX_Last, RX_Last : Stream_Element_Offset;

   begin -- SPI_Transfer
      if TX_Buffer'Last - TX_Buffer'First /= RX_Buffer'Last - RX_Buffer'First then
         -- check that both buffers have the same length
         raise Bad_SPI_Request;
      end if;
      begin -- Socket R/W exception block
         Send_Socket (Client_Socket, TX_Buffer, TX_Last, Server_Address);
         Receive_Socket (Client_Socket, RX_Buffer, RX_Last, Server_Address);
      exception
         when others => raise Request_Failed;
      end; -- Socket R/W exception block
      if TX_Last - TX_Buffer'First /= RX_Last - RX_Buffer'First then
         -- Check that the same number of bytes have been received
         raise Request_Failed;
      end if;
   end Spi_Transfer;

   procedure Finalize (Object :in out Controlled_Boolean) is

   begin
      -- allow for Finalization being called more thane once
      if Object.State then
         Object.State := False;
         Close_Socket (Client_Socket);
      end if; -- Object.Active

   end Finalize;

begin -- RPI_SPI
   Create_Socket (Client_Socket, Family_Inet, Socket_Datagram); -- UDP sockets
   Server_Address.Addr := Addresses (Get_Host_By_Name ("localhost"), 1);
   Server_Address.Port := SPI_Server_Port;
exception
   when others => raise No_SPI_Server;
end RPI_SPI;
