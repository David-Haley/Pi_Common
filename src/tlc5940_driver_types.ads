-- This package declares types used by TLC5940 package.
-- Author    : David Haley
-- Created   : 28/06/2019
-- Last Edit : 05/04/2025

-- 20250405 : Lit_Greyscales added.
-- 20220609 : Package renamed.

with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

package TLC5940_Driver_Types is

   subtype LED_Channels is Natural range 0 .. 15;
   -- Range of LED channels within a single TLC5940 device
   subtype Corrections is Unsigned_8 range 0 .. 63;
   -- Dot correction range for TLC5940
   subtype Greyscales is Unsigned_16 range 0 .. 4095;
   -- Greyscale range for TLC5940
   subtype Lit_Greyscales is Greyscales range 1 .. Greyscales'Last;

   -- Declarations for seven segment displays

   type Segments is (Segment_a, Segment_b, Segment_c, Segment_d, Segment_e,
                     Segment_f, Segment_g, Segment_DP);

   type Segment_Arrays is array (Segments) of LED_Channels;

   package LED_Channel_IO is new Ada.Text_IO.Integer_IO (LED_Channels);

   package Correction_IO is new Ada.Text_IO.Modular_IO (Corrections);

   package Greyscale_IO is new Ada.Text_IO.Modular_IO (Greyscales);

end TLC5940_Driver_Types;
