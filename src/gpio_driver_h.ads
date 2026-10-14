pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package GPIO_Driver_h is

  -- Simple GPIO driver for RPI intended to be callable from Ada. The
  -- driver uses libgpiod.
  -- Author    : David Haley
  -- Created   : 22/07/2025
  -- Last Edit : 14/10/2026

  -- 20261014 : Errors returned as -errno, Open_Output_Initial added.
   function Open_GPIO_Chip (Chip_Name : Interfaces.C.Strings.chars_ptr) return int  -- gpio_driver.h:13
   with Import => True,
        Convention => C,
        External_Name => "Open_GPIO_Chip";

  -- Must be called before any other GPIO functions are used.
  -- Returns 0 if no error, otherwise -errno.
   procedure Close_GPIO_Line (Line_Number : int)  -- gpio_driver.h:17
   with Import => True,
        Convention => C,
        External_Name => "Close_GPIO_Line";

  -- Close one line only. Can be used to close a line whilst retaining
  -- others in use. Closing a line that is not open has no effect.
   procedure Close_GPIO_Chip  -- gpio_driver.h:21
   with Import => True,
        Convention => C,
        External_Name => "Close_GPIO_Chip";

  -- Should be called when the use of GPIO is no longer required. Closes
  -- all lines currently in use.
   function Open_Output (Line_Name : Interfaces.C.Strings.chars_ptr) return int  -- gpio_driver.h:25
   with Import => True,
        Convention => C,
        External_Name => "Open_Output";

  -- Opens the named line as an output, initially low, and returns the line
  -- number or -errno if the call fails.
   function Open_Output_Initial (Line_Name : Interfaces.C.Strings.chars_ptr; Initial_High : int) return int  -- gpio_driver.h:29
   with Import => True,
        Convention => C,
        External_Name => "Open_Output_Initial";

  -- Opens the named line as an output, initially high if Initial_High is
  -- non zero otherwise low. Returns the line number or -errno if the call
  -- fails.
   function Set_High (Line_Number : int) return int  -- gpio_driver.h:34
   with Import => True,
        Convention => C,
        External_Name => "Set_High";

  -- Set the specified line high, returns 0 on success or -errno on failure.
   function Set_Low (Line_Number : int) return int  -- gpio_driver.h:37
   with Import => True,
        Convention => C,
        External_Name => "Set_Low";

  -- Set the specified line low, returns 0 on success or -errno on failure.
   function Pulse_High (Line_Number : int) return int  -- gpio_driver.h:40
   with Import => True,
        Convention => C,
        External_Name => "Pulse_High";

  -- Sets a previously low output high for the minimum time possible,
  -- time will depend on Pi speed and overhead. if output was initially
  -- high it will be set low, this is not treated as an error.
  -- Returns 0 on success or -errno on failure.
   function Pulse_Low (Line_Number : int) return int  -- gpio_driver.h:46
   with Import => True,
        Convention => C,
        External_Name => "Pulse_Low";

  -- Sets a previously high output low for the minimum time possible,
  -- time will depend on Pi speed and overhead. if output was initially
  -- low it will be set high, this is not treated as an error.
  -- Returns 0 on success or -errno on failure.
   function Open_Input (Line_Name : Interfaces.C.Strings.chars_ptr) return int  -- gpio_driver.h:52
   with Import => True,
        Convention => C,
        External_Name => "Open_Input";

  -- Opens the named line as an input and returns the line number or -errno
  -- if the call fails. Bias (pull up / down) is left as configured.
   function Read_Input (Line_Number : int) return int  -- gpio_driver.h:56
   with Import => True,
        Convention => C,
        External_Name => "Read_Input";

  -- Reads one line, input or output, returns 1 for high 0 for low and -errno
  -- on failure.
end GPIO_Driver_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
