pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package gpio_driver_h is

  -- Simple GPIO driver for RPI intended to be callable from Ada. The
  -- driver uses libgpiod.
  -- Author    : David Haley
  -- Created   : 22/07/2025
  -- Last Edit : 31/07/2025
   function Open_GPIO_Chip (Chip_Name : Interfaces.C.Strings.chars_ptr) return int  -- gpio_driver.h:7
   with Import => True, 
        Convention => C, 
        External_Name => "Open_GPIO_Chip";

  -- Must be called before any other GPIO functions are used.
  -- Returns 0 if no error, otherwise the errno set by the library call.
   procedure Close_GPIO_Line (Line_Number : int)  -- gpio_driver.h:11
   with Import => True, 
        Convention => C, 
        External_Name => "Close_GPIO_Line";

  -- Close one line only. Can be used to close a line whilst retaining
  -- others in use. Close_GPIO_Chip closes all lines currently in use.
   procedure Close_GPIO_Chip  -- gpio_driver.h:15
   with Import => True, 
        Convention => C, 
        External_Name => "Close_GPIO_Chip";

  -- Should be called when the use of GPIO is no longer required. Closes
  -- all lines currently in use.
   function Open_Output (Line_Name : Interfaces.C.Strings.chars_ptr) return int  -- gpio_driver.h:19
   with Import => True, 
        Convention => C, 
        External_Name => "Open_Output";

  -- Opens the named line as an output and returns the line number or -1
  -- if the call fails.
   function Set_High (Line_Number : int) return int  -- gpio_driver.h:23
   with Import => True, 
        Convention => C, 
        External_Name => "Set_High";

  -- Set the specified line high, returns -1 on failure.
   function Set_Low (Line_Number : int) return int  -- gpio_driver.h:26
   with Import => True, 
        Convention => C, 
        External_Name => "Set_Low";

  -- Set the specified line low, returns -1 on failure.
   function Pulse_High (Line_Number : int) return int  -- gpio_driver.h:29
   with Import => True, 
        Convention => C, 
        External_Name => "Pulse_High";

  -- Sets a previously low output high for the minimum time possible,
  -- time will depend on Pi speed and overhead. if output was initially
  -- high it will be set low, this is not treated as an error.
  -- Returns error number if a failure occurs.
   function Pulse_Low (Line_Number : int) return int  -- gpio_driver.h:35
   with Import => True, 
        Convention => C, 
        External_Name => "Pulse_Low";

  -- Sets a previously high output low for the minimum time possible,
  -- time will depend on Pi speed and overhead. if output was initially
  -- low it will be set high, this is not treated as an error.
  -- Returns error number if a failure occurs.
   function Open_Input (Line_Name : Interfaces.C.Strings.chars_ptr) return int  -- gpio_driver.h:41
   with Import => True, 
        Convention => C, 
        External_Name => "Open_Input";

  -- Opens the named line as an input and returns the line number or -1
  -- if the call fails.
   function Read_Input (Line_Number : int) return int  -- gpio_driver.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "Read_Input";

  -- Reads one line returns 1 for high 0 for low and -1 on failure.
end gpio_driver_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
