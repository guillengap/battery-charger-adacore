pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__battery_charger.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__battery_charger.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;

package body ada_main is

   E095 : Short_Integer; pragma Import (Ada, E095, "ada__tags_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "system__soft_links_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "system__exception_table_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "system__bb__timing_events_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "ada__streams_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__finalization_root_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "ada__finalization_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "system__storage_pools_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "system__finalization_masters_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__real_time_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "system__pool_global_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "system__tasking__protected_objects_E");
   E194 : Short_Integer; pragma Import (Ada, E194, "system__tasking__protected_objects__multiprocessors_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "system__tasking__restricted__stages_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "cortex_m__cache_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "hal__audio_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "hal__block_drivers_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "hal__gpio_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "hal__i2c_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "hal__real_time_clock_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "hal__sdmmc_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "hal__spi_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "hal__time_E");
   E251 : Short_Integer; pragma Import (Ada, E251, "cs43l22_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "hal__uart_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "lis3dsh_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "lis3dsh__spi_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "ravenscar_time_E");
   E230 : Short_Integer; pragma Import (Ada, E230, "sdmmc_init_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "stm32__adc_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "stm32__dac_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "stm32__dma__interrupts_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "stm32__exti_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "stm32__power_control_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "stm32__rtc_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "stm32__spi_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "stm32__spi__dma_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "stm32__i2c_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "stm32__usarts_E");
   E236 : Short_Integer; pragma Import (Ada, E236, "stm32__sdmmc_interrupt_E");
   E213 : Short_Integer; pragma Import (Ada, E213, "stm32__i2s_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "stm32__i2c__dma_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "stm32__gpio_E");
   E225 : Short_Integer; pragma Import (Ada, E225, "stm32__sdmmc_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "stm32__syscfg_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "stm32__device_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "stm32__pwm_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "stm32__setup_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "stm32__board_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "last_chance_handler_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "stm32__user_button_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");
      procedure Install_Restricted_Handlers_Sequential;
      pragma Import (C,Install_Restricted_Handlers_Sequential, "__gnat_attach_all_handlers");

      Partition_Elaboration_Policy : Character;
      pragma Import (C, Partition_Elaboration_Policy, "__gnat_partition_elaboration_policy");

      procedure Activate_All_Tasks_Sequential;
      pragma Import (C, Activate_All_Tasks_Sequential, "__gnat_activate_all_tasks");
      procedure Start_Slave_CPUs;
      pragma Import (C, Start_Slave_CPUs, "__gnat_start_slave_cpus");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := 0;
      WC_Encoding := 'b';
      Locking_Policy := 'C';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := 'F';
      Partition_Elaboration_Policy := 'S';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, True, False, False, False, False, True, False, 
           False, False, False, False, False, False, True, True, 
           False, False, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, True, 
           True, False, False, True, True, False, False, False, 
           True, False, False, False, False, True, False, True, 
           True, False, False, False, False, True, True, True, 
           True, True, False, True, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, True, False, False, True, 
           False, False, False, False, False, True, True, False, 
           True, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, True, True, False, False, False, 
           False, False, True, True, True, True, False, False, 
           False, False, False, True, True, False, True, True, 
           False, True, True, False, True, False, False, False, 
           False, False, True, False, False, True, False, False, 
           False, True, True, False, False, False, True, False, 
           False, False, True, False, False, False, False, False, 
           False, False, False, False, True, True, True, False, 
           False, True, False, True, True, True, False, True, 
           True, False, False, True, True, True, False, False, 
           True, False, False, False, True, False, False, False, 
           False, True, False),
         Count => (0, 0, 0, 1, 0, 0, 0, 0, 4, 0),
         Unknown => (False, False, False, False, False, False, False, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 1;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E051 := E051 + 1;
      Ada.Tags'Elab_Body;
      E095 := E095 + 1;
      System.Bb.Timing_Events'Elab_Spec;
      E109 := E109 + 1;
      E053 := E053 + 1;
      Ada.Streams'Elab_Spec;
      E126 := E126 + 1;
      System.Finalization_Root'Elab_Spec;
      E134 := E134 + 1;
      Ada.Finalization'Elab_Spec;
      E132 := E132 + 1;
      System.Storage_Pools'Elab_Spec;
      E136 := E136 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E129 := E129 + 1;
      Ada.Real_Time'Elab_Body;
      E006 := E006 + 1;
      System.Pool_Global'Elab_Spec;
      E138 := E138 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E188 := E188 + 1;
      System.Tasking.Protected_Objects.Multiprocessors'Elab_Body;
      E194 := E194 + 1;
      System.Tasking.Restricted.Stages'Elab_Body;
      E207 := E207 + 1;
      E228 := E228 + 1;
      HAL.AUDIO'ELAB_SPEC;
      E214 := E214 + 1;
      HAL.BLOCK_DRIVERS'ELAB_SPEC;
      E237 := E237 + 1;
      HAL.GPIO'ELAB_SPEC;
      E176 := E176 + 1;
      HAL.I2C'ELAB_SPEC;
      E184 := E184 + 1;
      HAL.REAL_TIME_CLOCK'ELAB_SPEC;
      E218 := E218 + 1;
      HAL.SDMMC'ELAB_SPEC;
      E232 := E232 + 1;
      HAL.SPI'ELAB_SPEC;
      E124 := E124 + 1;
      HAL.TIME'ELAB_SPEC;
      E252 := E252 + 1;
      CS43L22'ELAB_SPEC;
      CS43L22'ELAB_BODY;
      E251 := E251 + 1;
      HAL.UART'ELAB_SPEC;
      E248 := E248 + 1;
      LIS3DSH'ELAB_SPEC;
      LIS3DSH'ELAB_BODY;
      E142 := E142 + 1;
      LIS3DSH.SPI'ELAB_SPEC;
      LIS3DSH.SPI'ELAB_BODY;
      E254 := E254 + 1;
      Ravenscar_Time'Elab_Spec;
      Ravenscar_Time'Elab_Body;
      E256 := E256 + 1;
      E230 := E230 + 1;
      STM32.ADC'ELAB_SPEC;
      E153 := E153 + 1;
      E159 := E159 + 1;
      E200 := E200 + 1;
      E172 := E172 + 1;
      E220 := E220 + 1;
      STM32.RTC'ELAB_SPEC;
      STM32.RTC'ELAB_BODY;
      E217 := E217 + 1;
      STM32.SPI'ELAB_SPEC;
      STM32.SPI'ELAB_BODY;
      E240 := E240 + 1;
      STM32.SPI.DMA'ELAB_SPEC;
      STM32.SPI.DMA'ELAB_BODY;
      E242 := E242 + 1;
      STM32.I2C'ELAB_SPEC;
      STM32.USARTS'ELAB_SPEC;
      STM32.I2S'ELAB_SPEC;
      STM32.I2C.DMA'ELAB_SPEC;
      STM32.GPIO'ELAB_SPEC;
      STM32.SDMMC'ELAB_SPEC;
      E236 := E236 + 1;
      STM32.GPIO'ELAB_BODY;
      E165 := E165 + 1;
      STM32.DEVICE'ELAB_SPEC;
      E148 := E148 + 1;
      STM32.SDMMC'ELAB_BODY;
      E225 := E225 + 1;
      STM32.I2S'ELAB_BODY;
      E213 := E213 + 1;
      STM32.I2C.DMA'ELAB_BODY;
      E186 := E186 + 1;
      STM32.I2C'ELAB_BODY;
      E180 := E180 + 1;
      E170 := E170 + 1;
      STM32.USARTS'ELAB_BODY;
      E246 := E246 + 1;
      STM32.PWM'ELAB_SPEC;
      STM32.PWM'ELAB_BODY;
      E258 := E258 + 1;
      E146 := E146 + 1;
      STM32.BOARD'ELAB_SPEC;
      E123 := E123 + 1;
      E120 := E120 + 1;
      STM32.USER_BUTTON'ELAB_BODY;
      E260 := E260 + 1;
      Install_Restricted_Handlers_Sequential;
      Activate_All_Tasks_Sequential;
      Start_Slave_CPUs;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_battery_charger");

   procedure main is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
   end;

--  BEGIN Object file/option list
   --   C:\Users\Quetzalcoatl\Documents\ADA micro\Ada_Drivers_Library-master\arch\ARM\STM32\driver_demos\battery_charger\obj\Debug\last_chance_handler.o
   --   C:\Users\Quetzalcoatl\Documents\ADA micro\Ada_Drivers_Library-master\arch\ARM\STM32\driver_demos\battery_charger\obj\Debug\battery_charger.o
   --   -LC:\Users\Quetzalcoatl\Documents\ADA micro\Ada_Drivers_Library-master\arch\ARM\STM32\driver_demos\battery_charger\obj\Debug\
   --   -LC:\Users\Quetzalcoatl\Documents\ADA micro\Ada_Drivers_Library-master\arch\ARM\STM32\driver_demos\battery_charger\obj\Debug\
   --   -LC:\Users\Quetzalcoatl\Documents\ADA micro\Ada_Drivers_Library-master\examples\shared\common\
   --   -LC:\Users\Quetzalcoatl\Documents\ADA micro\Ada_Drivers_Library-master\boards\stm32f407_discovery\obj\full_lib_Debug\
   --   -LC:\gnat\2017\arm-eabi\lib\gnat\ravenscar-full-stm32f4\adalib\
--  END Object file/option list   

end ada_main;
