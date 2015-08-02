library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_unsigned.all;

library RiSC16;
use RiSC16.risc16_pkg.all;

entity dual_port_ram is
  generic (RAM_SIZE : positive := 128;
           INIT : word_vector);
  port (clk : in std_logic;
        i_data : in data_mem_in_t;
        i_code : in code_mem_in_t;
        o_data : out data_mem_out_t;
        o_code : out code_mem_out_t);
end dual_port_ram;

architecture mixed of dual_port_ram is
  signal content : word_vector (0 to RAM_SIZE - 1) := INIT;
begin
  o_data.r_data <= content (conv_integer (i_data.addr)) after 1 ns;
  o_code.command <= content (conv_integer (i_code.addr)) after 1 ns;

  write_proc : process (clk) is
  begin
    if rising_edge (clk) then
      if i_data.we = '1' then
        content (conv_integer (i_data.addr)) <= i_data.w_data after 1 ns;
      end if;
    end if;
  end process;
end mixed;
