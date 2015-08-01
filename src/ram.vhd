library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_unsigned.all;

library RiSC16;
use RiSC16.risc16_pkg.all;

entity ram is
  generic (RAM_SIZE : positive := 128);
  port (clk : in std_logic;
        i : in data_mem_in_t;
        o : out data_mem_out_t);
end ram;

architecture mixed of ram is
  signal content : word_vector (0 to RAM_SIZE - 1);
begin
  o.r_data <= content (conv_integer (i.addr)) after 1 ns;

  write_proc : process (clk) is
  begin
    if rising_edge (clk) then
      if i.we = '1' then
        content (conv_integer (i.addr)) <= i.w_data after 1 ns;
      end if;
    end if;
  end process;
end mixed;
