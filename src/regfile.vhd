library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library RiSC16;
use RiSC16.risc16_pkg.all;

entity regfile is
  port (clk : in std_logic;
        i : in regfile_in_t;
        o : out regfile_out_t);
end regfile;

architecture mixed of regfile is
  type regs_data_t is array (reg_num_t) of word_t;
  
  signal regs : regs_data_t;
  signal data1_tmp, data2_tmp : word_t;
begin
  data1_tmp <= regs (to_integer (i.r_addr1));
  data2_tmp <= regs (to_integer (i.r_addr2));

  o.r_data1 <= ZERO_WORD when i.r_addr1 = ZERO_REG else data1_tmp after 1 ns;
  o.r_data2 <= ZERO_WORD when i.r_addr2 = ZERO_REG else data2_tmp after 1 ns;
  
  process (clk) is
  begin
    if rising_edge (clk) then
      if i.we = '1' then
        regs (to_integer (i.w_addr)) <= i.w_data after 1 ns;
      end if;
    end if;
  end process;
end mixed;
