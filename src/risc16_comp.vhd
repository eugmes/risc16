library IEEE;
use IEEE.std_logic_1164.all;

library RiSC16;
use RiSC16.risc16_pkg.all;

package risc16_comp is
  component alu is
    port (i : in alu_in_t;
          o : out alu_out_t);
  end component;

  component regfile is
    port (clk : in std_logic;
          i : in regfile_in_t;
          o : out regfile_out_t);
  end component;

  component datapath is
    port (clk : in std_logic;
          reset : in std_logic;
          i : in datapath_in_t;
          o : out datapath_out_t);
  end component;
  
  component cpu is
    port (clk : in std_logic;
          reset : in std_logic;
          i : in cpu_in_t;
          o : out cpu_out_t);
  end component;
  
  component rom is
    generic (ROM_SIZE : positive := 128;
             INIT : word_vector);
    port (i : in code_mem_in_t;
          o : out code_mem_out_t);
  end component;

  component ram is
    generic (RAM_SIZE : positive := 128);
    port (clk : in std_logic;
         i : in data_mem_in_t;
         o : out data_mem_out_t);
  end component;
end risc16_comp;
