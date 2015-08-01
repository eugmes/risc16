library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library RiSC16;
use RiSC16.risc16_pkg.all;
use RiSC16.risc16_comp.all;

entity cpu is
  port (clk : in std_logic;
        reset : in std_logic;
        i : in cpu_in_t;
        o : out cpu_out_t);
end cpu;

architecture struct of cpu is
  signal datapath_o : datapath_out_t;
  signal datapath_i : datapath_in_t;
begin
  datapath_inst : datapath
    port map (clk => clk,
              reset => reset,
              i => datapath_i,
              o => datapath_o);

  regfile_inst : regfile
    port map (clk => clk,
              i => datapath_o.regfile,
              o => datapath_i.regfile);

  alu_inst : alu
    port map (i => datapath_o.alu,
              o => datapath_i.alu);
  
  o.code_mem <= datapath_o.code_mem;
  o.data_mem <= datapath_o.data_mem;
            
  datapath_i.code_mem <= i.code_mem;
  datapath_i.data_mem <= i.data_mem;
end struct;
