library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library RiSC16;
use RiSC16.risc16_pkg.all;
use RiSC16.risc16_comp.all;

entity cpu_test is
end cpu_test;

architecture behav of cpu_test is
  signal clk : std_logic;
  signal reset : std_logic := '1';
  
  signal cpu_i : cpu_in_t;
  signal cpu_o : cpu_out_t;
  
  signal data_mem_i : data_mem_in_t;
  signal data_mem_o : data_mem_out_t;
  
  signal code_mem_i : code_mem_in_t;
  signal code_mem_o : code_mem_out_t;
  
  constant CLK_PERIOD : time := 100 ns;
  
  type ram_t is array (natural range <>) of word_t;
  
  constant code_mem : ram_t (0 to 16) :=
--    ("0010010001111111",  -- addi r1, r0, -1
--     "1000010000000000",  -- sw r1, r0, 0
--     "1010100000000000",  -- lw r2, r0, 0
--     "1100000001111111",  -- beq r0, r0, -1
    ("0000010000000000",  -- add r1, r0, r0
     "0010100000010000",  -- addi r2, r0, 16
     "0100110000000000",  -- nand r3, r0, r0
     "1100010100000011",  -- beq r1, r2, 3
     "1000110010000000",  -- sw r3, r1, 0
     "0010010010000001",  -- addi r1, r1, 1
     "1100000001111100",  -- beq r0, r0, -4
     "1100000001111111",  -- beq r0, r0, -1
     others => ZERO_WORD);     
  signal data_mem : ram_t (0 to 16) := (others => ZERO_WORD);
begin
  dut : cpu
    port map (clk => clk,
              reset => reset,
              i => cpu_i,
              o => cpu_o);

  data_mem_i <= cpu_o.data_mem;
  code_mem_i <= cpu_o.code_mem;
  
  cpu_i.data_mem <= data_mem_o;
  cpu_i.code_mem <= code_mem_o;

  code_mem_proc : process (code_mem_i)
    variable addr : natural;
  begin
    addr := to_integer (uword_t (code_mem_i.addr));
    
    if addr <= code_mem'High then
      code_mem_o.command <= code_mem (addr) after 1 ns;
    else
      code_mem_o.command <= (others => '0') after 1 ns;
    end if;
  end process;

  data_mem_proc : process (clk)
    variable addr : natural;
  begin
    if rising_edge (clk) then
      addr := to_integer (uword_t (data_mem_i.addr));
      if data_mem_i.we = '1' and addr <= data_mem'High then
        data_mem (addr) <= data_mem_i.w_data after 1 ns;
      end if;
    end if;
  end process;

  process (data_mem_i, data_mem)
    variable addr : natural;
  begin
    addr := to_integer (uword_t (data_mem_i.addr));
 
    if addr <= data_mem'High then 
      data_mem_o.r_data <= data_mem (addr) after 1 ns;
    else
      data_mem_o.r_data <= ZERO_WORD after 1 ns;
    end if;
  end process;

  clock_gen : process
  begin
    clk <= '0';
    wait for CLK_PERIOD / 2;
    clk <= '1';
    wait for CLK_PERIOD / 2;
  end process;
  
  stim_proc : process
  begin
    reset <= '1';
    wait for CLK_PERIOD * 3;
    reset <= '0';
    wait;
  end process;
end behav;