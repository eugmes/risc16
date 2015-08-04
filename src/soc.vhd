library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library RiSC16;
use RiSC16.risc16_pkg.all;
use RiSC16.risc16_comp.all;

entity soc is
  port (CLK_12MHz : in std_logic;
        LED : out std_logic_vector (7 downto 0));
end soc;

architecture mixed of soc is
  signal reset_cnt : unsigned (1 downto 0) := (others => '0');
  signal clk : std_logic;
  signal reset : std_logic := '1';

  signal cpu_i : cpu_in_t;
  signal cpu_o : cpu_out_t;
  
  signal data_mem_i : data_mem_in_t;
  signal data_mem_o : data_mem_out_t;
  
  signal code_mem_i : code_mem_in_t;
  signal code_mem_o : code_mem_out_t;
begin
  clk <= CLK_12MHz;

  reset_proc : process (clk) is
  begin
    if rising_edge (clk) then
      if reset_cnt /= "11" then
        reset_cnt <= reset_cnt + 1;
        reset <= '1';
      else
        reset <= '0';
      end if;
    end if;
  end process;

  cpu_inst : cpu
    port map (clk => clk,
              reset => reset,
              i => cpu_i,
              o => cpu_o);

  ram_inst : dual_port_ram
    generic map (
      RAM_SIZE => 128,
      INIT => 
        (0 => "0000010000000000",  -- add r1, r0, r0
         1 => "0100110000000000",  -- nand r3, r0, r0 (r3 = 0xffff)
         2 => "1000010110000000",  -- sw r1, r3, 0
         3 => "0010010010000010",  -- addi r1, r1, 2
         4 => "1100000001111101",  -- beq r0, r0, -3
         5 to 127 => ZERO_WORD))
    port map (clk => clk,
              i_data => data_mem_i,
              i_code => code_mem_i,
              o_data => data_mem_o,
              o_code => code_mem_o);
              
  data_mem_i <= cpu_o.data_mem;
  code_mem_i <= cpu_o.code_mem;
  
  cpu_i.data_mem <= data_mem_o;
  cpu_i.code_mem <= code_mem_o;

  write_led : process (clk) is
  begin
    if rising_edge (clk) then
      if reset = '1' then
        LED <= (others => '1');
      else
        if data_mem_i.addr = x"ffff" and data_mem_i.we = '1' then
          LED <= data_mem_i.w_data (7 downto 0);
        end if;
      end if;
    end if;
  end process;
end mixed;
