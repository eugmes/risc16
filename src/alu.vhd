library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library RiSC16;
use RiSC16.risc16_pkg.all;

entity alu is
  port (i : in alu_in_t;
        o : out alu_out_t);
end alu;

architecture mixed of alu is
  function eq_test (a, b : in word_t) return word_t is
  begin
    if a = b then
      return (others => '1');
    else
      return (others => '0');
    end if;
  end eq_test;
begin
  with i.control select
    o.data <= i.data1 when ALU_PASS1,
              word_t (sword_t (i.data1) + sword_t (i.data2)) when ALU_ADD,
              i.data1 nand i.data2 when ALU_NAND,
              eq_test (i.data1, i.data2) when ALU_EQ;
end mixed;
