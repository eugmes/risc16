library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library RiSC16;
use RiSC16.risc16_pkg.all;

entity rom is
  generic (ROM_SIZE : positive := 128;
           INIT : word_vector);
  port (i : in code_mem_in_t;
        o : out code_mem_out_t);
end rom;

architecture mixed of rom is
  type reg_t is record
    content : word_vector (0 to ROM_SIZE - 1);
  end record;
  
  constant r : reg_t := (content => INIT);
begin
  o.command <= r.content (to_integer (uword_t (i.addr))) after 1 ns;
end mixed;
