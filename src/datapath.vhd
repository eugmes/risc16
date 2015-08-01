library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.ALL;

library RiSC16;
use RiSC16.risc16_pkg.all;

entity datapath is
  port (clk : in std_logic;
        reset : in std_logic;
        i : in datapath_in_t;
        o : out datapath_out_t);
end datapath;

architecture sequential of datapath is
  type cmd_fields_t is record
    opcode : opcode_t;
    reg_a : reg_addr_t;
    reg_b : reg_addr_t;
    reg_c : reg_addr_t;
    imm10 : word_t;
    imm7 : word_t;
  end record;
  
  function parse_cmd (cmd : in word_t) return cmd_fields_t is
  begin
    return (opcode => cmd (15 downto 13),
            reg_a => reg_addr_t (cmd (12 downto 10)),
            reg_b => reg_addr_t (cmd (9 downto 7)),
            reg_c => reg_addr_t (cmd (2 downto 0)),
            imm10 => cmd (9 downto 0) & "000000",
            imm7 => word_t (resize (signed (cmd (6 downto 0)), WORD_WIDTH)));
  end parse_cmd;

  type reg_t is record
    pc : word_t;
  end record;
  
  constant RESET_REG : reg_t := (pc => INITIAL_PC);

  signal r : reg_t := RESET_REG;
  signal rin : reg_t;
begin
  comb : process (i, r) is
    variable v : reg_t;
    
    variable cmd_fields : cmd_fields_t;
    
    variable alu_control : alu_control_t;
    variable alu_src1, alu_src2 : word_t;
    variable reg_addr1, reg_addr2 : reg_addr_t;
    variable data_we : std_logic;
    variable reg_w_addr : reg_addr_t;
    variable reg_w_data : word_t;
    variable reg_we : std_logic;
  begin
    v := r;
    
    v.pc := word_t (uword_t (v.pc) + to_unsigned (1, WORD_WIDTH));
    
    cmd_fields := parse_cmd (i.code_mem.command);

    alu_control := ALU_PASS1;
    alu_src1 := i.regfile.r_data1;
    alu_src2 := i.regfile.r_data2;

    reg_we := '1';
    reg_w_addr := cmd_fields.reg_a;
    reg_w_data := i.alu.data;
    
    reg_addr1 := cmd_fields.reg_b;
    reg_addr2 := cmd_fields.reg_c;
 
    data_we := '0';
 
    case cmd_fields.opcode is
      when OPCODE_ADD =>
        alu_control := ALU_ADD;
      when OPCODE_ADDI =>
        alu_control := ALU_ADD;
        alu_src2 := cmd_fields.imm7;
      when OPCODE_NAND =>
        alu_control := ALU_NAND;
      when OPCODE_LUI =>
        alu_src1 := cmd_fields.imm10;
      when OPCODE_SW =>
        alu_control := ALU_ADD;
        data_we := '1';
        reg_we := '0';
        alu_src2 := cmd_fields.imm7;
        reg_addr2 := cmd_fields.reg_a;
      when OPCODE_LW =>
        alu_control := ALU_ADD;
        alu_src2 := cmd_fields.imm7;
        reg_w_data := i.data_mem.r_data;
        reg_addr2 := cmd_fields.reg_a;
      when OPCODE_BEQ =>
        reg_we := '0';
        alu_control := ALU_EQ;
        reg_addr2 := cmd_fields.reg_a;
        
        if i.alu.data (0) = '1' then
          v.pc := word_t (sword_t (v.pc) + sword_t (cmd_fields.imm7));
        end if;
      when OPCODE_JALR =>
        reg_w_data := v.pc;
        v.pc := i.alu.data;
      when others => null;
    end case;

    rin <= v;

    o.alu <= (control => alu_control,
              data1 => alu_src1,
              data2 => alu_src2) after 1 ns;

    o.regfile <= (r_addr1 => reg_addr1,
                  r_addr2 => reg_addr2,
                  w_addr => reg_w_addr,
                  we => reg_we,
                  w_data => reg_w_data) after 1 ns;
 
    o.code_mem <= (addr => r.pc) after 1 ns; 
    o.data_mem <= (addr => i.alu.data,
                   we => data_we,
                   w_data => i.regfile.r_data2) after 1 ns;
  end process;
  
  regs : process (clk) is
  begin
    if rising_edge (clk) then
      if reset = '1' then
        r <= RESET_REG after 1 ns;
      else
        r <= rin after 1 ns;
      end if;
    end if;
  end process;
end sequential;
