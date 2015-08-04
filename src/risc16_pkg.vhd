library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package risc16_pkg is
  constant WORD_WIDTH : natural := 16;

  subtype word_t is std_logic_vector (WORD_WIDTH - 1 downto 0);
  subtype sword_t is signed (WORD_WIDTH - 1 downto 0);
  subtype uword_t is unsigned (WORD_WIDTH - 1 downto 0);
  
  constant ZERO_WORD : word_t := (others => '0');
  constant INITIAL_PC : word_t := ZERO_WORD;
  constant NOP_COMMAND : word_t := ZERO_WORD;

  constant REG_COUNT : natural := 8;
  constant REG_ADDR_WIDTH : natural := 3;
  
  subtype reg_num_t is natural range 0 to REG_COUNT - 1;
  subtype reg_addr_t is unsigned (REG_ADDR_WIDTH - 1 downto 0);
  
  constant ZERO_REG : reg_addr_t := (others => '0');
  
  type regfile_in_t is record
    r_addr1 : reg_addr_t;
    r_addr2 : reg_addr_t;
    w_addr : reg_addr_t;
    we : std_logic;
    w_data : word_t;
  end record;
  
  type regfile_out_t is record
    r_data1 : word_t;
    r_data2 : word_t;
  end record;

  constant OPCODE_WIDTH : natural := 3;
  subtype opcode_t is std_logic_vector (OPCODE_WIDTH - 1 downto 0);
  
  constant OPCODE_ADD  : opcode_t := "000";
  constant OPCODE_ADDI : opcode_t := "001";
  constant OPCODE_NAND : opcode_t := "010";
  constant OPCODE_LUI  : opcode_t := "011";
  constant OPCODE_SW   : opcode_t := "100";
  constant OPCODE_LW   : opcode_t := "101";
  constant OPCODE_BEQ  : opcode_t := "110";
  constant OPCODE_JALR : opcode_t := "111";
  
  subtype imm7_t is signed (6 downto 0);
  subtype imm10_t is signed (9 downto 0);
  
  type alu_control_t is (ALU_PASS1, ALU_ADD, ALU_NAND, ALU_EQ);
  
  type alu_out_t is record
    data : word_t;
  end record;
  
  type alu_in_t is record
    control : alu_control_t;
    data1 : word_t;
    data2 : word_t;
  end record;
  
  type code_mem_in_t is record
    addr : word_t;
  end record;

  type code_mem_out_t is record
    command : word_t;
  end record;
 
  type data_mem_in_t is record
    addr : word_t;
    we : std_logic;
    w_data : word_t;
  end record;

  type data_mem_out_t is record
    r_data : word_t;
  end record;

  type datapath_in_t is record
    code_mem : code_mem_out_t;
    data_mem : data_mem_out_t;
    alu : alu_out_t;
    regfile: regfile_out_t;
  end record;
  
  type datapath_out_t is record
    code_mem : code_mem_in_t;
    data_mem : data_mem_in_t;
    alu : alu_in_t;
    regfile : regfile_in_t;
  end record;
  
  type cpu_in_t is record
    code_mem : code_mem_out_t;
    data_mem : data_mem_out_t;
  end record;
  
  type cpu_out_t is record
    code_mem : code_mem_in_t;
    data_mem : data_mem_in_t;
  end record;
  
  type word_vector is array (natural range <>) of word_t;

  type cmd_fields_t is record
    opcode : opcode_t;
    reg_a : reg_addr_t;
    reg_b : reg_addr_t;
    reg_c : reg_addr_t;
    imm10 : word_t;
    imm7 : word_t;
  end record;

  function parse_cmd (cmd : in word_t) return cmd_fields_t;
end risc16_pkg;

package body risc16_pkg is
  function parse_cmd (cmd : in word_t) return cmd_fields_t is
  begin
    return (opcode => cmd (15 downto 13),
            reg_a => reg_addr_t (cmd (12 downto 10)),
            reg_b => reg_addr_t (cmd (9 downto 7)),
            reg_c => reg_addr_t (cmd (2 downto 0)),
            imm10 => cmd (9 downto 0) & "000000",
            imm7 => word_t (resize (signed (cmd (6 downto 0)), WORD_WIDTH)));
  end parse_cmd;
end risc16_pkg;
