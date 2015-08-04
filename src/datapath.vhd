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

architecture pipelined of datapath is
  type if_id_reg_t is record
    command : word_t;
    pc : word_t;
  end record;

  constant IF_ID_NOP : if_id_reg_t := (command => NOP_COMMAND, pc => ZERO_WORD);

  type id_ex_reg_t is record
    opcode : opcode_t;
    tgt_reg : reg_addr_t;
    src_reg1, src_reg2 : reg_addr_t;
    pc : word_t;
    operand0, operand1, operand2 : word_t;
  end record;

  constant ID_EX_NOP : id_ex_reg_t :=
      (opcode => OPCODE_ADD,
       tgt_reg => ZERO_REG,
       src_reg1 => ZERO_REG,
       src_reg2 => ZERO_REG,
       pc => ZERO_WORD,
       operand0 => ZERO_WORD,
       operand1 => ZERO_WORD,
       operand2 => ZERO_WORD);

  type ex_mem_reg_t is record
    opcode : opcode_t;
    tgt_reg : reg_addr_t;
    store_data : word_t;
    alu_output : word_t;
  end record;

  type mem_wb_reg_t is record
    tgt_reg : reg_addr_t;
    rf_write_data : word_t;
  end record;

  type reg_t is record
    pc : word_t;
    if_id : if_id_reg_t;
    id_ex : id_ex_reg_t;
    ex_mem : ex_mem_reg_t;
    mem_wb : mem_wb_reg_t;
  end record;

  constant RESET_REG : reg_t :=
    (pc => INITIAL_PC,
     if_id => IF_ID_NOP,
     id_ex => ID_EX_NOP,
     ex_mem => (opcode => OPCODE_ADD,
                tgt_reg => ZERO_REG,
                store_data => ZERO_WORD,
                alu_output => ZERO_WORD),
     mem_wb => (tgt_reg => ZERO_REG,
                rf_write_data => ZERO_WORD));

  signal r : reg_t := RESET_REG;
  signal rin : reg_t;
begin
  comb : process (i, r) is
    type alu_src2_t is (alu_src2_reg, alu_src2_pc1, alu_src2_op0);
    type alu_reg_stage_t is (alu_reg_id, alu_reg_ex, alu_reg_mem);
    type next_pc_src_t is (PC_INC, PC_BEQ, PC_JALR);

    procedure set_next_pc_and_stomp (opcode : in opcode_t; eq : in std_logic;
                                      next_pc_src : out next_pc_src_t;
                                      stomp : out boolean) is
    begin
      if opcode = OPCODE_BEQ and eq = '1' then
        stomp := True;
        next_pc_src := PC_BEQ;
      elsif opcode = OPCODE_JALR then
        stomp := True;
        next_pc_src := PC_JALR;
      else
        stomp := False;
        next_pc_src := PC_INC;
      end if;
    end set_next_pc_and_stomp;

    function calc_alu_control (opcode : in opcode_t) return alu_control_t is
    begin
      case opcode is
        when OPCODE_ADD => return ALU_ADD;
        when OPCODE_ADDI => return ALU_ADD;
        when OPCODE_NAND => return ALU_NAND;
        when OPCODE_SW => return ALU_ADD;
        when OPCODE_LW => return ALU_ADD;
        when OPCODE_BEQ => return ALU_EQ;
        when OPCODE_JALR => return ALU_PASS1;
        when others => return ALU_PASS1;
      end case;
    end calc_alu_control;

    function choose_reg_stage (reg_addr, ex_addr, mem_addr : in reg_addr_t) return alu_reg_stage_t is
    begin
      if reg_addr = ZERO_REG then
        return alu_reg_id;
      elsif reg_addr = ex_addr then
        return alu_reg_ex;
      elsif reg_addr = mem_addr then
        return alu_reg_mem;
      end if;

      return alu_reg_id;
    end choose_reg_stage;

    function choose_src2 (opcode : in opcode_t) return alu_src2_t is
    begin
      case opcode is
        when OPCODE_JALR => return alu_src2_pc1;
        when OPCODE_ADDI | OPCODE_LUI =>
          return alu_src2_op0;
        when others => return alu_src2_reg;
      end case;
    end choose_src2;

    procedure set_opcode_tgt_stall (cmd_fields : in cmd_fields_t;
                                    next_opcode : in opcode_t;
                                    next_tgt : in reg_addr_t;
                                    opcode : out opcode_t; tgt : out reg_addr_t;
                                    stall : out boolean) is
      variable need_stall : boolean;
    begin
      need_stall := false;

      if next_opcode = OPCODE_LW and next_tgt /= ZERO_REG then
        case cmd_fields.opcode is
          when OPCODE_ADD | OPCODE_NAND =>
            need_stall := cmd_fields.reg_b = next_tgt
                       or cmd_fields.reg_c = next_tgt;
          when OPCODE_ADDI | OPCODE_LW | OPCODE_JALR =>
            need_stall := cmd_fields.reg_b = next_tgt;
          when OPCODE_SW | OPCODE_BEQ =>
            need_stall := cmd_fields.reg_a = next_tgt
                       or cmd_fields.reg_b = next_tgt;
          when others => null;
        end case;
      end if;

      if need_stall then
        opcode := OPCODE_ADD;
        tgt := ZERO_REG;
        stall := true;
      else
        opcode := cmd_fields.opcode;
        case cmd_fields.opcode is
          when OPCODE_SW | OPCODE_BEQ =>
            tgt := ZERO_REG;
          when others =>
            tgt := cmd_fields.reg_a;
        end case;
        stall := false;
      end if;
    end set_opcode_tgt_stall;

    variable cmd_fields : cmd_fields_t;
    variable data_we : std_logic;
    variable alu_control : alu_control_t;
    variable alu_src1, alu_src2, alu_src2_tmp : word_t;
    variable src_reg2 : reg_addr_t;
    variable reg_we : std_logic;

    variable mem_write_data : word_t;

    variable stall, stomp : boolean;
    variable next_pc_src : next_pc_src_t;
    variable if_pc_p1, ex_pc_p1, ex_pc_res : word_t;
    variable id_opcode : opcode_t;
    variable id_tgt_reg : reg_addr_t;
    variable operand0 : word_t;

    variable v : reg_t;
  begin
    v := r;

    -- WRITEBACK stage
    reg_we := '1' when r.mem_wb.tgt_reg /= ZERO_REG else '0';

    -- MEMORY stage
    data_we := '1' when r.ex_mem.opcode = OPCODE_SW else '0';
    mem_write_data := i.data_mem.r_data when r.ex_mem.opcode = OPCODE_LW else
                      r.ex_mem.alu_output;
    v.mem_wb := (tgt_reg => r.ex_mem.tgt_reg,
                 rf_write_data => mem_write_data);

    -- EXECUTE stage
    set_next_pc_and_stomp (r.id_ex.opcode, i.alu.data (0), next_pc_src, stomp);
    alu_control := calc_alu_control (r.id_ex.opcode);

    mux_alu1: case choose_reg_stage (r.id_ex.src_reg1, r.ex_mem.tgt_reg, r.mem_wb.tgt_reg) is
      when alu_reg_id => alu_src1 := r.id_ex.operand1;
      when alu_reg_ex => alu_src1 := r.ex_mem.alu_output;
      when alu_reg_mem => alu_src1 := r.mem_wb.rf_write_data;
    end case;

    mux_alu2: case choose_reg_stage (r.id_ex.src_reg2, r.ex_mem.tgt_reg, r.mem_wb.tgt_reg) is
      when alu_reg_id => alu_src2_tmp := r.id_ex.operand2;
      when alu_reg_ex => alu_src2_tmp := r.ex_mem.alu_output;
      when alu_reg_mem => alu_src2_tmp := r.mem_wb.rf_write_data;
    end case;

    ex_pc_p1 := word_t (uword_t (r.id_ex.pc) + to_unsigned (1, WORD_WIDTH));
    ex_pc_res := word_t (sword_t (ex_pc_p1) + sword_t (r.id_ex.operand0));

    mux_imm: case choose_src2 (r.id_ex.opcode) is
      when alu_src2_reg => alu_src2 := alu_src2_tmp;
      when alu_src2_pc1 => alu_src2 := ex_pc_p1;
      when alu_src2_op0 => alu_src2 := r.id_ex.operand0;
    end case;

    v.ex_mem := (opcode => r.id_ex.opcode,
                 tgt_reg => r.id_ex.tgt_reg,
                 store_data => alu_src2_tmp,
                 alu_output => i.alu.data);

    -- DECODE stage
    cmd_fields := parse_cmd (r.if_id.command);
    set_opcode_tgt_stall (cmd_fields, r.id_ex.opcode, r.id_ex.tgt_reg, id_opcode, id_tgt_reg, stall);

    operand0 := cmd_fields.imm10 when id_opcode = OPCODE_LUI else cmd_fields.imm7;
    src_reg2 := cmd_fields.reg_a when id_opcode = OPCODE_BEQ else cmd_fields.reg_c;

    if stomp then
      v.id_ex := ID_EX_NOP;
    else
      v.id_ex := (opcode => id_opcode,
                  tgt_reg => id_tgt_reg,
                  src_reg1 => cmd_fields.reg_b,
                  src_reg2 => src_reg2,
                  pc => r.if_id.pc,
                  operand0 => operand0,
                  operand1 => i.regfile.r_data1,
                  operand2 => i.regfile.r_data2);
    end if;

    -- FETCH stage
    if_pc_p1 := word_t (uword_t (r.pc) + to_unsigned (1, WORD_WIDTH));

    if stomp then
      v.if_id := IF_ID_NOP;
    elsif stall then
      null;
    else
      v.if_id := (command => i.code_mem.command,
                  pc => r.pc);
    end if;

    -- Update PC
    if stall then
      null;
    else
      case next_pc_src is
        when PC_INC => v.pc := if_pc_p1;
        when PC_BEQ => v.pc := ex_pc_res;
        when PC_JALR => v.pc := alu_src1;
      end case;
    end if;

    rin <= v after 1 ns;

    o.alu <= (control => alu_control,
              data1 => alu_src2,
              data2 => alu_src1) after 1 ns;

    o.regfile <= (r_addr1 => cmd_fields.reg_b,
                  r_addr2 => src_reg2,
                  w_addr => r.mem_wb.tgt_reg,
                  we => reg_we,
                  w_data => r.mem_wb.rf_write_data) after 1 ns;

    o.code_mem <= (addr => r.pc) after 1 ns;
    o.data_mem <= (addr => r.ex_mem.alu_output,
                   we => data_we,
                   w_data => r.ex_mem.store_data) after 1 ns;
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
end pipelined;
