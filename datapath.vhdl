LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE IEEE.NUMERIC_STD.ALL;
ENTITY datapath IS

    PORT (
        clk, rst, ES_tri : IN STD_LOGIC;
        adr_gen_mux1_sel : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        address_out : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
        mem_data_in : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
        queue_out_to_ctrl : OUT STD_LOGIC_VECTOR(47 DOWNTO 0);
        inst_reg_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        inst_reg_en : IN STD_LOGIC;
        pop_from_queue, alu_temp_reg1_en, alu_temp_reg2_en : IN STD_LOGIC;
        alu_op_sel : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        ALU_tri_en : IN STD_LOGIC;
        ax_en, ax_en_l, ax_en_h, ax_tri_en : IN STD_LOGIC;
        bx_en, bx_en_l, bx_en_h, bx_tri_en : IN STD_LOGIC;
        cx_en, cx_en_l, cx_en_h, cx_tri_en : IN STD_LOGIC;
        dx_en, dx_en_l, dx_en_h, dx_tri_en : IN STD_LOGIC;
        sp_en, sp_tri_en : IN STD_LOGIC;
        bp_en, bp_tri_en : IN STD_LOGIC;
        si_en, si_tri_en : IN STD_LOGIC;
        di_en, di_tri_en : IN STD_LOGIC;
        data_out : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
        disable_inst_fetch : IN STD_LOGIC;
        number_of_pop : IN INTEGER;
        adr_gen_mux2_sel : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        memory_bus_tri : IN STD_LOGIC;
        queue_empty : OUT STD_LOGIC;
        queue_to_bus_tri : IN STD_LOGIC;
        ip_mux_sel : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        flag_reg_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        flag_reg_en : IN STD_LOGIC;
        update_IP_loop : IN STD_LOGIC);

END ENTITY datapath;

ARCHITECTURE behavioral OF datapath IS

    SIGNAL ES_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
    SIGNAL CS_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
    SIGNAL SS_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
    SIGNAL DS_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
    SIGNAL IP_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
    SIGNAL segment_input : STD_LOGIC_VECTOR (15 DOWNTO 0);
    SIGNAL inc_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
    SIGNAL IP_en : STD_LOGIC;
    SIGNAL adr_gen_mux1_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
    SIGNAL adr_gen_mux2_out : STD_LOGIC_VECTOR (15 DOWNTO 0);
    SIGNAL queue_full : STD_LOGIC;
    SIGNAL queue_out : STD_LOGIC_VECTOR(47 DOWNTO 0);
    SIGNAL data_bus_16 : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL push_queue : STD_LOGIC;
    SIGNAL alu_temp_reg1_out : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL alu_temp_reg2_out : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL alu_out : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL ip_mux_out : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL ip_queue_temp : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL alu_carry_out : STD_LOGIC;
    SIGNAL alu_zero : STD_LOGIC;
    SIGNAL ax_out, bx_out, cx_out, dx_out, sp_out, bp_out, si_out, di_out : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL alu_flag_out : STD_LOGIC_VECTOR(7 DOWNTO 0);
BEGIN

    ES : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, '0', segment_input, ES_out);
    CS : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, '0', segment_input, CS_out);
    SS : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, '0', segment_input, SS_out);
    DS : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, '0', segment_input, DS_out);

    IP : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, IP_en, ip_mux_out, IP_out);

    ip_queue_temp <= "00000000" & queue_out(7 DOWNTO 0);

    IP_MUX : ENTITY work.mux(behavioral)
        PORT MAP(inc_out, ip_queue_temp, di_out, si_out, ip_mux_sel, ip_mux_out);

    INC : ENTITY work.incrementor(behavioral)
        GENERIC MAP(16)
        PORT MAP(IP_out, inc_out);

    IP_en <= (NOT(queue_full OR disable_inst_fetch)) or update_IP_loop;

    adr_gen_mux1 : ENTITY work.mux(behavioral)
        GENERIC MAP(16)
        PORT MAP(CS_out, ES_out, SS_out, DS_out, adr_gen_mux1_sel, adr_gen_mux1_out);

    adr_gen_mux2 : ENTITY work.mux(behavioral)
        GENERIC MAP(16)
        PORT MAP(IP_out, di_out, Si_out, dx_out, adr_gen_mux2_sel, adr_gen_mux2_out);
    address_generator : ENTITY work.address_calculator(behavioral)
        PORT MAP(adr_gen_mux1_out, adr_gen_mux2_out, address_out);

    push_queue <= NOT disable_inst_fetch;

    fifo_queue : ENTITY work.queue(behavioral)
        PORT MAP(clk, rst, push_queue, pop_from_queue, mem_data_in, queue_full, queue_empty, queue_out, number_of_pop);

    queue_out_to_ctrl <= queue_out;

    inst_reg : ENTITY work.reg(behavioral)
        GENERIC MAP(8)
        PORT MAP(clk, rst, inst_reg_en, queue_out(7 DOWNTO 0), inst_reg_out);
    alu_temp_reg1 : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, alu_temp_reg1_en, data_bus_16, alu_temp_reg1_out);
    alu_temp_reg2 : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, alu_temp_reg2_en, data_bus_16, alu_temp_reg2_out);

    Arith_logic_unit : ENTITY work.alu(behavioral)
        PORT MAP(alu_temp_reg1_out, alu_temp_reg2_out, alu_op_sel, alu_out, alu_flag_out);

    flag_reg : ENTITY work.reg(behavioral)
        GENERIC MAP(8)
        PORT MAP(clk, rst, flag_reg_en, alu_flag_out, flag_reg_out);

    ALU_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(alu_out, ALU_tri_en, data_bus_16);

    AX : ENTITY work.x_registers(behavioral)
        PORT MAP(clk, rst, ax_en, ax_en_l, ax_en_h, data_bus_16, ax_out);

    AX_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(ax_out, ax_tri_en, data_bus_16);

    BX : ENTITY work.x_registers(behavioral)
        PORT MAP(clk, rst, bx_en, bx_en_l, bx_en_h, data_bus_16, bx_out);

    BX_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(bx_out, bx_tri_en, data_bus_16);

    CX : ENTITY work.x_registers(behavioral)
        PORT MAP(clk, rst, cx_en, cx_en_l, cx_en_h, data_bus_16, cx_out);

    CX_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(cx_out, cx_tri_en, data_bus_16);

    DX : ENTITY work.x_registers(behavioral)
        PORT MAP(clk, rst, dx_en, dx_en_l, dx_en_h, data_bus_16, dx_out);

    DX_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(dx_out, dx_tri_en, data_bus_16);

    SP : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, sp_en, data_bus_16, sp_out);

    SP_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(sp_out, sp_tri_en, data_bus_16);
    BP : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, bp_en, data_bus_16, bp_out);

    BP_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(bp_out, bp_tri_en, data_bus_16);

    SI : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, si_en, data_bus_16, si_out);

    SI_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(si_out, si_tri_en, data_bus_16);

    DI : ENTITY work.reg(behavioral)
        GENERIC MAP(16)
        PORT MAP(clk, rst, di_en, data_bus_16, di_out);

    DI_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(di_out, di_tri_en, data_bus_16);

    memory_to_bus_tri : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(mem_data_in, memory_bus_tri, data_bus_16);

    queue_to_bus_tristate : ENTITY work.TriStateBuffer(behavioral)
        PORT MAP(queue_out(15 DOWNTO 0), queue_to_bus_tri, data_bus_16);

    data_out <= data_bus_16;
END behavioral; -- bwhavioralsab