library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


entity datapath is 

    port (clk, rst, ES_tri : in std_logic; adr_gen_mux1_sel : in std_logic_vector(1 downto 0); 
         address_out : out std_logic_vector(15 downto 0);
         mem_data_in : in std_logic_vector(15 downto 0);
         queue_out_to_ctrl : out std_logic_vector(7 downto 0);
         inst_reg_out : out std_logic_vector(7 downto 0);
         inst_reg_en : in std_logic;
         pop_from_queue, alu_temp_reg1_en, alu_temp_reg2_en : in std_logic;
         alu_op_sel : in std_logic_vector(3 downto 0);
         ALU_tri_en : in std_logic;
         ax_en, ax_en_l, ax_en_h, ax_tri_en : in std_logic;
         bx_en, bx_en_l, bx_en_h, bx_tri_en : in std_logic;
         cx_en, cx_en_l, cx_en_h, cx_tri_en : in std_logic;
         dx_en, dx_en_l, dx_en_h, dx_tri_en : in std_logic;
         sp_en, sp_tri_en : in std_logic;
         bp_en, bp_tri_en : in std_logic;
         si_en, si_tri_en : in std_logic;
         di_en, di_tri_en : in std_logic);

end entity datapath;

architecture bwhavioral of datapath is

    signal ES_out : std_logic_vector (15 downto 0);
    signal CS_out : std_logic_vector (15 downto 0);
    signal SS_out : std_logic_vector (15 downto 0);
    signal DS_out : std_logic_vector (15 downto 0);
    signal IP_out : std_logic_vector (15 downto 0);
    signal segment_input : std_logic_vector (15 downto 0);
    signal inc_out : std_logic_vector (15 downto 0);
    signal IP_en : std_logic;
    signal adr_gen_mux1_out : std_logic_vector (15 downto 0);
    signal queue_full : std_logic;
    signal queue_empty : std_logic;
    signal disable_inst_fetch : std_logic;
    signal queue_out : std_logic_vector(7 downto 0);
    signal data_bus_16 : std_logic_vector(15 downto 0);
    signal push_queue : std_logic;
    signal alu_temp_reg1_out : std_logic_vector(15 downto 0);
    signal alu_temp_reg2_out : std_logic_vector(15 downto 0);
    signal alu_out : std_logic_vector(15 downto 0);
    signal alu_carry_out : std_logic;
    signal alu_zero : std_logic;
    signal ax_out, bx_out,cx_out,dx_out, sp_out,bp_out,si_out,di_out : std_logic_vector(15 downto 0);




begin

    ES : entity work.reg(behavioral)
        generic map (16)
        port map(clk, rst, '0', segment_input, ES_out);
    CS : entity work.reg(behavioral)
        generic map (16)
        port map(clk, rst, '0', segment_input, CS_out);
    SS : entity work.reg(behavioral)
        generic map (16)
        port map(clk, rst, '0', segment_input, SS_out);
    DS : entity work.reg(behavioral)
        generic map (16)
        port map(clk, rst, '0', segment_input, DS_out);

    IP : entity work.reg(behavioral)
        generic map (16)
        port map(clk, rst, IP_en, inc_out, IP_out);

    INC : entity work.incrementor(behavioral)
            generic map(16)
            port map(IP_out, inc_out);

    IP_en <= not(queue_full or disable_inst_fetch);

    adr_gen_mux1: entity work.mux(behavioral)
        generic map(16)
        port map(ES_out, CS_out, SS_out, DS_out, adr_gen_mux1_sel, adr_gen_mux1_out);


    address_generator: entity work.address_calculator(behavioral)
            port map(adr_gen_mux1_out, IP_out, address_out);

    push_queue <= not disable_inst_fetch ;

    fifo_queue: entity work.queue(behavioral)
                port map(clk, rst, push_queue, pop_from_queue, mem_data_in, queue_full, queue_empty, queue_out);

    queue_out_to_ctrl <= queue_out;

    inst_reg: entity work.reg(behavioral)
        generic map (8)
        port map(clk, rst, inst_reg_en, queue_out, inst_reg_out);


    alu_temp_reg1: entity work.reg(behavioral)
        generic map(16)
        port map(clk, rst, alu_temp_reg1_en, data_bus_16, alu_temp_reg1_out);


    alu_temp_reg2: entity work.reg(behavioral)
        generic map(16)
        port map(clk, rst, alu_temp_reg2_en, data_bus_16, alu_temp_reg2_out);

    Arith_logic_unit: entity work.alu(behavioral)
        port map(alu_temp_reg1_out,alu_temp_reg2_out, alu_op_sel, alu_out, alu_carry_out, alu_zero);

    ALU_tri : entity work.TriStateBuffer(behavioral)
            port map(alu_out, ALU_tri_en, data_bus_16);


            -- entity x_registers is
            --     port (clk, rst, en,en_l,en_h: in std_logic; 
            --           d : in std_logic_vector (15 downto 0); 
            --           q : out std_logic_vector (15 downto 0));
            
            -- end entity x_registers;
    

    AX : entity work.x_registers(behavioral)
                port map(clk, rst, ax_en, ax_en_l, ax_en_h,data_bus_16, ax_out);

    AX_tri : entity work.TriStateBuffer(behavioral)
                port map(ax_out, ax_tri_en, data_bus_16);

    BX : entity work.x_registers(behavioral)
                port map(clk, rst, bx_en, bx_en_l, bx_en_h,data_bus_16, bx_out);

    BX_tri : entity work.TriStateBuffer(behavioral)
                port map(bx_out, bx_tri_en, data_bus_16);

    CX : entity work.x_registers(behavioral)
                port map(clk, rst, cx_en, cx_en_l, cx_en_h,data_bus_16, cx_out);

    CX_tri : entity work.TriStateBuffer(behavioral)
                port map(cx_out, cx_tri_en, data_bus_16);

    DX : entity work.x_registers(behavioral)
                port map(clk, rst, dx_en, dx_en_l, dx_en_h,data_bus_16, dx_out);

    DX_tri : entity work.TriStateBuffer(behavioral)
                port map(dx_out, dx_tri_en, data_bus_16);

     
        


end bwhavioral ; -- bwhavioralsab