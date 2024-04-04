library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


-- entity controller is 

--     port (clk, rst, ES_tri : out std_logic; adr_gen_mux1_sel : out std_logic_vector(1 downto 0); 
--          queue_out_to_ctrl : in std_logic_vector(7 downto 0);
--          inst_reg_out : in std_logic_vector(7 downto 0);
--          inst_reg_en : out std_logic;
--          pop_from_queue, alu_temp_reg1_en, alu_temp_reg2_en : out std_logic;
--          alu_op_sel : out std_logic_vector(3 downto 0);
--          ALU_tri_en : out std_logic;
--          ax_en, ax_en_l, ax_en_h, ax_tri_en : out std_logic;
--          bx_en, bx_en_l, bx_en_h, bx_tri_en : out std_logic;
--          cx_en, cx_en_l, cx_en_h, cx_tri_en : out std_logic;
--          dx_en, dx_en_l, dx_en_h, dx_tri_en : out std_logic;
--          sp_en, sp_tri_en : out std_logic;
--          bp_en, bp_tri_en : out std_logic;
--          si_en, si_tri_en : out std_logic;
--          di_en, di_tri_en : out std_logic);

-- end entity controller;

-- entity datapath is 

--     port (clk, rst, ES_tri : in std_logic; adr_gen_mux1_sel : in std_logic_vector(1 downto 0); 
--          address_out : out std_logic_vector(15 downto 0);
--          mem_data_in : in std_logic_vector(15 downto 0);
--          queue_out_to_ctrl : out std_logic_vector(7 downto 0);
--          inst_reg_out : out std_logic_vector(7 downto 0);
--          inst_reg_en : in std_logic;
--          pop_from_queue, alu_temp_reg1_en, alu_temp_reg2_en : in std_logic;
--          alu_op_sel : in std_logic_vector(3 downto 0);
--          ALU_tri_en : in std_logic;
--          ax_en, ax_en_l, ax_en_h, ax_tri_en : in std_logic;
--          bx_en, bx_en_l, bx_en_h, bx_tri_en : in std_logic;
--          cx_en, cx_en_l, cx_en_h, cx_tri_en : in std_logic;
--          dx_en, dx_en_l, dx_en_h, dx_tri_en : in std_logic;
--          sp_en, sp_tri_en : in std_logic;
--          bp_en, bp_tri_en : in std_logic;
--          si_en, si_tri_en : in std_logic;
--          di_en, di_tri_en : in std_logic);

-- end entity datapath;

entity processor is 
    port(clk,rst: in std_logic; address_out : out std_logic_vector(15 downto 0); mem_data_in : in std_logic_vector(15 downto 0));
end entity processor;

architecture behavioral of processor is

    signal ES_tri : std_logic; 
    signal adr_gen_mux1_sel : std_logic_vector(1 downto 0); 
    signal queue_out_to_ctrl : std_logic_vector(7 downto 0);
    signal inst_reg_out : std_logic_vector(7 downto 0);
    signal inst_reg_en : std_logic;
    signal pop_from_queue, alu_temp_reg1_en, alu_temp_reg2_en : std_logic;
    signal alu_op_sel : std_logic_vector(3 downto 0);
    signal ALU_tri_en : std_logic;
    signal ax_en, ax_en_l, ax_en_h, ax_tri_en : std_logic;
    signal bx_en, bx_en_l, bx_en_h, bx_tri_en : std_logic;
    signal cx_en, cx_en_l, cx_en_h, cx_tri_en : std_logic;
    signal dx_en, dx_en_l, dx_en_h, dx_tri_en : std_logic;
    signal sp_en, sp_tri_en : std_logic;
    signal bp_en, bp_tri_en : std_logic;
    signal si_en, si_tri_en : std_logic;
    signal di_en, di_tri_en : std_logic;

begin

    Data_Path: entity work.datapath(behavioral)
        port map(clk, rst, ES_tri, adr_gen_mux1_sel,
                 address_out,
                 mem_data_in,
                 queue_out_to_ctrl,
                 inst_reg_out,
                 inst_reg_en,
                 pop_from_queue, alu_temp_reg1_en, alu_temp_reg2_en,
                 alu_op_sel,
                 ALU_tri_en,
                 ax_en, ax_en_l, ax_en_h, ax_tri_en,
                 bx_en, bx_en_l, bx_en_h, bx_tri_en,
                 cx_en, cx_en_l, cx_en_h, cx_tri_en,
                 dx_en, dx_en_l, dx_en_h, dx_tri_en,
                 sp_en, sp_tri_en,
                 bp_en, bp_tri_en,
                 si_en, si_tri_en,
                 di_en, di_tri_en); 

    Contrl: entity work.controller(behavioral)
        port map(clk, rst, ES_tri, adr_gen_mux1_sel, 
                 queue_out_to_ctrl,
                 inst_reg_out,
                 inst_reg_en,
                 pop_from_queue, alu_temp_reg1_en, alu_temp_reg2_en,
                 alu_op_sel,
                 ALU_tri_en,
                 ax_en, ax_en_l, ax_en_h, ax_tri_en,
                 bx_en, bx_en_l, bx_en_h, bx_tri_en,
                 cx_en, cx_en_l, cx_en_h, cx_tri_en,
                 dx_en, dx_en_l, dx_en_h, dx_tri_en,
                 sp_en, sp_tri_en,
                 bp_en, bp_tri_en,
                 si_en, si_tri_en,
                 di_en, di_tri_en);

end behavioral ; -- behavioral