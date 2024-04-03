library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


entity controller is 

    port (clk, rst, ES_tri : out std_logic; adr_gen_mux1_sel : out std_logic_vector(1 downto 0); 
         queue_out_to_ctrl : in std_logic_vector(7 downto 0);
         inst_reg_out : in std_logic_vector(7 downto 0);
         inst_reg_en : out std_logic;
         pop_from_queue, alu_temp_reg1_en, alu_temp_reg2_en : out std_logic;
         alu_op_sel : out std_logic_vector(3 downto 0);
         ALU_tri_en : out std_logic;
         ax_en, ax_en_l, ax_en_h, ax_tri_en : out std_logic;
         bx_en, bx_en_l, bx_en_h, bx_tri_en : out std_logic;
         cx_en, cx_en_l, cx_en_h, cx_tri_en : out std_logic;
         dx_en, dx_en_l, dx_en_h, dx_tri_en : out std_logic;
         sp_en, sp_tri_en : out std_logic;
         bp_en, bp_tri_en : out std_logic;
         si_en, si_tri_en : out std_logic;
         di_en, di_tri_en : out std_logic);

end entity controller;

architecture behavioral of controller is
begin

    

end behavioral ; -- behavioral