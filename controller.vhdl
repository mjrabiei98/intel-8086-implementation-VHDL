library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


entity controller is 

    generic(move_mem_reg_opcd : std_logic_vector := "10010");

    port (clk, rst: in std_logic; ES_tri : out std_logic; adr_gen_mux1_sel : out std_logic_vector(1 downto 0); 
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
         di_en, di_tri_en : out std_logic;
         mem_write_en : out std_logic;
         disable_inst_fetch : out std_logic);

end entity controller;

architecture behavioral of controller is

    TYPE state IS (idle,fetch, pop_state, move_mem_reg_state);
	SIGNAL pstate, nstate :state := idle;

begin

    PROCESS (clk, rst)
		
	BEGIN
		IF (rst = '1') THEN
			pstate <= idle;
		ELSIF (clk = '1' AND clk'EVENT) THEN
			pstate <= nstate ;
		END IF;
	END PROCESS;

    PROCESS (pstate) BEGIN 



    ES_tri <= '0'; 
    adr_gen_mux1_sel <= "00";
    inst_reg_en <= '0';
    pop_from_queue <= '0'; 
    alu_temp_reg1_en <= '0'; 
    alu_temp_reg2_en <= '0';
    alu_op_sel <= "0000";
    ALU_tri_en <= '0';
    ax_en<= '0'; ax_en_l<= '0'; ax_en_h<= '0'; ax_tri_en <= '0';
    bx_en<= '0'; bx_en_l<= '0'; bx_en_h<= '0'; bx_tri_en <= '0';
    cx_en<= '0'; cx_en_l<= '0'; cx_en_h<= '0'; cx_tri_en <= '0';
    dx_en<= '0'; dx_en_l<= '0'; dx_en_h<= '0'; dx_tri_en <= '0';
    sp_en<= '0'; sp_tri_en <= '0';
    bp_en<= '0'; bp_tri_en <= '0';
    si_en<= '0'; si_tri_en <= '0';
    di_en<= '0'; di_tri_en <= '0';
    mem_write_en <= '0';
    disable_inst_fetch <= '0';    
	
	
		CASE pstate IS  

			WHEN idle =>
				
                nstate <= fetch; 	

			WHEN fetch =>

                inst_reg_en <= '1';
                nstate <= pop_state; 	
            
            WHEN pop_state =>

                inst_reg_en <= '0';

                if(inst_reg_out(7 downto 3) = move_mem_reg_opcd ) then

                    nstate <= move_mem_reg_state;

                else
                    nstate <= fetch; 
                end if;
            
            WHEN move_mem_reg_state =>
                nstate <= fetch;

	
        END CASE;				
	END PROCESS;



end behavioral ; -- behavioral