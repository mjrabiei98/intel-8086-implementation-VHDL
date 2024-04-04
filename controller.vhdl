library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;


entity controller is 

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
         mem_write_en : out std_logic);

end entity controller;

architecture behavioral of controller is

    TYPE state IS (idle);
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

    --      sel<= '0';            load<= '0';              counter_en<= '0';      mbr_en<= '0';       done<= '0';
    --      init <='0';           counterW_en<= '0';       
	
	
	-- 	CASE pstate IS  

	-- 		WHEN idle =>

    --             pipe_out <= '1';
	-- 			done <= '0';
    --             load <= '0';
    --             mbr_en <= '0';
    --             sel <='0';
    --             counter_en <='0';
	-- 			IF (start='1' AND start'EVENT) THEN 
	-- 				nstate <= init_bias; 
	-- 			ELSE
	-- 			    nstate <= idle; 
	-- 			END IF;		

	-- 		WHEN init_bias =>

    --             pipe_out <= '0';
    --             sel <= '0';
    --             load <= '1'; 
    --             counter_en <='0';
    --             init <= '1';
    --             nstate <= MAC; 	
            
	-- 		WHEN MAC =>

    --              init <= '0';
    --              sel <= '1';
    --              counter_en <='1';
    --              counterW_en <='1';
    --              load <='1';

	-- 			if (cnt=NumOfSteps) then 

	-- 				nstate<= ready;
                    

    --             else
                    
    --                 nstate<= MAC;

    --             end if;

	-- 		WHEN ready =>
	
    --     END CASE;				
	END PROCESS;



end behavioral ; -- behavioral