LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE IEEE.NUMERIC_STD.ALL;
ENTITY controller IS

    GENERIC (
        AX_reg_opcd : STD_LOGIC_VECTOR(2 DOWNTO 0) := "000";
        CX_reg_opcd : STD_LOGIC_VECTOR(2 DOWNTO 0) := "001";
        DX_reg_opcd : STD_LOGIC_VECTOR(2 DOWNTO 0) := "010";
        BX_reg_opcd : STD_LOGIC_VECTOR(2 DOWNTO 0) := "011";
        move_mem_reg_opcd : STD_LOGIC_VECTOR(4 DOWNTO 0) := "10010");

    PORT (
        clk, rst : IN STD_LOGIC;
        ES_tri : OUT STD_LOGIC;
        adr_gen_mux1_sel : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
        queue_out_to_ctrl : IN STD_LOGIC_VECTOR(47 DOWNTO 0);
        inst_reg_out : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        inst_reg_en : OUT STD_LOGIC;
        pop_from_queue, alu_temp_reg1_en, alu_temp_reg2_en : OUT STD_LOGIC;
        alu_op_sel : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
        ALU_tri_en : OUT STD_LOGIC;
        ax_en, ax_en_l, ax_en_h, ax_tri_en : OUT STD_LOGIC;
        bx_en, bx_en_l, bx_en_h, bx_tri_en : OUT STD_LOGIC;
        cx_en, cx_en_l, cx_en_h, cx_tri_en : OUT STD_LOGIC;
        dx_en, dx_en_l, dx_en_h, dx_tri_en : OUT STD_LOGIC;
        sp_en, sp_tri_en : OUT STD_LOGIC;
        bp_en, bp_tri_en : OUT STD_LOGIC;
        si_en, si_tri_en : OUT STD_LOGIC;
        di_en, di_tri_en : OUT STD_LOGIC;
        mem_write_en : OUT STD_LOGIC;
        disable_inst_fetch : OUT STD_LOGIC;
        number_of_pop : out integer);

END ENTITY controller;

ARCHITECTURE behavioral OF controller IS

    TYPE state IS (idle, fetch, pop_state, move_reg_reg_state, move_reg_mem_state, move_mem_reg_state);
    SIGNAL pstate, nstate : state := idle;

BEGIN

    PROCESS (clk, rst)

    BEGIN
        IF (rst = '1') THEN
            pstate <= idle;
        ELSIF (clk = '1' AND clk'EVENT) THEN
            pstate <= nstate;
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
        number_of_pop <= 1;    
        CASE pstate IS

            WHEN idle =>

                nstate <= fetch;
                ES_tri <= '0';
                adr_gen_mux1_sel <= "00";
                inst_reg_en <= '0';
                pop_from_queue <= '0';
                alu_temp_reg1_en <= '0';
                alu_temp_reg2_en <= '0';
                alu_op_sel <= "0000";
                ALU_tri_en <= '0';
                ax_en <= '0';
                ax_en_l <= '0';
                ax_en_h <= '0';
                ax_tri_en <= '0';
                bx_en <= '0';
                bx_en_l <= '0';
                bx_en_h <= '0';
                bx_tri_en <= '0';
                cx_en <= '0';
                cx_en_l <= '0';
                cx_en_h <= '0';
                cx_tri_en <= '0';
                dx_en <= '0';
                dx_en_l <= '0';
                dx_en_h <= '0';
                dx_tri_en <= '0';
                sp_en <= '0';
                sp_tri_en <= '0';
                bp_en <= '0';
                bp_tri_en <= '0';
                si_en <= '0';
                si_tri_en <= '0';
                di_en <= '0';
                di_tri_en <= '0';
                mem_write_en <= '0';
                disable_inst_fetch <= '0';

            WHEN fetch =>

                inst_reg_en <= '1';
                nstate <= pop_state;

            WHEN pop_state =>

                inst_reg_en <= '0';
                pop_from_queue <= '1';
                IF (inst_reg_out(7 DOWNTO 3) = move_mem_reg_opcd) THEN
                    IF (queue_out_to_ctrl(7 DOWNTO 6) = "11") THEN -- reg to reg
                        nstate <= move_reg_reg_state;
                    ELSIF (queue_out_to_ctrl(7 DOWNTO 6) = "01" and inst_reg_out(1) = '1') THEN -- reg to mem
                        nstate <= move_reg_mem_state;
                    ELSE -- mem to reg
                        nstate <= move_mem_reg_state;
                    END IF;
                ELSE
                    nstate <= fetch;
                END IF;

            WHEN move_reg_reg_state =>

                -- if(inst_reg_out(0) = '1')then
                IF queue_out_to_ctrl(7 DOWNTO 6) = "11" THEN
                    IF (queue_out_to_ctrl(5 DOWNTO 3) = AX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = BX_reg_opcd) THEN
                        ax_tri_en <= '1';
                        bx_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = AX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = CX_reg_opcd) THEN
                        ax_tri_en <= '1';
                        cx_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = AX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = DX_reg_opcd) THEN
                        ax_tri_en <= '1';
                        dx_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = BX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = AX_reg_opcd) THEN
                        bx_tri_en <= '1';
                        ax_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = BX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = CX_reg_opcd) THEN
                        bx_tri_en <= '1';
                        cx_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = BX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = DX_reg_opcd) THEN
                        bx_tri_en <= '1';
                        dx_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = CX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = AX_reg_opcd) THEN
                        cx_tri_en <= '1';
                        ax_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = CX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = BX_reg_opcd) THEN
                        cx_tri_en <= '1';
                        bx_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = CX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = DX_reg_opcd) THEN
                        cx_tri_en <= '1';
                        dx_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = DX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = AX_reg_opcd) THEN
                        dx_tri_en <= '1';
                        ax_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = DX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = BX_reg_opcd) THEN
                        dx_tri_en <= '1';
                        bx_en <= '1';
                    ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = DX_reg_opcd AND queue_out_to_ctrl(2 DOWNTO 0) = CX_reg_opcd) THEN
                        dx_tri_en <= '1';
                        cx_en <= '1';
                    END IF;
                END IF;

                pop_from_queue <= '1';
                nstate <= fetch;
            
            WHEN move_reg_mem_state =>
                
                if (queue_out_to_ctrl(5 DOWNTO 3) = AX_reg_opcd) then
                    ax_tri_en <= '1';
                elsif (queue_out_to_ctrl(5 DOWNTO 3) = BX_reg_opcd) then
                    bx_tri_en <= '1';
                elsif (queue_out_to_ctrl(5 DOWNTO 3) = CX_reg_opcd) then
                    cx_tri_en <= '1';
                elsif (queue_out_to_ctrl(5 DOWNTO 3) = DX_reg_opcd) then
                    dx_tri_en <= '1';
                end if;
                    
                -- pop_from_queue <= '1';
                mem_write_en <= '1';
                nstate <= fetch;

            WHEN move_mem_reg_state =>
                pop_from_queue <= '1';
                nstate <= fetch;

        END CASE;
    END PROCESS;

END behavioral; -- behavioral