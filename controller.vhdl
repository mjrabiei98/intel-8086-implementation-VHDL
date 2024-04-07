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
        move_mem_reg_opcd : STD_LOGIC_VECTOR(4 DOWNTO 0) := "10010";
        move_imd_opcd : STD_LOGIC_VECTOR(3 DOWNTO 0) := "1011";
        inc_reg_opcd : STD_LOGIC_VECTOR(4 DOWNTO 0) := "01000";
        dec_reg_opcd : STD_LOGIC_VECTOR(6 DOWNTO 0) := "1111111";
        mul_reg_reg_opcd : STD_LOGIC_VECTOR(6 DOWNTO 0) := "1111011";
        loop_opcd : STD_LOGIC_VECTOR(7 DOWNTO 0) := "11100010");

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
        number_of_pop : OUT INTEGER;
        adr_gen_mux2_sel : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
        memory_bus_tri : OUT STD_LOGIC;
        queue_empty : IN STD_LOGIC;
        queue_to_bus_tri : OUT STD_LOGIC);

END ENTITY controller;

ARCHITECTURE behavioral OF controller IS

    TYPE state IS (idle, fetch, pop_state, move_reg_reg_state, move_reg_mem_state,
        move_mem_reg_state, mevoe_immediate1, mevoe_immediate2, mul_reg_reg_state1,
        mul_reg_reg_state2, mul_reg_reg_state3);
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
        number_of_pop <= 1;
        adr_gen_mux2_sel <= "00";
        memory_bus_tri <= '0';

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
                number_of_pop <= 1;
                adr_gen_mux2_sel <= "00";
                memory_bus_tri <= '0';

            WHEN fetch =>

                inst_reg_en <= '1';
                IF queue_empty = '1'THEN
                    nstate <= fetch;
                ELSE
                    nstate <= pop_state;
                END IF;
            WHEN pop_state =>

                inst_reg_en <= '0';
                pop_from_queue <= '1';
                IF (inst_reg_out(7 DOWNTO 3) = move_mem_reg_opcd) THEN
                    IF (queue_out_to_ctrl(7 DOWNTO 6) = "11") THEN -- reg to reg
                        nstate <= move_reg_reg_state;
                    ELSIF (queue_out_to_ctrl(7 DOWNTO 6) = "01" AND inst_reg_out(1) = '1') THEN -- reg to mem
                        nstate <= move_reg_mem_state;
                    ELSE -- mem to reg
                        nstate <= move_mem_reg_state;
                    END IF;
                ELSIF (inst_reg_out(7 DOWNTO 4) = move_imd_opcd) THEN
                    nstate <= mevoe_immediate1;

                ELSIF (inst_reg_out(7 DOWNTO 1) = mul_reg_reg_opcd) THEN
                    nstate <= mul_reg_reg_state1;

                ELSE
                    nstate <= fetch;

                END IF;

            WHEN move_reg_reg_state =>

                -- if(inst_reg_out(0) = '1')then
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

                pop_from_queue <= '1';
                nstate <= fetch;

            WHEN move_reg_mem_state =>

                IF (queue_out_to_ctrl(5 DOWNTO 3) = AX_reg_opcd) THEN
                    ax_tri_en <= '1';
                ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = BX_reg_opcd) THEN
                    bx_tri_en <= '1';
                ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = CX_reg_opcd) THEN
                    cx_tri_en <= '1';
                ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = DX_reg_opcd) THEN
                    dx_tri_en <= '1';
                END IF;

                pop_from_queue <= '1';
                mem_write_en <= '1';
                adr_gen_mux2_sel <= "01";
                adr_gen_mux1_sel <= "11";
                nstate <= fetch;
                disable_inst_fetch <= '1';

            WHEN move_mem_reg_state =>

                IF (queue_out_to_ctrl(5 DOWNTO 3) = AX_reg_opcd) THEN
                    ax_en <= '1';
                ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = BX_reg_opcd) THEN
                    bx_en <= '1';
                ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = CX_reg_opcd) THEN
                    cx_en <= '1';
                ELSIF (queue_out_to_ctrl(5 DOWNTO 3) = DX_reg_opcd) THEN
                    dx_en <= '1';
                END IF;

                memory_bus_tri <= '1';
                pop_from_queue <= '1';
                adr_gen_mux2_sel <= "01";
                adr_gen_mux1_sel <= "11";
                pop_from_queue <= '1';
                nstate <= fetch;
                disable_inst_fetch <= '1';

            WHEN mevoe_immediate1 =>
                nstate <= mevoe_immediate2;
                IF (inst_reg_out(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_en_l <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_en_l <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_en_l <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_en_l <= '1';
                END IF;
                queue_to_bus_tri <= '1';

            WHEN mevoe_immediate2 =>

                queue_to_bus_tri <= '1';
                IF (inst_reg_out(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_en_h <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_en_h <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_en_h <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_en_h <= '1';
                END IF;
                pop_from_queue <= '1';
                number_of_pop <= 2;
                nstate <= fetch;

            WHEN mul_reg_reg_state1 =>
                nstate <= mul_reg_reg_state2;
                -- load first reg to temp 1
            WHEN mul_reg_reg_state2 =>
                nstate <= mul_reg_reg_state3;
                -- load second register to temp2
            WHEN mul_reg_reg_state3 =>
                nstate <= fetch;
                -- calculate the alu
                -- load low part to ax

            -- load second part to cx
            -- load flags
            -- pop

        END CASE;
    END PROCESS;

END behavioral; -- behavioral