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
        SP_reg_opcd : STD_LOGIC_VECTOR(2 DOWNTO 0) := "100";
        BP_reg_opcd : STD_LOGIC_VECTOR(2 DOWNTO 0) := "101";
        SI_reg_opcd : STD_LOGIC_VECTOR(2 DOWNTO 0) := "110";
        DI_reg_opcd : STD_LOGIC_VECTOR(2 DOWNTO 0) := "111";
        move_mem_reg_opcd : STD_LOGIC_VECTOR(4 DOWNTO 0) := "10010";
        move_imd_opcd : STD_LOGIC_VECTOR(3 DOWNTO 0) := "1011";
        inc_reg_opcd : STD_LOGIC_VECTOR(4 DOWNTO 0) := "01000";
        dec_reg_opcd : STD_LOGIC_VECTOR(4 DOWNTO 0) := "01001";
        mul_reg_reg_opcd : STD_LOGIC_VECTOR(6 DOWNTO 0) := "1111011";
        loop_disp_opcd : STD_LOGIC_VECTOR(7 DOWNTO 0) := "11100010";
        loopz_disp_opcd : STD_LOGIC_VECTOR(7 DOWNTO 0) := "11100001";
        adc_im_opcd : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0001010";
        add_im_opcd : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0000010";
        add_mm_opcd : STD_LOGIC_VECTOR(6 DOWNTO 0) := "0000001";
        cbw_opcd : STD_LOGIC_VECTOR(7 DOWNTO 0) := "01100010");

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
        queue_to_bus_tri : OUT STD_LOGIC;
        ip_mux_sel : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
        flag_reg_out : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        flag_reg_en : OUT STD_LOGIC;
        update_IP_loop : OUT STD_LOGIC);

END ENTITY controller;

ARCHITECTURE behavioral OF controller IS

    TYPE state IS (idle, fetch, decode_state, move_reg_reg_state, move_reg_mem_state,
        move_mem_reg_state, mevoe_immediate1, mevoe_immediate2, mevoe_immediate3, mul_reg_reg_state1,
        mul_reg_reg_state2, mul_reg_reg_state3, inc_state1, inc_state2, dec_state1, dec_state2,
        loopz_disp_state, loopz_2, loopz_3, loopz_4,
        ADC1, ADC2, ADC3, ADC4, ADC5, ADC6,
        ADD_im1, ADD_im2, ADD_im3, ADD_im4, ADD_im5,
        ADD_mm1, ADD_mm2, ADD_mm3,
        cbw1, cbw2);
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
        ip_mux_sel <= "00";
        flag_reg_en <= '0';
        queue_to_bus_tri <= '0';
        update_IP_loop <= '0';

        CASE pstate IS

            WHEN idle =>

                nstate <= fetch;

            WHEN fetch =>

                inst_reg_en <= '1';
                nstate <= decode_state;

            WHEN decode_state =>

                inst_reg_en <= '0';
                pop_from_queue <= '1';
                IF (inst_reg_out(7 DOWNTO 3) = move_mem_reg_opcd) THEN
                    IF (queue_out_to_ctrl(15 DOWNTO 14) = "11") THEN -- reg to reg
                        nstate <= move_reg_reg_state;
                    ELSIF (queue_out_to_ctrl(15 DOWNTO 14) = "01") THEN -- reg to mem
                        nstate <= move_reg_mem_state;
                    ELSE -- mem to reg
                        nstate <= move_mem_reg_state;
                    END IF;
                ELSIF (inst_reg_out(7 DOWNTO 4) = move_imd_opcd) THEN
                    nstate <= mevoe_immediate1;

                ELSIF (inst_reg_out(7 DOWNTO 1) = mul_reg_reg_opcd) THEN
                    nstate <= mul_reg_reg_state1;

                ELSIF (inst_reg_out(7 DOWNTO 3) = inc_reg_opcd) THEN
                    nstate <= inc_state1;

                ELSIF (inst_reg_out(7 DOWNTO 3) = dec_reg_opcd) THEN
                    nstate <= dec_state1;

                ELSIF (inst_reg_out(7 DOWNTO 0) = loopz_disp_opcd) THEN
                    nstate <= loopz_disp_state;

                ELSIF (inst_reg_out(7 DOWNTO 1) = adc_im_opcd) THEN
                    nstate <= ADC1;

                ELSIF (inst_reg_out(7 DOWNTO 1) = add_im_opcd) THEN
                    nstate <= ADD_im1;

                ELSIF (inst_reg_out(7 DOWNTO 1) = add_mm_opcd) THEN
                    nstate <= ADD_mm1;

                ELSIF (inst_reg_out(7 DOWNTO 0) = cbw_opcd) THEN
                    nstate <= cbw1;

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
                adr_gen_mux2_sel <= "11";
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
                adr_gen_mux2_sel <= "11";
                adr_gen_mux1_sel <= "11";
                -- pop_from_queue <= '1';
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
                nstate <= mevoe_immediate3;

            WHEN mevoe_immediate3 =>

                pop_from_queue <= '1';
                number_of_pop <= 2;
                nstate <= fetch;

            WHEN mul_reg_reg_state1 =>

                nstate <= mul_reg_reg_state2;
                alu_temp_reg1_en <= '1';
                ax_tri_en <= '1';

            WHEN mul_reg_reg_state2 =>

                nstate <= mul_reg_reg_state3;
                alu_temp_reg2_en <= '1';
                IF (queue_out_to_ctrl(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_tri_en <= '1';
                ELSIF (queue_out_to_ctrl(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_tri_en <= '1';
                ELSIF (queue_out_to_ctrl(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_tri_en <= '1';
                ELSIF (queue_out_to_ctrl(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_tri_en <= '1';
                END IF;

            WHEN mul_reg_reg_state3 =>

                nstate <= fetch;
                alu_op_sel <= "0101";
                ALU_tri_en <= '1';
                ax_en <= '1';
                pop_from_queue <= '1';
                number_of_pop <= 1;
                nstate <= fetch;
                -- flag_reg_en <= '1';

                -- load second part to cx
            WHEN inc_state1 =>
                IF (inst_reg_out(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SP_reg_opcd) THEN
                    sp_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BP_reg_opcd) THEN
                    bp_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SI_reg_opcd) THEN
                    si_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DI_reg_opcd) THEN
                    di_tri_en <= '1';
                END IF;
                alu_temp_reg1_en <= '1';
                nstate <= inc_state2;

            WHEN inc_state2 =>
                IF (inst_reg_out(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SP_reg_opcd) THEN
                    sp_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BP_reg_opcd) THEN
                    bp_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SI_reg_opcd) THEN
                    si_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DI_reg_opcd) THEN
                    di_en <= '1';
                END IF;
                ALU_tri_en <= '1';
                alu_op_sel <= "0110";
                nstate <= fetch;

            WHEN dec_state1 =>

                IF (inst_reg_out(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SP_reg_opcd) THEN
                    sp_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BP_reg_opcd) THEN
                    bp_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SI_reg_opcd) THEN
                    si_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DI_reg_opcd) THEN
                    di_tri_en <= '1';
                END IF;
                alu_temp_reg1_en <= '1';
                nstate <= dec_state2;

            WHEN dec_state2 =>
                IF (inst_reg_out(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SP_reg_opcd) THEN
                    sp_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BP_reg_opcd) THEN
                    bp_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SI_reg_opcd) THEN
                    si_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DI_reg_opcd) THEN
                    di_en <= '1';
                END IF;
                ALU_tri_en <= '1';
                alu_op_sel <= "0111";
                nstate <= fetch;

            WHEN loopz_disp_state =>
                nstate <= loopz_2;
                alu_temp_reg1_en <= '1';
                cx_tri_en <= '1';

            WHEN loopz_2 =>
                nstate <= loopz_3;
                alu_op_sel <= "0111";
                flag_reg_en <= '1';
            WHEN loopz_3 =>
                alu_op_sel <= "0111";
                alu_tri_en <= '1';
                cx_en <= '1';
                IF flag_reg_out(0) = '0' THEN
                    ip_mux_sel <= "01";
                    update_IP_loop <= '1';
                    nstate <= loopz_4;
                ELSE
                    nstate <= fetch;
                END IF;

            WHEN loopz_4 =>
                nstate <= fetch;
                pop_from_queue <= '1';
                number_of_pop <= 6;

            WHEN ADC1 =>
                queue_to_bus_tri <= '1';
                bx_en_l <= '1';
                nstate <= ADC2;

            WHEN ADC2 =>
                queue_to_bus_tri <= '1';
                bx_en_h <= '1';
                nstate <= ADC3;

            WHEN ADC3 =>
                ax_tri_en <= '1';
                alu_temp_reg1_en <= '1';
                nstate <= ADC4;
                pop_from_queue <= '1';

            WHEN ADC4 =>
                bx_tri_en <= '1';
                alu_temp_reg2_en <= '1';
                pop_from_queue <= '1';
                nstate <= ADC5;

            WHEN ADC5 =>
                alu_op_sel <= "0000"; --add
                alu_tri_en <= '1';
                alu_temp_reg1_en <= '1';
                nstate <= ADC6;

            WHEN ADC6 =>
                alu_op_sel <= "0110"; --inc
                alu_tri_en <= '1';
                ax_en <= '1';
                nstate <= fetch;
                flag_reg_en <= '1';

            WHEN ADD_im1 =>
                queue_to_bus_tri <= '1';
                bx_en_l <= '1';
                nstate <= ADD_im2;

            WHEN ADD_im2 =>
                queue_to_bus_tri <= '1';
                bx_en_h <= '1';
                nstate <= ADD_im3;

            WHEN ADD_im3 =>
                ax_tri_en <= '1';
                alu_temp_reg1_en <= '1';
                nstate <= ADD_im4;
                pop_from_queue <= '1';

            WHEN ADD_im4 =>
                bx_tri_en <= '1';
                alu_temp_reg2_en <= '1';
                pop_from_queue <= '1';
                nstate <= ADD_im5;

            WHEN ADD_im5 =>
                alu_op_sel <= "0000"; --add
                alu_tri_en <= '1';
                ax_en <= '1';
                flag_reg_en <= '1';
                nstate <= fetch;

            WHEN ADD_mm1 =>
                IF (inst_reg_out(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SP_reg_opcd) THEN
                    sp_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BP_reg_opcd) THEN
                    bp_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SI_reg_opcd) THEN
                    si_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DI_reg_opcd) THEN
                    di_tri_en <= '1';
                END IF;
                alu_temp_reg1_en <= '1';
                nstate <= ADD_mm2;

            WHEN ADD_mm2 =>
                IF (inst_reg_out(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SP_reg_opcd) THEN
                    sp_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BP_reg_opcd) THEN
                    bp_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SI_reg_opcd) THEN
                    si_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DI_reg_opcd) THEN
                    di_en <= '1';
                END IF;
                alu_temp_reg1_en <= '1';
                nstate <= ADD_mm3;

            WHEN ADD_mm3 =>
                alu_op_sel <= "0000"; --add
                alu_tri_en <= '1';
                IF (inst_reg_out(2 DOWNTO 0) = AX_reg_opcd) THEN
                    ax_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BX_reg_opcd) THEN
                    bx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = CX_reg_opcd) THEN
                    cx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DX_reg_opcd) THEN
                    dx_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SP_reg_opcd) THEN
                    sp_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = BP_reg_opcd) THEN
                    bp_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = SI_reg_opcd) THEN
                    si_tri_en <= '1';
                ELSIF (inst_reg_out(2 DOWNTO 0) = DI_reg_opcd) THEN
                    di_tri_en <= '1';
                END IF;
                flag_reg_en <= '1';
                pop_from_queue <= '1';
                nstate <= fetch;

            WHEN cbw1 =>
                ax_tri_en <= '1';
                alu_temp_reg1_en <= '1';
                nstate <= cbw2;

            WHEN cbw2 =>
                alu_op_sel <= "1011"; --signextend
                alu_tri_en <= '1';
                ax_en <= '1';
                pop_from_queue <= '1';
                nstate <= fetch;

        END CASE;
    END PROCESS;

END behavioral; -- behavioral