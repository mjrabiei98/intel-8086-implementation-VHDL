LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY reg IS
    GENERIC (register_size : INTEGER := 8);
    PORT (
        clk, rst, en : IN STD_LOGIC;
        d : IN STD_LOGIC_VECTOR (register_size - 1 DOWNTO 0);
        q : OUT STD_LOGIC_VECTOR (register_size - 1 DOWNTO 0));
END ENTITY reg;

ARCHITECTURE behavioral OF reg IS
BEGIN
    PROCESS (rst, clk)
    BEGIN
        IF (rst = '1') THEN
            q <= (OTHERS => '0');
        ELSIF (clk'event AND clk = '1' AND en = '1') THEN
            q <= d;
        END IF;
    END PROCESS;
END ARCHITECTURE behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_TEXTIO.ALL;
USE STD.TEXTIO.ALL;
USE IEEE.std_logic_arith.ALL;
ENTITY queue IS
    PORT (
        clk, rst, push, pop : IN STD_LOGIC;
        data_in : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
        full : OUT STD_LOGIC;
        empty : OUT STD_LOGIC;
        data_out : OUT STD_LOGIC_VECTOR(47 DOWNTO 0);
        number_of_pop : IN INTEGER);
END ENTITY queue;

ARCHITECTURE behavioral OF queue IS
    TYPE queue_type IS ARRAY (0 TO 5) OF STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL queue : queue_type;
    SIGNAL head, tail : INTEGER RANGE 0 TO 6 := 0;
    SIGNAL count : INTEGER RANGE 0 TO 6 := 0;
BEGIN
    PROCESS (clk, rst)
    BEGIN
        IF rst = '1' THEN
            head <= 0;
            tail <= 0;
            count <= 0;
            queue <= (OTHERS => (OTHERS => '0'));

        ELSIF (clk'event AND clk = '1') THEN
            IF (count < 5) THEN
                IF push = '1' THEN
                    queue(tail MOD 6) <= data_in(15 DOWNTO 8);
                    queue(tail MOD 6 + 1) <= data_in(7 DOWNTO 0);
                    tail <= (tail + 2) MOD 6;
                    count <= count + 2;
                END IF;
            END IF;
        ELSIF (clk'event AND clk = '0') THEN

            IF pop = '1' THEN
                head <= (head + number_of_pop) MOD 6;
                if number_of_pop = 6 then
                    count <= 0;
                    head <= 0;
                    tail <= 0;
                else
                    count <= count - number_of_pop;
                end if;
            END IF;

        END IF;
    END PROCESS;

    full <= '1' WHEN count > 4 ELSE
        '0';
    empty <= '1' WHEN count = 0 ELSE
        '0';

    data_out <= queue((head + 5) MOD 6) & queue((head + 4) MOD 6) & queue((head + 3) MOD 6) & queue((head + 2) MOD 6) & queue((head + 1) MOD 6) & queue(head MOD 6);
END ARCHITECTURE behavioral;
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY incrementor IS
    GENERIC (input_size : INTEGER := 16);
    PORT (
        data_in : STD_LOGIC_VECTOR (input_size - 1 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(input_size - 1 DOWNTO 0));
END ENTITY incrementor;

ARCHITECTURE behavioral OF incrementor IS
BEGIN

    data_out <= STD_LOGIC_VECTOR(unsigned(data_in) + 1);

END ARCHITECTURE behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY x_registers IS
    PORT (
        clk, rst, en, en_l, en_h : IN STD_LOGIC;
        d : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
        q : OUT STD_LOGIC_VECTOR (15 DOWNTO 0));

END ENTITY x_registers;

ARCHITECTURE behavioral OF x_registers IS
    SIGNAL q_h, q_l : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL reg_data : STD_LOGIC_VECTOR(15 DOWNTO 0);
BEGIN
    PROCESS (rst, clk)
    BEGIN
        IF (rst = '1') THEN
            q <= (OTHERS => '0');
            q_h <= (OTHERS => '0');
            q_l <= (OTHERS => '0');
            reg_data <= d;
        ELSIF (clk'event AND clk = '1' AND en = '1') THEN
            reg_data <= d;
            q <= d;
        ELSIF (clk'event AND clk = '1' AND en_l = '1') THEN

            q_l <= reg_data(15 DOWNTO 8) & d(7 DOWNTO 0);
            q <= reg_data(15 DOWNTO 8) & d(7 DOWNTO 0);
            reg_data <= reg_data(15 DOWNTO 8) & d(7 DOWNTO 0);

        ELSIF (clk'event AND clk = '1' AND en_h = '1') THEN
            q_h <= d(15 DOWNTO 8) & reg_data(7 DOWNTO 0);
            reg_data <= d(15 DOWNTO 8) & reg_data(7 DOWNTO 0);
            q <= d(15 DOWNTO 8) & reg_data(7 DOWNTO 0);
        END IF;
    END PROCESS;
END ARCHITECTURE behavioral;

-- architecture gate_level of x_registers is 
--     signal temp: std_logic_vector(15 downto 0);
-- begin

--     xreg_h: entity work.reg(behavioral)
--           generic map(8)
--           port map(clk, rst, en, d(15 downto 8), temp(15 downto 8));

--     xreg_l: entity work.reg(behavioral)
--             generic map(8)
--             port map(clk, rst, en, d(15 downto 8), temp(7 downto 0));

--     q <= temp;
--     q_h <= temp(15 downto 8);
--     q_l <= temp(7 downto 0);
-- end architecture gate_level;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY address_calculator IS
    GENERIC (address_size : INTEGER := 16);
    PORT (
        a, b : IN STD_LOGIC_VECTOR(address_size - 1 DOWNTO 0);
        address_out : OUT STD_LOGIC_VECTOR(address_size - 1 DOWNTO 0));
END ENTITY address_calculator;

ARCHITECTURE behavioral OF address_calculator IS
    SIGNAL temp : STD_LOGIC_VECTOR(31 DOWNTO 0);
BEGIN
    temp <= STD_LOGIC_VECTOR(unsigned(a) * 16 + unsigned(b));
    address_out <= temp(15 DOWNTO 0);
END ARCHITECTURE behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
ENTITY mux IS
    GENERIC (input_size : INTEGER := 16);
    PORT (
        a, b, c, d : IN STD_LOGIC_VECTOR(input_size - 1 DOWNTO 0);
        sel : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(input_size - 1 DOWNTO 0));
END ENTITY mux;

ARCHITECTURE behavioral OF mux IS
BEGIN
    PROCESS (a, b, c, d, sel)
    BEGIN
        IF sel = "00" THEN
            data_out <= a;
        ELSIF sel = "01" THEN
            data_out <= b;
        ELSIF sel = "10" THEN
            data_out <= c;
        ELSIF sel = "11" THEN
            data_out <= d;
        ELSE
            data_out <= a;
        END IF;
    END PROCESS;
END ARCHITECTURE behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY alu IS
    GENERIC (input_size : INTEGER := 16);
    PORT (
        a, b : IN STD_LOGIC_VECTOR(input_size - 1 DOWNTO 0);
        op_sel : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        result : OUT STD_LOGIC_VECTOR(input_size - 1 DOWNTO 0);
        alu_flag_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
END ENTITY alu;

ARCHITECTURE Behavioral OF alu IS
BEGIN
    PROCESS (op_sel, a, b)
        VARIABLE sum_extended : STD_LOGIC_VECTOR(16 DOWNTO 0);
        variable mul_resualt : STD_LOGIC_VECTOR(31 DOWNTO 0);
        variable data_out : STD_LOGIC_VECTOR(15 DOWNTO 0);
    BEGIN
        alu_flag_out <= (others => '0');
        CASE op_sel IS
            WHEN "0000" => -- Addition
                sum_extended := STD_LOGIC_VECTOR(unsigned('0' & a) + unsigned('0' & b));
                data_out := STD_LOGIC_VECTOR(signed(a) + signed(b));
                alu_flag_out(1) <= sum_extended(15); -- carry flag
            WHEN "0001" => -- Subtraction
                sum_extended := STD_LOGIC_VECTOR(signed(a) - signed(b));
                data_out := STD_LOGIC_VECTOR(signed(a) - signed(b));
            WHEN "0010" => -- AND
                data_out := a AND b;
            WHEN "0011" => -- OR
                data_out := a OR b;
            WHEN "0100" => -- XOR
                data_out := a XOR b;
            WHEN "0101" => -- mult
                mul_resualt := STD_LOGIC_VECTOR(signed(a) * signed(b));
                data_out := mul_resualt(15 downto 0);
            WHEN "0110" => -- inc
                data_out := STD_LOGIC_VECTOR(signed(a) + 1);
            WHEN "0111" => -- dec
                data_out := STD_LOGIC_VECTOR(signed(a) - 1);
                
            when "1000" => --ROL_FUNC
                data_out := std_logic_vector(signed(a) ROL to_integer(signed(b)));
            when "1001" => -- SRL_FUNC
                data_out := std_logic_vector(signed(a) SRL to_integer(signed(b)));
            when "1100" => --SLL_FUNC
                data_out := std_logic_vector(signed(a) SLL to_integer(signed(b)));
            when "1101" => --DIV_FUNC
                data_out := std_logic_vector(signed(a) / signed(b));
            when "1110" => --ADC_FUNC
                data_out := std_logic_vector(signed(a) + signed(b));
            when "1111" => --SL1_FUNC
                data_out := std_logic_vector(signed(a) SLL 1);
            WHEN OTHERS => -- Default or undefined operation
                data_out := (OTHERS => 'X');
        END CASE;
        result <= data_out;
        if data_out = "0000000000000000" then -- zero flag
            alu_flag_out(0) <= '1';
        end if;
        -- parity
        alu_flag_out(2) <= NOT(data_out(0) XOR data_out(1) XOR data_out(2) XOR data_out(3) XOR data_out(4) XOR data_out(5) XOR data_out(6) XOR data_out(7) XOR data_out(8) XOR data_out(9) XOR data_out(10) XOR data_out(11) XOR data_out(12) XOR data_out(13) XOR data_out(14) XOR data_out(15));
    END PROCESS;

END Behavioral;
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY TriStateBuffer IS
    GENERIC (buffer_size : INTEGER := 16);
    PORT (
        data_in : IN STD_LOGIC_VECTOR(buffer_size - 1 DOWNTO 0); -- Input data
        enable : IN STD_LOGIC; -- Enable signal for the buffer
        data_out : OUT STD_LOGIC_VECTOR(buffer_size - 1 DOWNTO 0) -- Output data
    );
END TriStateBuffer;

ARCHITECTURE Behavioral OF TriStateBuffer IS
BEGIN
    PROCESS (data_in, enable)
    BEGIN
        IF enable = '1' THEN
            data_out <= data_in; -- Drive the signal
        ELSE
            data_out <= (OTHERS => 'Z'); -- High impedance
        END IF;
    END PROCESS;
END Behavioral;