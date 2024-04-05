library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity reg is 
    generic (register_size : integer := 8);
    port (clk, rst, en: in std_logic; d : in std_logic_vector (register_size - 1 downto 0); q : out std_logic_vector (register_size-1 downto 0 ) );
end entity reg;

architecture behavioral of reg is 
begin
    process(rst,clk)
    begin 
        if(rst = '1') then q <= (others => '0'); 
        elsif (clk'event and clk = '1' and en = '1') then q <= d;
        end if;
    end process;
end architecture behavioral;





library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_TEXTIO.ALL;
use STD.TEXTIO.ALL;
use IEEE.std_logic_arith.all;


entity queue is
    port(clk, rst, push, pop : in std_logic; 
         data_in : in std_logic_vector (15 downto 0); 
         full : out std_logic;
         empty : out std_logic;
         data_out : out std_logic_vector(47 downto 0));
end entity queue;

architecture behavioral of queue is
    type queue_type is array (0 to 5) of STD_LOGIC_VECTOR(7 downto 0);
    signal queue : queue_type;
    signal head, tail : integer range 0 to 6 := 0;
    signal count : integer range 0 to 6 := 0;
begin
    process(clk, rst)
    begin
        if rst = '1' then
            head <= 0;
            tail <= 0;
            count <= 0;
            queue <= (others => (others => '0'));

        elsif (clk'event and clk = '1') then
            if (count < 5) then 
                if push = '1' then
                    queue(tail mod 6) <= data_in(15 downto 8);
                    queue(tail mod 6 + 1) <= data_in(7 downto 0);
                    tail <= (tail + 2) mod 6 ;
                    count <= count + 2;
                end if;
            end if;
            if pop = '1' then
                head <= (head + 1) mod 6;
                count <= count - 1;
            end if;
        end if;
    end process;

    full <= '1' when count > 4 else '0';
    empty <= '1' when count = 0 else '0';

    data_out <= queue((head+5) mod 6) & queue((head+4) mod 6) & queue((head+3) mod 6) & queue((head+2) mod 6) & queue((head+1) mod 6) & queue(head mod 6); 


end architecture behavioral;




library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity incrementor is
    generic(input_size : integer := 16);
    port( data_in : std_logic_vector (input_size-1 downto 0); data_out : out std_logic_vector(input_size-1 downto 0));
end entity incrementor;

architecture behavioral of incrementor is 
begin

    data_out <= std_logic_vector(unsigned(data_in) + 1);

end architecture behavioral;





library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity x_registers is
    port (clk, rst, en,en_l,en_h: in std_logic; 
          d : in std_logic_vector (15 downto 0); 
          q : out std_logic_vector (15 downto 0));

end entity x_registers;

architecture behavioral of x_registers is 
    signal q_h, q_l : std_logic_vector(15 downto 0);
    signal reg_data : std_logic_vector(15 downto 0);
begin
    process(rst,clk)
    begin 
        if(rst = '1') then
            q <= (others => '0');
            q_h <= (others => '0');
            q_l <= (others => '0');
            reg_data <= d;
        elsif (clk'event and clk = '1' and en = '1') then 
            reg_data <= d;
            q <= d;
        elsif (clk'event and clk = '1' and en_l = '1') then 
            
            q_l <= reg_data(15 downto 0) & d(7 downto 0);
            q <= q_l;
            reg_data <= reg_data(15 downto 0) & d(7 downto 0);

        elsif (clk'event and clk = '1' and en_h = '1') then 
            q_h <= d(15 downto 8) & reg_data(7 downto 0);
            reg_data <= d(15 downto 8) & reg_data(7 downto 0);
            q <= q_h;
        end if;
    end process;
end architecture behavioral;

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




library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity address_calculator is
    generic (address_size: integer := 16);
    port(a,b : in std_logic_vector(address_size-1 downto 0);
         address_out : out std_logic_vector(address_size-1 downto 0));
end entity address_calculator;

architecture behavioral of address_calculator is
    signal temp: std_logic_vector(31 downto 0);
begin
    temp <= std_logic_vector(unsigned(a) * 16 + unsigned(b));
    address_out <= temp(15 downto 0);
end architecture behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity mux is
    generic(input_size: integer := 16);
    port(a,b,c,d : in std_logic_vector(input_size-1 downto 0);
         sel : in std_logic_vector(1 downto 0);
         data_out : out std_logic_vector(input_size-1 downto 0));
end entity mux;

architecture behavioral of mux is
begin
    process(a,b,c,d,sel)
    begin
        if sel = "00" then data_out <= a;
        elsif sel = "01" then data_out <= b;
        elsif sel = "10" then data_out <= c;
        elsif sel = "11" then data_out <= d;
        else data_out <= a;
        end if;
    end process;
end architecture behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity alu is
    generic(input_size : integer := 16);
    port(a,b : in std_logic_vector(input_size-1 downto 0);
         op_sel : in std_logic_vector(3 downto 0);
         data_out : out std_logic_vector(input_size-1 downto 0);
         carry_out, zero :  out std_logic);
end entity alu;



architecture Behavioral of alu is
begin
    process(op_sel, a, b)
    variable sum_extended : std_logic_vector(16 downto 0);
    begin
        case op_sel is
            when "0000" =>  -- Addition
                sum_extended := std_logic_vector(unsigned('0' & a) + unsigned('0' & b));
                data_out <= std_logic_vector(signed(a) + signed(b)); 
                carry_out <= sum_extended(15);
                if sum_extended = "0000000000000000" then zero <= '1'; else zero <= '0'; end if;
            when "0001" =>  -- Subtraction
                sum_extended := std_logic_vector(signed(a) - signed(b));
                data_out <= std_logic_vector(signed(a) - signed(b));
                -- zero <= '1' when sum_extended = 0 others => '0';
            when "0010" =>  -- AND
                data_out <= a and b;
            when "0011" =>  -- OR
                data_out <= a or b;
            when "0100" =>  -- XOR
                data_out <= a xor b;
            when others =>  -- Default or undefined operation
                data_out <= (others => 'X');
        end case;
    end process;

end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity TriStateBuffer is
    generic(buffer_size : integer := 16);
    Port (
        data_in  : in  std_logic_vector(buffer_size-1 downto 0);  -- Input data
        enable   : in  std_logic;  -- Enable signal for the buffer
        data_out : out std_logic_vector(buffer_size-1 downto 0)   -- Output data
    );
end TriStateBuffer;

architecture Behavioral of TriStateBuffer is
begin
    process(data_in, enable)
    begin
        if enable = '1' then
            data_out <= data_in;  -- Drive the signal
        else
            data_out <= (others => 'Z');  -- High impedance
        end if;
    end process;
end Behavioral;