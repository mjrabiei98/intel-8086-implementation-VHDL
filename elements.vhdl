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



entity memory is
    generic(DATA_WIDTH : integer := 16; ADDR_WIDTH : integer := 16; instruction_base_address : integer := 0);
    port (clk, rst, write_en : in std_logic; 
         address_in : in std_logic_vector (ADDR_WIDTH-1 downto 0 ); 
         data_in : in std_logic_vector (DATA_WIDTH-1 downto 0) ; 
         data_out : out std_logic_vector (DATA_WIDTH-1 downto 0));
end entity memory;


architecture behavioral of memory is
    type memory_array is array (integer range <>) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal mem : memory_array(0 to 2**ADDR_WIDTH-1);

    file input_file : TEXT open READ_MODE is "mem_init.txt"; -- Open the text file for reading

begin

    -- Memory initialization process

    process
    variable line : LINE;
    variable text_data : std_logic_vector(1 to DATA_WIDTH);
    variable i:integer := instruction_base_address;
    begin
        while not endfile(input_file) loop
            readline(input_file, line);
            read(line, text_data);
            mem(i) <= text_data;
            i := i + 1;
        end loop;
        file_close(input_file);
        wait;
    end process;

    -- write process
    process(clk)
    begin
        if (clk'event and clk = '1' and write_en = '1') then
            mem(conv_integer(unsigned(address_in))) <= data_in;
        end if;
    end process;

    -- read process 
    data_out <= mem(conv_integer(unsigned(address_in)));

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
         data_out : out std_logic_vector(7 downto 0));
end entity queue;

architecture behavioral of queue is
    type queue_type is array (0 to 5) of STD_LOGIC_VECTOR(7 downto 0);
    signal queue : queue_type;
    signal head, tail : integer range 0 to 6 := 0;
begin
    process(clk, rst)
    begin
        if rst = '1' then
            head <= 0;
            tail <= 0;
            data_out <= "00000000";
            -- queue := (others => '0');

        elsif (clk'event and clk = '1') then
            if (head < 5) then 
                if push = '1' then
                    queue(tail mod 6) <= data_in;
                    tail <= tail + 1;
                end if;
            end if;
            if pop = '1' then
                data_out <= queue(head mod 6);
                head <= head + 1;
            end if;
        end if;
    end process;

    full <= '1' when (tail - head) > 4 else '0';
    empty <= '1' when (tail - head) < 4 else '0';

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
    process
    begin
        data_out <= std_logic_vector(unsigned(data_in) + 1);
    end process;
end architecture behavioral;





library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- entity X_registers




library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- entity address_calculator


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- entity ALU


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- entity mux


