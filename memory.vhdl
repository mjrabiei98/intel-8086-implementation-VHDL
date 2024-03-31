
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_TEXTIO.ALL;
use STD.TEXTIO.ALL;
use IEEE.std_logic_arith.all;
-- use IEEE.NUMERIC_STD.ALL;



entity memory is
    generic(DATA_WIDTH : integer := 16; ADDR_WIDTH : integer := 16; instruction_base_address : integer := 0);
    port (clk, rst, write_en : in std_logic; 
         address_in : in std_logic_vector (ADDR_WIDTH-1 downto 0 ); 
         data_in : in std_logic_vector (DATA_WIDTH-1 downto 0) ; 
         data_out : out std_logic_vector (DATA_WIDTH-1 downto 0));
end entity memory;


architecture behavioral of memory is
    -- type memory_array is array (integer range <>) of std_logic_vector(DATA_WIDTH-1 downto 0);
    -- signal mem : memory_array(0 to 2**ADDR_WIDTH-1);

    type memory_array is array (0 to 2**ADDR_WIDTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal mem : memory_array;
    signal init_done : std_logic := '0';


    file input_file : TEXT open READ_MODE is "mem_init.txt"; -- Open the text file for reading

begin

    -- Memory initialization process

    process (clk)
    variable line : LINE;
    variable text_data : std_logic_vector(1 to DATA_WIDTH);
    variable i:integer := instruction_base_address;
    begin
        if init_done = '0' then
            while not endfile(input_file) loop
                readline(input_file, line);
                read(line, text_data);
                -- write(output, line);        -- Write the line to the standard output
                -- write(output, line);
                mem(i) <= (text_data);
                i := i + 1;
            end loop;
            file_close(input_file);
            init_done <= '1';
        end if;
        if (clk'event and clk = '1' and write_en = '1' and init_done = '1') then
            mem(conv_integer(unsigned(address_in))) <= data_in;
            report("write successfull");
        end if;
    end process;

    -- write process
    -- process(clk)
    -- begin
    --     if (clk'event and clk = '1' and write_en = '1') then
    --         mem(conv_integer(unsigned(address_in))) <= data_in;
    --         report("write successfull");
    --     end if;
    -- end process;

    -- read process 
    data_out <= mem(conv_integer(unsigned(address_in)));

end architecture behavioral;