library ieee;
use ieee.std_logic_1164.all;

entity testbench is end entity testbench;



architecture tb of testbench is
    signal clk : std_logic := '0';
    signal rst : std_logic := '0';
    signal write_en : std_logic;
    signal address : std_logic_vector(15 downto 0);
    signal mem_data_out : std_logic_vector(15 downto 0);
    signal mem_data_in : std_logic_vector(15 downto 0);
begin
    mem1: entity work.memory(behavioral)
        port map(clk, rst, write_en, 
                 address, 
                 mem_data_in, 
                 mem_data_out);

    processpr_8086: entity work.processor(behavioral)
        port map(clk,rst, address, mem_data_out, write_en, mem_data_in);
    
    clk <=  not clk after 5 ns when now <= 500 ns else '0';

    process
    begin
        wait for 10 ns; rst <= '1';
        wait for 10 ns; rst <= '0';
        wait for 500 ns;
        std.env.stop; -- or std.env.stop;
    end process;

end architecture tb;