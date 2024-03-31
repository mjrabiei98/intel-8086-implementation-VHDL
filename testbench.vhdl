library ieee;
use ieee.std_logic_1164.all;

entity register_tester is end entity register_tester;


-- entity queue is
--     port(clk, rst, push, pop : in std_logic; 
--          data_in : in std_logic_vector (15 downto 0); 
--          full : out std_logic;
--          empty : out std_logic;
--          data_out : out std_logic_vector(7 downto 0));
-- end entity queue;

architecture tb of register_tester is
    signal CLK : std_logic := '0';
    signal RST : std_logic := '0';
    signal write_en : std_logic := '1';
    signal push, pop : std_logic := '0';
    signal data_in : std_logic_vector(15 downto 0) := (others => '0');
    signal full, empty : std_logic;
    signal data_out : std_logic_vector(7 downto 0);
begin
    mem1: entity work.queue(behavioral)
        port map(CLK,RST,push,pop,data_in,full,empty,data_out);
    CLK <=  not CLK after 5 ns when now <= 300 ns else '0';
    process
    begin
        -- rst <= '1';
        wait for 10 ns; RST <= '0';
        wait for 15 ns ; data_in <= "0011001100110011";
        wait for 19 ns; push <= '1';
        wait for 10 ns; push <= '0';
        wait for 24 ns ; data_in <= "0011000000110000";
        wait for 24 ns ; push <= '1';
        wait for 24 ns ; push <= '0';
        wait for 100 ns ; data_in <= "0000001100000011";
        wait for 19 ns; pop <= '1';
        wait for 10 ns; pop <= '0';
        wait for 150 ns ; RST <= '1';
        wait for 500 ns;
        std.env.stop; -- or std.env.stop;
    end process;

end architecture tb;