library ieee;
use ieee.std_logic_1164.all;

entity register_tester is end entity register_tester;

architecture tb of register_tester is
    signal CLK : std_logic := '0';
    signal RST : std_logic;
    signal en : std_logic := '1';
    signal data_in : std_logic_vector(15 downto 0) := (others => '0');
    signal data_out : std_logic_vector(15 downto 0);
begin
    reg1: entity work.reg(behavioral)
        generic map(16)
        port map(CLK,RST,en,data_in,data_out);
    CLK <=  not CLK after 5 ns when now <= 200 ns else '0';
    process
    begin
        rst <= '1';
        wait for 10 ns; rst <= '0';
        wait for 15 ns ; data_in <= "0011001100110011";
        wait for 24 ns ; data_in <= "0011000000110000";
        wait for 100 ns ; data_in <= "0000001100000011";
        wait for 150 ns ; RST <= '1';
        wait for 500 ns;
        std.env.stop; -- or std.env.stop;
    end process;

end architecture tb;