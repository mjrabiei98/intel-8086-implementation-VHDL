library ieee;
use ieee.std_logic_1164.all;

entity register_tester is end entity register_tester;


-- entity memory is
--     generic(DATA_WIDTH : integer := 16; ADDR_WIDTH : integer := 16; instruction_base_address : integer := 0);
--     port (clk, rst, write_en : in std_logic; 
--          address_in : in std_logic_vector (ADDR_WIDTH-1 downto 0 ); 
--          data_in : in std_logic_vector (DATA_WIDTH-1 downto 0) ; 
--          data_out : out std_logic_vector (DATA_WIDTH-1 downto 0));

architecture tb of register_tester is
    signal CLK : std_logic := '0';
    signal RST : std_logic := '0';
    signal write_en : std_logic := '1';
    signal data_in : std_logic_vector(15 downto 0) := (others => '0');
    signal address_in : std_logic_vector(15 downto 0) := (others => '0');
    signal data_out : std_logic_vector(15 downto 0);
begin
    mem1: entity work.memory(behavioral)
        port map(CLK,RST,write_en,address_in,data_in,data_out);
    CLK <=  not CLK after 5 ns when now <= 200 ns else '0';
    process
    begin
        -- rst <= '1';
        wait for 10 ns; RST <= '0';
        wait for 10 ns; address_in <= "0000000011000000";
        wait for 15 ns ; data_in <= "0011001100110011";
        wait for 19 ns; write_en <= '1';
        wait for 10 ns; write_en <= '0';
        wait for 24 ns ; data_in <= "0011000000110000";
        wait for 100 ns ; data_in <= "0000001100000011";
        wait for 19 ns; write_en <= '1';
        wait for 10 ns; write_en <= '0';
        wait for 150 ns ; RST <= '1';
        wait for 500 ns;
        std.env.stop; -- or std.env.stop;
    end process;

end architecture tb;