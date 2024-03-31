library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;



-- entity reg is 
--     generic (register_size : integer := 8);
--     port (clk, rst, en: in std_logic; d : in std_logic_vector (register_size - 1 downto 0); q : out std_logic_vector (register_size-1 downto 0 ) );
-- end entity reg;

-- entity TriStateBuffer is
--     generic(buffer_size : integer := 16);
--     Port (
--         data_in  : in  std_logic_vector(buffer_size-1 downto 0);  -- Input data
--         enable   : in  std_logic;  -- Enable signal for the buffer
--         data_out : out std_logic_vector(buffer_size-1 downto 0)   -- Output data
--     );

entity datapath is 
    port (clk, rst, en_ES, ES_to_bus : in std_logic);
end entity datapath;

architecture bwhavioral of datapath is

    signal 

begin

    ES : entity work.reg(behavioral)
        generic map (16)
        port map(clk, rst, en_ES, );

    ES_triState : entity work.reg(behavioral)
        generic map (16)
        port map( );

        


end bwhavioral ; -- bwhavioralsab