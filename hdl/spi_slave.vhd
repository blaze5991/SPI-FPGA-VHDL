--@Author: Georgios Kleitsiotis
--@Forked_by:nematoli
--@LICENCE: National Technical University of Athens - Nanoelectronics Group
--@Version: v1.0
--@Date: 11-12-2022(dd-mm-yyyy)
--@port: reset_n --> Reser signal (active low)
--	 cpol	 --> Clock polarity
--	 cpha	 --> Clock phase
--	 sclk 	 --> SPI clock
--	 ss_n	 --> Slave select (active low)
--	 MOSI	 --> Master Out Slave IN
--	 MISO	 --> Master In Slave Out
--  rx_enable	 --> Convert received data to parallel
--	   tx	 --> Data to be tranmsited
-- 	   rx	 --> Data received (if rx_enable = '1')
--	 busy	 --> Slave busy (if high)
-- This is an ammended version of nematoli's spi_slave original code. The project is still work-in-progress. Please provide feedback @ gklitsiotis@mail.ntua.gr
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
--this is comment
ENTITY spi_slave IS
  GENERIC(
    data_length : INTEGER := 8);     --data length in bits
  PORT(
    reset_n      : IN     STD_LOGIC;  																	 --asynchronous active low reset
	 cpol    	  : IN 	  STD_LOGIC;  																	 --clock polarity mode
    cpha    	  : IN 	  STD_LOGIC;  																	 --clock phase mode
    sclk         : IN     STD_LOGIC;  																	 --spi clk
	 ss_n         : IN     STD_LOGIC;  																	 --slave select
    mosi         : IN     std_logic_vector(0 downto 0);  																	 --master out slave in
    miso         : OUT    STD_LOGIC;  																	 --master in slave out
	 rx_enable    : IN     STD_LOGIC;  																	 --enable signal to wire rxBuffer to outside 
    tx			  : IN     STD_LOGIC_VECTOR(data_length-1 DOWNTO 0);  						 --data to transmit
    rx		     : OUT    STD_LOGIC_VECTOR(data_length-1 DOWNTO 0) := (OTHERS => '0');  --data received
    busy         : OUT    STD_LOGIC := '0');  														 --slave busy signal	 
END spi_slave;

ARCHITECTURE behavioural OF spi_slave IS
  SIGNAL mode    : STD_LOGIC;  																	  --according to CPOL and CPHA
  SIGNAL clk     : STD_LOGIC;  
  SIGNAL bit_counter : STD_LOGIC_VECTOR(data_length DOWNTO 0) := (others => '0'); 						  --active bit indicator
  SIGNAL rxBuffer  : STD_LOGIC_VECTOR(data_length-1 DOWNTO 0) := (OTHERS => '0');  --receiver buffer
  SIGNAL txBuffer  : STD_LOGIC_VECTOR(data_length-1 DOWNTO 0) := (OTHERS => '0');  --transmit buffer
BEGIN
  busy <= NOT ss_n;  

  mode <= cpol XOR cpha;  

  PROCESS (cpol, ss_n, sclk)
  BEGIN
  IF(ss_n = '1') then
     clk <= '0';
  ELSE
     IF (cpol = '0') then 
	     clk <= sclk;
	  ELSE
	     clk <= NOT sclk;
	  END IF;
  END IF;
  END PROCESS;

  --where is the active bit
  PROCESS(ss_n, clk)
  BEGIN
    IF(ss_n = '1' OR reset_n = '0') THEN                         
	   bit_counter <= (others => '0');
            if cpha = '1' then
            bit_counter(0) <= '1';
            else
            bit_counter(1) <= '1';
            end if; --reset active bit binary_read 
    ELSE                                                         
      IF(rising_edge(clk)) THEN                                  
        bit_counter <= bit_counter(data_length-1 DOWNTO 0) & '0';    --left shift active bit indicator
      END IF;
    END IF;
  END PROCESS;


  PROCESS(ss_n, clk, rx_enable, reset_n)
  BEGIN      

	 --receive mosi bit
    IF(cpha = '0' and cpol = '0') then
		 IF(reset_n = '0') THEN			--reset the buffer
			rxBuffer <= (OTHERS => '0');
		 ELSIF(rising_edge(clk)) THEN
			rxBuffer(data_length-1 DOWNTO 0) <= rxBuffer(data_length-2 DOWNTO 0) & mosi;  --shift in the received bit
		 END IF;
	 ELSIF( cpha = '1' and cpol = '0') THEN
		 IF (reset_n = '0') THEN       --reset the buffer
			rxBuffer <= (OTHERS => '0');
		 ELSIF(falling_edge(clk)) THEN
			rxBuffer(data_length-1 DOWNTO 0) <= rxBuffer(data_length-2 DOWNTO 0) & mosi;  --shift in the received bit
		 END IF;
    elsif (cpha = '0' and cpol = '1')then
         IF (reset_n = '0') THEN       --reset the buffer
			rxBuffer <= (OTHERS => '0');
		 ELSIF(falling_edge(clk)) THEN
			rxBuffer(data_length-1 DOWNTO 0) <= rxBuffer(data_length-2 DOWNTO 0) & mosi;  --shift in the received bit
		 END IF;
	elsif (cpha = '1' and cpol = '1') then
	     IF (reset_n = '0') THEN       --reset the buffer
			rxBuffer <= (OTHERS => '0');
		 ELSIF(rising_edge(clk)) THEN
			rxBuffer(data_length-1 DOWNTO 0) <= rxBuffer(data_length-2 DOWNTO 0) & mosi;  --shift in the received bit
		 END IF; 
	 END IF;

    --if user wants the received data output it
    IF(reset_n = '0') THEN
      rx <= (OTHERS => '0');
    ELSIF(ss_n = '1' AND rx_enable = '1') THEN  
      rx <= rxBuffer;
    END IF;

    --transmit registers
    IF(reset_n = '0') THEN
      txBuffer <= (OTHERS => '0');
    ELSIF(ss_n = '1') THEN  
      txBuffer <= tx;
    ELSIF(bit_counter(data_length) = '0' AND rising_edge(clk)) THEN
      txBuffer(data_length-1 DOWNTO 0) <= txBuffer(data_length-2 DOWNTO 0) & txBuffer(data_length-1);  --shift through tx data
    END IF;

    --transmit miso bit
    IF(ss_n = '1' OR reset_n = '0') THEN           
      miso <= 'Z';
    ELSIF(rising_edge(clk)) THEN
      miso <= txBuffer(data_length-1);               
    END IF;

  END PROCESS;
END behavioural;
