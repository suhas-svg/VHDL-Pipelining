------------------------------------------------------------------------------------------------------------
--Name: Maitri Radhesh
--CSU ID: 2831749

--Name: Srinivas Anish Raj, Kesaboyina
--CSU ID: 2828614


--Program compilation		compiles successfully for all the test cases


----Test Prog 1:	
--Outputs:
--CC1 (sw=1000000001)	(IF_instr)  : A44020
--CC2 (sw=0100000001)	(ID_rd1)    : 000084
--CC3 
--CC4 (sw=0001000000) (MEM_AluOut)  : 000088
--CC5
--CC6 (sw=010000001)	   (ID_rd1)    : 000088
--CC7
--CC8 (sw=0001000000) (MEM_AluOut)  : 00008C	
--TEST CASE PASSED

-----------------------------------------------------

----Test Prog 2: 
--Outputs:
--CC1 to CC6  (sw=1000000000) (PC) Starts with 0, 4, 40, 44, 48, 4C
--CC7 to CC19 (sw=1000000000) (PC) 2C, 30, 34, 38, 0C, 10, 14, 18, 1C, 20, 24, 28, 5C
--TEST CASE PASSED

-----------------------------------------------------

----Test Prog 3:
--Outputs: 
--CC1 to CC3 
--CC4 (sw=0001000001) (MEM_MEMOut)  : 001010 
--CC4 (sw=0001000000) (MEM_ALUOut)  : 000084
--CC5
--CC6 (sw=0001000000) (MEM_ALUOut)  : 00008C
--CC7 to CC9 
--CC10 (sw=0001000000) (MEM_ALUOut)  : 000084
--CC11 to CC12
--CC13 (sw=0001000000) (MEM_ALUOut)  : 00109C

--TEST CASE PASSED
-----------------------------------------------------

-- Troubles faced:
-- 1. Missed the change from 'instruction_mips' to 'ID_instr'
-- 2. Had missed the initialization of 'EX_funct' in the Execution stage
-- 3. The new update of the 'reset_mips' process   

-- Troubles rectifiied: 
-- 1. The pipelining logic of the professor helped us understand the change from 'instruction_mips' to 'ID_instr'
-- 2. We realised that we had forgot to include the initialization of 'EX_funct' in the Execution stage as we required 
--    the value of 'EX_funct' in the calculation of 'AluCtl'
-- 3. The updated value of the 'reset_mips' process solved the trouble of test program 3 where we were 
--    the control signal values were not getting reset correctly.

-- Implementation of MIPS
-- File name   : mips.vhd
--
------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
  
entity mips is
    port (clock_mips	  : in  std_logic;
          reset_mips     : in  std_logic;
          memory_in_mips       : out std_logic_vector (31 downto 0);	-- data to write into memory (sw)
          memory_out_mips      : in  std_logic_vector (31 downto 0); -- data being read from memory (lw)
          memory_address_mips  : out std_logic_vector (31 downto 0);	-- memory address to read/write
          pc_mips              : out std_logic_vector (31 downto 0);	-- instruction address to fetch
          instruction_mips     : in std_logic_vector (31 downto 0);  -- instruction data to execute in the next cycle
          overflow_mips			   : out std_logic;  -- flag for overflow
          invalid_mips         : out std_logic;  -- flag for invalid opcode
			 memory_write_mips    : out std_logic;                     -- control signal for data memory read/write
			 
			 -- ***DEBUG*** 
			 -- IF
			 ifpc4_mips: out std_logic_vector (31 downto 0);
			 -- ID
			 idrt_mips, idrd_mips: out std_logic_vector (4 downto 0);
			 idrd1_mips, idrd2_mips, idpc4_mips: out std_logic_vector (31 downto 0);
			 idcontrol_mips: out std_logic_vector (9 downto 0);
			 -- EX
			 --forwardA_mips, forwardB_mips: out std_logic_vector (1 downto 0);
			 exbtgt_mips, 
			 exaluout_mips, exrd2_mips: out std_logic_vector (31 downto 0);
			 exregrd_mips: out std_logic_vector (4 downto 0);
			 excontrol_mips: out std_logic_vector (9 downto 0);
			 -- MEM
			 memmemout_mips, memaluout_mips, memrd2_mips: out std_logic_vector (31 downto 0);
			 memregrd_mips: out std_logic_vector (4 downto 0);
			 memcontrol_mips: out std_logic_vector (9 downto 0);
			 -- WB
			 wbmemout_mips, wbaluout_mips: out std_logic_vector (31 downto 0);
			 wbregrd_mips: out std_logic_vector (4 downto 0);
			 wbcontrol_mips: out std_logic_vector (9 downto 0));
end entity;

architecture rtl of mips is

------ Register file ---
   type register_type is array (0 to 31) of std_logic_vector(31 downto 0);
   signal Registers : register_type := (
   		0 => x"00000000", 
			4 => x"00000004",
			5 => x"00000084",
			6 => x"0000008c",
			7 => x"00000001",
			others => x"00000000");
			
------ Pipeline registers ---
	signal MEMWB_memout, MEMWB_ALUOut: std_logic_vector(31 downto 0); 
	signal MEMWB_RegRd: std_logic_vector(4 downto 0); 
	signal MEMWB_RegWrite, MEMWB_MemtoReg: std_logic;  
	
	signal EXMEM_btgt, EXMEM_ALUOut, EXMEM_rd2: std_logic_vector(31 downto 0); --pipereg32_type;
	signal EXMEM_Zero: std_logic; 
	signal EXMEM_RegRd: std_logic_vector(4 downto 0); 
	signal EXMEM_RegWrite, EXMEM_MemWrite, EXMEM_MemtoReg, EXMEM_Branch, EXMEM_BranchNE: std_logic;

	signal IDEX_pc4, IDEX_rd1, IDEX_rd2, IDEX_extend: std_logic_vector(31 downto 0); 
	signal IDEX_rt, IDEX_rd: std_logic_vector(4 downto 0); 
	signal IDEX_RegDst, IDEX_RegWrite, IDEX_MemWrite, IDEX_ALUSrc, IDEX_MemtoReg, IDEX_Branch, IDEX_BranchNE: std_logic;  
	signal IDEX_ALUOp: std_logic_vector(1 downto 0); 
	-- FORWARDING LOGIC
	signal IDEX_rs: std_logic_vector(4 downto 0);
   -- HAZARD DETECTION
	signal IDEX_MemRead: std_logic;
	
	signal IFID_pc4, IFID_instr: std_logic_vector(31 downto 0); 

	
------ Signal Declaration ------
	 -- WB
	 signal WB_memout, WB_wd, WB_ALUOut: std_logic_vector(31 downto 0); 
	 signal WB_wn: std_logic_vector (4 downto 0);  -- (WB_RegRd == WB_wn)
	 signal WB_MemtoReg, WB_RegWrite: std_logic;
	 -- MEM
	 signal MEM_ALUOut, MEM_rd2, MEM_btgt: std_logic_vector (31 downto 0);  -- ALU output value
	 signal MEM_RegRd: std_logic_vector (4 downto 0);  
	 signal MEM_Zero, MEM_RegWrite, MEM_MemWrite, MEM_MemtoReg, MEM_Branch, MEM_BranchNE, MEM_PCSrc: std_logic;
	 -- EX
	 signal alu1, alu2: std_logic_vector (31 downto 0);   -- ALU input values
    signal EX_ALUOut, EX_extend, EX_offset, EX_rd1, EX_rd2, EX_pc4, EX_btgt: std_logic_vector (31 downto 0);  -- ALU output value
	 signal ID_funct, EX_funct: std_logic_vector(5 downto 0);
    signal ALUctl	: std_logic_vector (3 downto 0);  -- ALU output value
	 signal EX_ALUOp	: std_logic_vector (1 downto 0);
	 signal overflow, EX_Zero: std_logic;       -- 1 bit signals for ALU arithmetic operations
	 signal EX_rt, EX_rd, EX_RegRd: std_logic_vector (4 downto 0);  -- rs, rt, rd
	 signal EX_RegDst, EX_RegWrite, EX_MemWrite, EX_ALUSrc, EX_MemtoReg, EX_Branch, EX_BranchNE, EX_PCSrc: std_logic;
	 -- FORWARDING LOGIC
	 signal alu2temp: std_logic_vector(31 downto 0);
	 signal EX_rs: std_logic_vector(4 downto 0);
	 signal forwardA, forwardB: std_logic_vector(1 downto 0);
	 -- HAZARD DETECTION UNIT
	 signal EX_MemRead: std_logic;

	 -- ID
	 signal ID_op: std_logic_vector(5 downto 0);
	 -- NOT NEEDED -- signal ID_funct: std_logic_vector(5 downto 0);
	 signal ID_immed: std_logic_vector(15 downto 0);
	 signal ID_instr, ID_extend, ID_rd1, ID_rd2, ID_pc4: std_logic_vector(31 downto 0);
	 signal ID_rs, ID_rt, ID_rd: std_logic_vector (4 downto 0);  -- rs, rt, rd
	 signal ID_RegDst, ID_RegWrite, ID_MemWrite, ID_ALUSrc, ID_MemtoReg, ID_Branch, ID_BranchNE, ID_Jump: std_logic;
    signal ID_ALUOp: std_logic_vector (1 downto 0);
    signal jumpaddress	: std_logic_vector (31 downto 0);
	 -- HAZARD DETECTION UNIT
	 signal IFIDWrite, PCWrite, NullControl, ID_MemRead: std_logic;
	 -- IF
	 signal IF_pc4, IF_pc_next: std_logic_vector (31 downto 0);     -- instruction address to fetch
	
------ Component Declarations ------
component ALU_32
	port(
		A_alu32 : in std_logic_vector(31 downto 0);  	 	-- A input
		B_alu32 : in std_logic_vector(31 downto 0);  	 	-- B input
		ALUctl_alu32 : in std_logic_vector(3 downto 0);  	-- control
		ALUout_alu32 : out std_logic_vector(31 downto 0);  -- result
		overflow_alu32: out std_logic; -- overflow result
		Zero_alu32 : out std_logic);  								-- check if ALUout is zero
end component ;


begin

----------------------------------------------------
--synchornous write to registers (WB stage)
		
		-- **RETRIVE FROM PIPELINE REGISTERS 
		WB_memout <= MEMWB_memout;
		WB_ALUOut <= MEMWB_ALUOut;
		WB_wn <= MEMWB_RegRd;
		WB_RegWrite <= MEMWB_RegWrite;
		WB_MemtoReg <= MEMWB_MemtoReg;
		
		----------------------------------------------------------------
    -- your code (WB stage)
	   with WB_MemtoReg select
			WB_wd<= WB_memout when '0', WB_ALUOut when others; ----- CORRECTED*****

    ----------------------------------------------------------------

      process(CLOCK_mips, reset_mips) begin
			-- Multiplexor Controlling Write Register Input
			
			 if(reset_mips = '1') then
             Registers(0) <= x"00000000";
             Registers(1) <= x"00000000";
             Registers(2) <= x"00000000";
             Registers(3) <= x"00000000";
             Registers(4) <= x"00000004";
             Registers(5) <= x"00000084";
             Registers(6) <= x"0000008c";
             Registers(7) <= x"00000001";
             Registers(8) <= x"00000000";
             Registers(8) <= x"00000000";
             Registers(9) <= x"00000000";
             Registers(10) <= x"00000000";
             Registers(11) <= x"00000000";
             Registers(12) <= x"00000000";
             Registers(13) <= x"00000000";
             Registers(14) <= x"00000000";
             Registers(15) <= x"00000000";
             Registers(16) <= x"00000000";
             Registers(17) <= x"00000000";
             Registers(18) <= x"00000000";
             Registers(18) <= x"00000000";
             Registers(19) <= x"00000000"; 	
				 
         elsif (FALLING_EDGE(clock_mips)) then
            if (WB_RegWrite = '1') then
                  Registers(to_integer(unsigned(WB_wn)))<= WB_wd;
            end if;
         end if;
      end process;

		-- ******* FOR DEBUG
		wbmemout_mips <= WB_memout;
		wbaluout_mips <= WB_ALUOut;
		wbregrd_mips <= WB_wn;
		
		wbcontrol_mips(9) <= '0'; --EX_RegDst;
		wbcontrol_mips(8) <= '0'; --EX_ALUSrc;
		wbcontrol_mips(7) <= WB_MemtoReg;
		wbcontrol_mips(6) <= WB_RegWrite;
		wbcontrol_mips(5) <= '0'; --MEM_MemRead;
		wbcontrol_mips(4) <= '0'; --MEM_MemWrite;
		wbcontrol_mips(3) <= '0'; --MEM_Branch;
		wbcontrol_mips(2 downto 1) <= "00"; --EX_ALUOp;
		wbcontrol_mips(0) <= '0'; --ID_Jump;

----------------------------------------------------
--Memory interfacing (MEM stage) ----	

	-- **RETRIVE FROM PIPELINE REGISTERS 
	MEM_btgt <= EXMEM_btgt;				-- data
	MEM_ALUOut <= EXMEM_ALUOut;
	MEM_rd2 <= EXMEM_rd2;
	MEM_Zero <= EXMEM_Zero;
	MEM_RegRd <= EXMEM_RegRd;
	MEM_RegWrite <= EXMEM_RegWrite;	-- control
	MEM_MemWrite <= EXMEM_MemWrite; 
	MEM_MemtoReg <= EXMEM_MemtoReg;
	MEM_Branch <= EXMEM_Branch;
	MEM_BranchNE <= EXMEM_BranchNE;

  ----------------------------------------------------------------
  -- your code (MEM stage)
  
  MEM_PCSrc <= (MEM_Branch and MEM_Zero);
  
  memory_in_mips <= MEM_rd2; -- Data from register to be written to Data Memory
  memory_address_mips <= MEM_ALUOut; -- Address of data memory
  memory_write_mips <= MEM_MemWrite; -- Controls if data should be written to data memory
  ----------------------------------------------------------------

	-- **** SAVE TO PIPELINE REGISTERS	
      process(CLOCK_mips) begin
         if (RISING_EDGE(clock_mips)) then
				MEMWB_memout <= memory_out_mips;
				MEMWB_ALUOut <= MEM_ALUOut;
				MEMWB_RegRd <= MEM_RegRd;
				MEMWB_RegWrite <= MEM_RegWrite;
				MEMWB_MemtoReg <= MEM_MemtoReg;
         end if;
      end process;

		-- ******* FOR DEBUG	
		memmemout_mips <= memory_out_mips;
		memaluout_mips <= MEM_ALUOut;
		memrd2_mips <= MEM_rd2;
		memregrd_mips <= MEM_RegRd;

		memcontrol_mips(9) <= '0'; --EX_RegDst;
		memcontrol_mips(8) <= '0'; --EX_ALUSrc;
		memcontrol_mips(7) <= MEM_MemtoReg;
		memcontrol_mips(6) <= MEM_RegWrite;
		memcontrol_mips(5) <= MEM_MemWrite;
		memcontrol_mips(4) <= '0'; --MEM_MemRead NOT USED
		memcontrol_mips(3) <= MEM_Branch;
		memcontrol_mips(2 downto 1) <= "00"; --EX_ALUOp;
		memcontrol_mips(0) <= '0'; --ID_Jump;
		
----------------------------------------------------
--ALU interfacing (EX stage)

	-- **RETRIVE FROM PIPELINE REGISTERS 
	EX_rd1 <= IDEX_rd1;
	EX_rd2 <= IDEX_rd2;
	EX_rt <= IDEX_rt;
	EX_rd <= IDEX_rd;
	EX_pc4 <= IDEX_pc4;
	EX_pc4 <= IDEX_pc4;
	EX_extend <= IDEX_extend;
	EX_RegDst <= IDEX_RegDst;
	EX_ALUSrc <= IDEX_ALUSrc;
	EX_ALUOp <= IDEX_ALUOp;
	EX_RegWrite <= IDEX_RegWrite;
	EX_MemWrite <= IDEX_MemWrite;
	EX_MemtoReg <= IDEX_MemtoReg;
	EX_Branch <= IDEX_Branch;
	EX_BranchNE <= IDEX_BranchNE;
	-- FORWARDING LOGIC
	EX_rs <= IDEX_rs;
   -- HAZARD DETECTION UNIT
	EX_MemRead <= IDEX_MemRead;

  ----------------------------------------------------------------
	-- your code (EX stage)
	
	EX_funct <= EX_extend(5 downto 0);

	--ALU Control
	ALUctl(3) <= '0'; 							-- Ainvert
	ALUctl(2) <= (EX_ALUOp(1) and (not EX_ALUOp(0)) and EX_funct(1)) or (EX_ALUOp(0)); -- Binvert
	ALUctl(1) <= (EX_ALUop(1) and (not EX_funct(2))) or (not EX_ALUop(1));
	ALUctl(0) <= EX_ALUOp(1) and (EX_funct(3) or ((not EX_funct(1)) and EX_funct(0)));	

	--ALU Input
	alu1<= EX_rd1;
	with EX_ALUSrc select
		alu2<= EX_extend when '1', EX_rd2 when others;
	
	--Register Destination
	with EX_RegDst select
		EX_RegRd <= EX_rd when '1', EX_rt when others;
	
	--BTGT		
	EX_offset(31 downto 2) <= EX_extend(29 downto 0);
	EX_offset(1 downto 0) <= "00";
	EX_btgt <= EX_pc4 + EX_offset;
	
	

----------------------------------------------------------------

	-- The component "ALU_32" produces output (ALUout, overflow, Zero)
	------ Component Instantiations ------
   ALU_32_1 : ALU_32
      port map ( A_alu32       => alu1,
                 B_alu32       => alu2,
                 ALUctl_alu32  => ALUctl,
				     ALUout_alu32=> EX_ALUout,
					  overflow_alu32=>overflow,
					  Zero_alu32=>EX_Zero);

	-- **** SAVE TO PIPELINE REGISTERS
      process(CLOCK_mips) begin
         if (RISING_EDGE(clock_mips)) then
				EXMEM_btgt <= EX_btgt;
				EXMEM_ALUOut <= EX_ALUOut;
				EXMEM_rd2 <= EX_rd2;
				EXMEM_Zero <= EX_Zero;
				EXMEM_RegRd <= EX_RegRd;
				EXMEM_RegWrite <= EX_RegWrite;
				EXMEM_MemWrite <= EX_MemWrite;
				EXMEM_MemtoReg <= EX_MemtoReg;
				EXMEM_Branch <= EX_Branch;
				EXMEM_BranchNE <= EX_BranchNE;
         end if;
      end process;
		
		-- ******* FOR DEBUG
		exbtgt_mips <= EX_btgt;
		--forwardA_mips <= forwardA;
		--forwardB_mips <= forwardB;
		exaluout_mips <= EX_ALUOut;
		exrd2_mips <= EX_rd2;
		exregrd_mips <= EX_RegRd;
		
		excontrol_mips(9) <= EX_RegDst;
		excontrol_mips(8) <= EX_ALUSrc;
		excontrol_mips(7) <= EX_MemtoReg;
		excontrol_mips(6) <= EX_RegWrite;
		excontrol_mips(5) <= EX_MemWrite;
		excontrol_mips(4) <= '0'; --EX_MemRead NOT USED
		excontrol_mips(3) <= EX_Branch;
		excontrol_mips(2 downto 1) <= EX_ALUOp;
		excontrol_mips(0) <= '0'; --ID_Jump;
			
----------------------------------------------------
--inst analysis (ID stage)

	-- **RETRIVE FROM PIPELINE REGISTERS 
	ID_pc4 <= IFID_pc4;
	ID_instr <= IFID_instr;

  ----------------------------------------------------------------
  -- your code (ID stage)
  
   ID_op <= ID_instr(31 downto 26);
	ID_rs <= ID_instr(25 downto 21);
	ID_rt <= ID_instr(20 downto 16);
	ID_rd <= ID_instr(15 downto 11);
	
--sign bit	
	ID_immed <= ID_instr(15 downto 0);
	ID_extend(15 downto 0) <= ID_immed;
	with ID_instr(15) select
	ID_extend(31 downto 16) <= "1111111111111111" when '1',
									"0000000000000000" when others;
									
--asynchornous read from registers 
	ID_rd1 <= Registers((to_integer(unsigned(ID_rs))));
	ID_rd2 <= Registers((to_integer(unsigned(ID_rt))));
									
	jumpaddress(31 downto 28) <= ID_pc4(31 downto 28);
	jumpaddress(27 downto 2) <= ID_instr(25 downto 0);
	jumpaddress(1 downto 0) <= "00";
									
	-- Control Signals
		ID_RegDst 	<=  not ID_op(5); 	-- Register Destination
		ID_ALUSrc 	<= ID_op(1);			-- ALU Source
		ID_MemtoReg <= (not ID_op(0));			-- Data to be written to Register ----- CORRECTED*****--
--		ID_RegWrite <= ((not ID_op(2)) and (not ID_op(1))) or (ID_op(5) and (not ID_op(3))); -- Controls if data should be written to Register
--		ID_MemWrite <= ID_op(3);			-- Controls if data should be written to memory
		ID_Branch <= ID_op(2) and (not ID_op(0));
		ID_Jump <= ID_op(1) and (not ID_op(0));
		
		--Signal to control ALU control signals
		ID_ALUOp(1) <= not ID_op(5) and (not ID_op(2)); 
		ID_ALUOp(0) <= ID_op(2);
  
		process(reset_mips) begin
			if (reset_mips = '1') then 
				ID_RegWrite <= '0';
				ID_MemWrite <= '0';
			else
				ID_RegWrite <= ((not ID_op(2)) and (not ID_op(1))) or (ID_op(5) and (not ID_op(3))); -- Controls if data should be written to Register
				ID_MemWrite <= ID_op(3);			-- Controls if data should be written to memory
			end if;
		end process;

  ----------------------------------------------------------------

	-- **** SAVE TO PIPELINE REGISTERS	
      process(CLOCK_mips) begin	
         if (RISING_EDGE(clock_mips)) then
				IDEX_pc4 <= ID_pc4;
				IDEX_rd1 <= ID_rd1;
				IDEX_rd2 <= ID_rd2;
				IDEX_extend <= ID_extend;
				IDEX_rt <= ID_rt;
				IDEX_rd <= ID_rd;
				IDEX_RegDst <= ID_RegDst; --and (not NullControl);
				IDEX_RegWrite <= ID_RegWrite; -- and (not NullControl);
				IDEX_MemWrite <= ID_MemWrite; -- and (not NullControl);
				IDEX_ALUSrc <= ID_ALUSrc; -- and (not NullControl);
				IDEX_MemtoReg <= ID_memtoReg; -- and (not NullControl);
				IDEX_Branch <= ID_Branch; -- and (not NullControl);
				IDEX_BranchNE <= ID_BranchNE; -- and (not NullControl);
				IDEX_ALUOp <= ID_ALUOp;
				-- FORWARDING LOGIC
				IDEX_rs <= ID_rs;
				-- HAZARD DETECTION
				IDEX_MemRead <= ID_MemRead; -- and (not NullControl);
         end if;
      end process;

		-- ******* FOR DEBUG	
		idrt_mips <= ID_rt;
		idrd_mips <= ID_rd;
		idrd1_mips <= ID_rd1;
		idrd2_mips <= ID_rd2;
		idpc4_mips <= ID_pc4;
		
		idcontrol_mips(9) <= ID_RegDst;
		idcontrol_mips(8) <= ID_ALUSrc;
		idcontrol_mips(7) <= ID_MemtoReg;
		idcontrol_mips(6) <= ID_RegWrite;
		idcontrol_mips(5) <= ID_MemWrite;
		idcontrol_mips(4) <= '0'; --ID_MemRead NOT USED
		idcontrol_mips(3) <= ID_Branch;
		idcontrol_mips(2 downto 1) <= ID_ALUOp;
		idcontrol_mips(0) <= ID_Jump;

----------------------------------------------------  
-- Inst memory & PC update (IF stage) 

	-- pc updates in MEMORY stage (because Zero, btgt, and jumpaddress are computed in EX stage)
  pc_mips <= IF_pc_next;
  IF_pc4 <= IF_pc_next + 4; 
  
  process (CLOCK_mips, reset_mips) 
  begin
       if(reset_mips = '1') then
             IF_pc_next <= x"00000000";
							 
			 elsif(RISING_EDGE(CLOCK_mips)) then
					if (ID_Jump = '1') then
						IF_pc_next <= jumpaddress;
					elsif (MEM_PCSrc = '1') then
						IF_pc_next <= MEM_btgt;
					else 											
						IF_pc_next <= IF_pc4;              -- instruction memory address (to fetch) increases by 4
					end if;
			 else 
					IF_pc_next <= IF_pc_next;
       end if;
     end process;

	-- **** SAVE TO PIPELINE REGISTERS	  
      process(CLOCK_mips) begin						
         if (RISING_EDGE(clock_mips)) then
					IFID_pc4 <= IF_pc4;
					IFID_instr <= instruction_mips;
        end if;
      end process;
		-- The component "inst_memory_128B" produces output (instruction)

		-- ******* FOR DEBUG
		ifpc4_mips <= IF_pc4;

end architecture;
