# VHDL-Pipelining

# MIPS Processor Implementation in VHDL

This project implements a 32-bit MIPS processor using VHDL. It covers all the essential components required for a functional processor, including the Arithmetic Logic Unit (ALU), register file, and control unit. The design has been simulated and verified using ModelSim, with successful synthesis and timing analysis.

## Features

- **ALU**: Performs arithmetic and logical operations.
- **Register File**: Provides 32 general-purpose registers for data storage and retrieval.
- **Control Unit**: Decodes instructions and generates control signals for execution.
- **Instruction Support**: Implements a subset of MIPS instruction set, including arithmetic, logical, memory, and control flow instructions.
- **Simulation and Verification**: Fully verified using ModelSim for functional correctness.
- **Synthesis**: Compatible with FPGA and ASIC design flows, ensuring optimal performance through timing analysis.

## Design Overview

The MIPS processor includes the following components:

1. **Arithmetic Logic Unit (ALU)**:
   - Supports operations such as addition, subtraction, AND, OR, and shifts.
   - Generates flags for zero, carry, and overflow.

2. **Register File**:
   - Provides read and write access to 32 registers.
   - Supports two simultaneous read operations and one write operation.

3. **Control Unit**:
   - Decodes the 32-bit instruction.
   - Generates control signals for ALU operations, memory access, and branching.

4. **Data Path**:
   - Implements the instruction execution pipeline, including fetch, decode, execute, memory access, and write-back stages.

5. **Memory Access**:
   - Interfaces with instruction and data memory for load/store operations.

## Tools Used

- **VHDL**: For hardware description and design.
- **ModelSim**: For simulation and functional verification.
- **Synthesis Tool**: Used for timing analysis and mapping the design to hardware.

## File Structure

- **`mips-rake.vhd`**: VHDL source code for the MIPS processor.
- **Testbenches**: VHDL files for testing and verifying individual modules and the overall processor.

## How to Use

### Simulation

1. Open ModelSim or any compatible simulator.
2. Load the VHDL files into the project.
3. Compile the source and testbench files.
4. Run simulations to verify functionality.

### Synthesis

1. Import the VHDL files into your synthesis tool (e.g., Xilinx Vivado, Quartus).
2. Perform synthesis and timing analysis.
3. Generate a bitstream for FPGA implementation if needed.

## Supported Instructions

This processor implements a subset of the MIPS instruction set, including:

- **Arithmetic**: ADD, SUB, MULT, DIV
- **Logical**: AND, OR, XOR, NOR
- **Shift**: SLL, SRL, SRA
- **Memory Access**: LW, SW
- **Control Flow**: BEQ, BNE, J, JR

## Example Usage

Below is an example test case for an arithmetic operation:

### Input:
Instruction: `ADD R1, R2, R3`  
Register Values:  
- `R2 = 5`  
- `R3 = 3`

### Output:
- `R1 = 8`

## Applications

- Educational use for understanding computer architecture and MIPS processors.
- Prototyping and testing in FPGA-based systems.
- Development of custom processors for embedded systems.

## Future Work

- Extend instruction set to support more advanced operations.
- Add pipelining for improved performance.
- Incorporate hazard detection and forwarding mechanisms.

