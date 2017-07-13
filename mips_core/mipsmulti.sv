//-------------------------------------------------------
// mipsmulti.v
// David_Harris@hmc.edu 8 November 2005
// Update to SystemVerilog 17 Nov 2010 DMH
// Multicycle MIPS processor
//------------------------------------------------

module mips(input  logic        clk, reset, irq,
            output logic [31:0] adr, writedata,
            output logic        memwrite, iack,
            input  logic [31:0] readdata);

  logic        zero, overflow, pcen, epcwrite, causewrite, intcause, irwrite, regwrite,
               alusrca, iord, regdst, jal;
  logic [1:0]  memtoreg;
  logic [2:0]  pcsrc, alusrcb, alucontrol;
  logic [5:0]  op, funct;

  controller c(clk, reset, op, funct, zero, overflow,
               pcen, epcwrite, causewrite, intcause, memwrite, irwrite, regwrite,
               alusrca, iord, memtoreg, regdst, jal,
               pcsrc, alusrcb, alucontrol);
  datapath dp(clk, reset, 
              pcen, epcwrite, causewrite, intcause, irwrite, regwrite,
              alusrca, iord, memtoreg, regdst, jal,
              pcsrc, alusrcb, alucontrol,
              op, funct, zero, overflow,
              adr, writedata, readdata);
endmodule

module controller(input  logic       clk, reset,
                  input  logic [5:0] op, funct,
                  input  logic       zero, overflow,
                  output logic       pcen, epcwrite, causewrite, intcause,
				  output logic		 memwrite, irwrite, regwrite,
                  output logic       alusrca, iord, 
				  output logic [1:0] memtoreg,
				  output logic		 regdst, jal,
                  output logic [2:0] pcsrc,
                  output logic [2:0] alusrcb, alucontrol);

  logic [1:0] aluop;
  logic       branch, pcwrite, bne;

  // Main Decoder and ALU Decoder subunits.
  maindec md(clk, reset, op, funct,
             pcwrite, epcwrite, causewrite, intcause, memwrite, irwrite, regwrite,
             alusrca, branch, iord, memtoreg, regdst, 
             alusrcb, pcsrc, aluop, bne, jal);
  aludec  ad(funct, aluop, alucontrol);

  assign pcen = pcwrite | branch & (zero ^ bne);

 
endmodule

module maindec(input  logic       clk, reset, 
               input  logic [5:0] op, funct,
               output logic       pcwrite, epcwrite, causewrite, intcause,
			   output logic		  memwrite, irwrite, regwrite,
               output logic       alusrca, branch, iord, 
			   output logic [1:0] memtoreg, 
			   output logic 	  regdst,
               output logic [2:0] alusrcb, pcsrc,
			   output logic	[1:0] aluop,
               output logic       bne, jal);

  parameter   FETCH   = 4'b0000; 	// State 0
  parameter   DECODE  = 4'b0001; 	// State 1
  parameter   MEMADR  = 4'b0010;	// State 2
  parameter   MEMRD   = 4'b0011;	// State 3
  parameter   MEMWB   = 4'b0100;	// State 4
  parameter   MEMWR   = 4'b0101;	// State 5
  parameter   RTYPEEX = 4'b0110;	// State 6
  parameter   RTYPEWB = 4'b0111;	// State 7
  parameter   BEQEX   = 4'b1000;	// State 8
  parameter   ADDIEX  = 4'b1001;	// State 9
  parameter   IWB     = 4'b1010;	// state 10
  parameter   JEX     = 4'b1011;	// State 11
  parameter   BNEEX   = 4'b1100;	// State 12
  parameter   ORIEX   = 4'b1101;	// State 13
  parameter   JALEX   = 4'b1110;	// State 14
  parameter   JREX    = 4'b1111;	// State 15

  parameter   LW      = 6'b100011;	// Opcode for lw
  parameter   SW      = 6'b101011;	// Opcode for sw
  parameter   RTYPE   = 6'b000000;	// Opcode for R-type
  parameter   BEQ     = 6'b000100;	// Opcode for beq
  parameter   BNE     = 6'b000101;	// Opcode for bne
  parameter   ADDI    = 6'b001000;	// Opcode for addi
  parameter   ORI     = 6'b001101;	// Opcode for ori
  parameter   J       = 6'b000010;	// Opcode for j
  parameter   JAL     = 6'b000011;	// Opcode for jal
  
  parameter   JR      = 6'b001000; // Funct for jr

  logic [3:0]  state, nextstate;
  logic [22:0] controls;

  // state register
  always_ff @(posedge clk or posedge reset)			
    if(reset) state <= FETCH;
    else state <= nextstate;

  // next state logic
  always_comb
    case(state)
      FETCH:   nextstate <= DECODE;
      DECODE:  case(op)
                 LW:      nextstate <= MEMADR;
                 SW:      nextstate <= MEMADR;
                 RTYPE:   if(funct == JR)
                            nextstate <= JREX;
                          else
                            nextstate <= RTYPEEX;
                 BEQ:     nextstate <= BEQEX;
                 BNE:     nextstate <= BNEEX;
                 ADDI:    nextstate <= ADDIEX;
                 ORI:     nextstate <= ORIEX;
                 J:       nextstate <= JEX;
                 JAL:     nextstate <= JALEX;
                 default: nextstate <= 4'bx; // should never happen
               endcase
      MEMADR: case(op)
                 LW:      nextstate <= MEMRD;
                 SW:      nextstate <= MEMWR;
                 default: nextstate <= 4'bx;
               endcase
      MEMRD:   nextstate <= MEMWB;
      MEMWB:   nextstate <= FETCH;
      MEMWR:   nextstate <= FETCH;
      RTYPEEX: nextstate <= RTYPEWB;
      RTYPEWB: nextstate <= FETCH;
      BEQEX:   nextstate <= FETCH;
      BNEEX:   nextstate <= FETCH;
      ADDIEX:  nextstate <= IWB;
      IWB:     nextstate <= FETCH;
      ORIEX:   nextstate <= IWB;
      JEX:     nextstate <= FETCH;
      JALEX:   nextstate <= FETCH;
      JREX:    nextstate <= FETCH;
      default: nextstate <= 4'bx; // should never happen
    endcase

  // output logic
  assign {jal, bne, pcwrite,
		  epcwrite, causewrite, intcause,
          memwrite, irwrite, regwrite, 
          alusrca, branch, iord, memtoreg, regdst,
          alusrcb, pcsrc, aluop} = controls;

  always_comb
    case(state)
      FETCH:   controls <= 23'b001_000_010_000000_001_000_00;
      DECODE:  controls <= 23'b000_000_000_000000_011_000_00;
      MEMADR:  controls <= 23'b000_000_000_100000_010_000_00;
      MEMRD:   controls <= 23'b000_000_000_001000_000_000_00;
      MEMWB:   controls <= 23'b000_000_001_000010_000_000_00;
      MEMWR:   controls <= 23'b000_000_100_001000_000_000_00;
      RTYPEEX: controls <= 23'b000_000_000_100000_000_000_10;
      RTYPEWB: controls <= 23'b000_000_001_000001_000_000_00;
      BEQEX:   controls <= 23'b000_000_000_110000_000_001_01;
      BNEEX:   controls <= 23'b010_000_000_110000_000_001_01;
      ADDIEX:  controls <= 23'b000_000_000_100000_010_000_00;
      ORIEX:   controls <= 23'b000_000_000_100000_100_000_11;
      IWB:     controls <= 23'b000_000_001_000000_000_000_00;
      JEX:     controls <= 23'b001_000_000_000000_000_010_00;
      JALEX:   controls <= 23'b101_000_001_000000_000_010_00;
      JREX:    controls <= 23'b001_000_000_000000_000_011_00;
	  UNDEF:   controls <= 23'b001_111_000_000000_000_111_00;
	  OVERFLOW:controls <= 23'b001_110_000_000000_000_111_00;
	  MFC0:    controls <= 23'b000_000_001_000100_000_000_00;
	  SYSCALL: controls <= 23'b;
      default: controls <= 23'bxxx_xxx_xxx_xxxxxx_xxx_xxx_xx; // should never happen
    endcase
endmodule

module aludec(input  logic [5:0] funct,
              input  logic [1:0] aluop,
              output logic [2:0] alucontrol);

  always @(*)
    case(aluop)
      2'b00: alucontrol <= 3'b010;  // add
      2'b01: alucontrol <= 3'b110;  // sub
      2'b11: alucontrol <= 3'b001;  // ori
      default: case(funct)          // RTYPE
          6'b100000: alucontrol <= 3'b010; // ADD
          6'b100010: alucontrol <= 3'b110; // SUB
          6'b100100: alucontrol <= 3'b000; // AND
          6'b100101: alucontrol <= 3'b001; // OR
          6'b101010: alucontrol <= 3'b111; // SLT
          default:   alucontrol <= 3'bxxx; // ???
        endcase
    endcase

endmodule

module datapath(input  logic        clk, reset,
                input  logic        pcen, epcwrite, causewrite, intcause,
				input  logic		irwrite, regwrite, alusrca, iord, 
				input  logic [1:0]  memtoreg, 
				input  logic		regdst, jal,
                input  logic [1:0]  pcsrc, 
                input  logic [2:0]  alusrcb, alucontrol,
                output logic [5:0]  op, funct,
                output logic        zero, overflow,
                output logic [31:0] adr, writedata, 
                input  logic [31:0] readdata);

  // Below are the internal signals of the datapath module.

  logic [4:0]  writereg, writereg0;
  logic [31:0] pcnext, pc, epc, cause, status;
  logic [31:0] instr, data, srca, srcb;
  logic [31:0] a;
  logic [31:0] aluresult, aluout;
  logic [31:0] signimm;   // the sign-extended immediate
  logic [31:0] signimmsh;	// the sign-extended immediate shifted left by 2
  logic [31:0] zeroimm;   // the zero-extended immediate
  logic [31:0] wd3, rd1, rd2, wd30;

  // op and funct fields to controller
  assign op = instr[31:26];
  assign funct = instr[5:0];

  flopenr #(32) pcreg(clk, reset, pcen, pcnext, pc);
  mux2    #(32) adrmux(pc, aluout, iord, adr);
  flopenr #(32) epcreg(clk, reset, epcwrite, pc, epc);
  mux2	  #(32) causemux(8'h30, 8'h28, intcause, causecode);
  flopenr #(32) causereg(clk, reset, causewrite, causecode, cause);
  flopenr #(32) instrreg(clk, reset, irwrite, readdata, instr);
  flopr   #(32) datareg(clk, reset, readdata, data);
  mux2    #(5)  regdstmux(instr[20:16], instr[15:11], regdst, writereg0);
  mux2    #(5)  jalmux(writereg0, 5'b11111, jal, writereg);
  mux3    #(32) wdmux(aluout, data, cause, memtoreg, wd30);
  mux2    #(32) jalpcmux(wd30, pc, jal, wd3);
  regfile       rf(clk, regwrite, instr[25:21], instr[20:16], writereg, wd3, rd1, rd2);
  signext       se(instr[15:0], signimm);
  zeroext       ze(instr[15:0], zeroimm);
  sl2           immsh(signimm, signimmsh);
  flopr   #(32) areg(clk, reset, rd1, a);
  flopr   #(32) breg(clk, reset, rd2, writedata);
  mux2    #(32) srcamux(pc, a, alusrca, srca);
  mux5    #(32) srcbmux(writedata, 32'b100, signimm, signimmsh, zeroimm, alusrcb, srcb);
  alu           alu(srca, srcb, alucontrol, aluresult, zero, overflow);
  flopr   #(32) alureg(clk, reset, aluresult, aluout);
  mux5    #(32) pcmux(aluresult, aluout, {pc[31:28], instr[25:0], 2'b00}, rd1, 32'b100, pcsrc, pcnext);
  
endmodule


module mux3 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] d0, d1, d2,
              input  logic [1:0]       s, 
              output logic [WIDTH-1:0] y);

  assign #1 y = s[1] ? d2 : (s[0] ? d1 : d0); 
endmodule

module mux4 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] d0, d1, d2, d3,
              input  logic [1:0]       s, 
              output logic [WIDTH-1:0] y);

   always_comb
      case(s)
         2'b00: y <= d0;
         2'b01: y <= d1;
         2'b10: y <= d2;
         2'b11: y <= d3;
      endcase
endmodule

module mux5 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] d0, d1, d2, d3, d4,
              input  logic [2:0]       s, 
              output logic [WIDTH-1:0] y);

   always_comb
      casex(s)
         3'b000: y <= d0;
         3'b001: y <= d1;
         3'b010: y <= d2;
         3'b011: y <= d3;
         3'b1xx: y <= d4;
      endcase
endmodule

