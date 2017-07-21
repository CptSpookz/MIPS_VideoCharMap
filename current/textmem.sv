//-------------------------------------------------------------
// textmem.sv
//
// Memoria externa para decodificar
// a instrução e imprimir os caracteres.
//
// São Carlos, 2017
//-------------------------------------------------------------

module textmem( input  logic        clk,
                input logic textprint,
                input  logic [5:0] character,
                output logic [63:0] vdata);

  logic  [31:0] CRAM_upper[35:0];
  logic  [31:0] CRAM_lower[35:0];
  logic	 [63:0] char;

  // inicializando a memoria com os bitmaps
  initial
    begin
      $readmemh("charmem_upper.dat",CRAM_upper);
      $readmemh("charmem_lower.dat",CRAM_lower);
    end

	assign char = {CRAM_upper[character], CRAM_lower[character]};

	always @(posedge clk)
	 if(textprint)
	 begin
      vdata <= char;
	 end
endmodule
