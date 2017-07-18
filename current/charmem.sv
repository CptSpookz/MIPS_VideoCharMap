//------------------------------------------------
// charmem.sv
// Memoria externa para os caracteres.
//------------------------------------------------

module charmem( input  logic        clk,
                input logic charprint,
                input  logic [5:0] character,
                output wire [63:0] vdata);

  logic  [63:0] CRAM[39:0];


  // inicializando a memoria com os bitmaps
  initial
    begin
      $readmemh("charmem.dat",CRAM);
    end

    always @(posedge clk)
	 if(charprint)
      vdata <= CRAM[character];
endmodule
