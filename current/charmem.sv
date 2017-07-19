//------------------------------------------------
// charmem.sv
// Memoria externa para os caracteres.
//------------------------------------------------

module charmem( input  logic        clk,
                input  logic	    charprint,
                input  logic [5:0]  character,
                output logic [63:0] vdata);

  logic  [63:0] CRAM[39:0];
  logic  [63:0] aux;

  // inicializando a memoria com os bitmaps
  initial
    begin
      $readmemh("charfile_old.dat",CRAM);
    end
  
    always_ff @(posedge clk)
	 if(charprint)
      vdata <= CRAM[character];
endmodule
