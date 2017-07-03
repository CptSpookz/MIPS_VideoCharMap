 module regn (D, Clock, Resetn, Q);
 parameter n = 16;
 input [n-1:0] D;
 input Clock, Resetn;
 output [n-1:0] Q;
 reg[n-1:0] Q;

 always @(negedge Resetn or posedge Clock)
 if (!Resetn)
 Q <= 0;
 else
 Q <= D;
 endmodule