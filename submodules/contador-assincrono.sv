module upcount (Resetn, Clock, E, Q);
 input Resetn, Clock, E;
 output [3:0] Q;
 reg [3:0] Q;

 always @(negedge Resetn or posedge Clock)
 if (!Resetn)
 Q <= 0;
 else if (E)
 Q <= Q + 1;
 endmodule