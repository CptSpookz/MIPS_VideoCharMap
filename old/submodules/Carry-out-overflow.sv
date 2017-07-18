module addern (carryin, X, Y, S, carryout, overflow);
 parameter n = 32;
 input carryin;
 input [n-1:0] X, Y;
 output [n-1:0] S;
 output carryout, overflow;
 reg[n-1:0]S;
 reg carryout, overflow;

 always @(X or Y or carryin)
 begin
 S = X + Y + carryin;
 carryout=(X[n-1] & Y[n-1]) | (X[n-1] & S[n-1])
 | (Y[n-1] & S[n-1]);
 overflow = carryout ˆ X[n-1] ˆ Y[n-1] ˆ S[n-1];
 end
endmodule