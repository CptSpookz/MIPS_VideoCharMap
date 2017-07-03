module fulladd (Cin, x, y, s, Cout);
 input Cin, x, y;
 output s, Cout;

 assign s = x ˆ y ˆ Cin;
 assign Cout = (x & y) | (x & Cin) | (y & Cin);
 endmodule