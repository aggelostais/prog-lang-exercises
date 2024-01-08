%Bgainei se pinaka chars
dec_bin(0,'0').
dec_bin(1,'1').
dec_bin(N,B):-N>1,X is N mod 2,Y is N//2,dec_bin(Y,B1),atom_concat(B1, X, B).

dec_bin(9,X),atom_chars(X,Y).
