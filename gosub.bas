100 LET X = 3
110 GOSUB 400
120 PRINT U, V, W
200 LET X = 5
210 GOSUB 400
215 PRINT U, V, W
220 LET Z = U + 2*V + 3*W
230 PRINT "Z = " Z
240 STOP
400 LET U = X*X
410 LET V = X*X*X
420 LET W = X*X*X*X + X*X*X + X*X + X
430 RETURN
440 END