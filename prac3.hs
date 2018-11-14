import System.IO

fibPaso::(Integer,Integer)->(Integer,Integer)
fibPaso(x,y)=(y, x+y)

fibPar::Integer->(Integer,Integer)
fibPar n
    |n==0=(0,1)
    |otherwise=fibPaso(fibPar(n-1))

fibEficiente::Integer->Integer
fibEficiente n =fst (fibPar n)

esPar::Int -> Bool
esPar n = 0==(mod) n 2

listaP20::[Int]
listaP20=[1..20]

sonPares::[Bool]
sonPares=[esPar n | n <- [0..20]]

--rotarFinal::[Integer] -> [Integer]


numerosPrimos = 3 : 5 : 7 : 11 : []

factorial :: [] -> []
factorial [] -> []
factorial (first n)