module Practico3 where
    aa = [1,2,3,4,5,6]
    pal = [1,2,3,2,1]
    pal2 = [1,2,2,1]
    b = 2 : []

    rotarFinal :: [Integer] -> [Integer]
    rotarFinal [] = []
    rotarFinal x = (tail x) ++ [head x]

    rotarFinalN :: [Integer]-> Integer -> [Integer]
    rotarFinalN l n
        |n == 0 = l 
        |n > 0 = rotarFinalN (rotarFinal l) (n-1)

    rangoL :: [Integer] -> [Integer]
    rangoL [] = []
    rangoL x =  [minimum x] ++ [maximum x]

    palindromo::[Integer] -> Bool
    palindromo l
        |l == [] = True
        |length(l) == 1 = True
        |(head l) == (last l) = palindromo(init(tail l))
        |(head l) /= (last l) = False

    interior::[Integer] -> [Integer]
    interior l = tail(init l)

    primerosN :: [Integer] -> Integer -> [Integer]
    primerosN l n
        |n == 0 = []
        |n > 0 = [(head l)] ++ (primerosN (tail l) (n-1))

    ultimosN::[Integer] -> Integer -> [Integer]
    ultimosN l n  
        |n == 0 = []
        |n > 0 = (ultimosN (init l) (n-1)) ++ [(last l)]

    

    sumaMultiplosDeCinco :: Integer -> (Integer, [Integer])
    sumaMultiplosDeCinco n = (sum([n, (n-5)..5]), [n, (n-5)..5])

    paresLista :: [Integer] -> [Integer]
    paresLista l
        |l == [] = []
        |(mod (head l) 2) == 0 = [(head l)] ++ (paresLista(tail l))
        |(mod (head l) 2) == 1 = (paresLista(tail l))

    imparesLista :: [Integer] -> [Integer]
    imparesLista l
        |l == [] = []
        |(mod (head l) 2) == 1 = [(head l)] ++ (imparesLista(tail l))
        |(mod (head l) 2) == 0 = (imparesLista(tail l))
    
    paresImpares :: [Integer] -> ([Integer], [Integer])
    paresImpares a = ((paresLista a), (imparesLista a))

    listaToNumero :: [Integer] -> Integer
    listaToNumero l
        |l == [] = 0
        |otherwise = (last l) + listaToNumero(init(l))*10

    
    
    listaNM :: [Integer] -> Integer -> Integer -> [Integer]
    listaNM l n m = (ultimosN (primerosN l m) (m-(n-1)))

    sumaNToM :: Integer -> Integer -> [[Integer]] -> [Integer]
    sumaNToM n m l
        |(length l) == 0 = []
        |otherwise = [(sum (listaNM (head l) n m))] ++ (sumaNToM n m (tail l))

    ll = [[1,2,3,4,5],[2,2,2,2],[10,11,12,14]]

    darRangoNM :: Integer -> Integer -> [Integer] -> [Integer]
    darRangoNM n m l= (ultimosN (primerosN l m) (m-n))

    hamming :: [Integer] -> [Integer] -> Integer
    hamming a b
        |(a == [] || b == []) = 0
        |(head a) == (head b) = 0 + (hamming (tail a) (tail b))
        |(head a) /= (head b) = 1 + (hamming (tail a) (tail b))

    eliminarMultiplos :: Integer -> [Integer] -> [Integer]
    eliminarMultiplos x l
        |l == [] = []
        |(mod (head l) x) == 0 = (eliminarMultiplos x (tail l))
        |otherwise = [(head l)] ++ (eliminarMultiplos x (tail l))

    calcularPrimosEnLista :: [Integer] -> [Integer]
    calcularPrimosEnLista l
        |l == [] = []
        |otherwise = [(head l)] ++ calcularPrimosEnLista (eliminarMultiplos (head l) (tail l)) 

    