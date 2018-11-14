import Char
import Test.QuickCheck

esPar :: Integer -> Bool
esPar x = ((mod x 2) == 0)

paresImpares :: [Integer] -> ([Integer],[Integer])
paresImpares l = ((filter esPar l), (filter (not.esPar) l)) 

cuadrado :: Integer -> Integer
cuadrado x = x*x

sumCuadrados :: Integer -> Integer
sumCuadrados x = foldr (+) 0 (map cuadrado [1..x])

todoMayuscula :: String -> String
todoMayuscula str = map toUpper str

invertirStr :: String -> String
invertirStr str 
    |str == [] = []
    |otherwise =(invertirStr (tail str)) ++ [(head str)]

invertirTodo :: [String] -> [String]
invertirTodo l = map invertirStr (reverse l)

insertarAdelanteAux :: a -> [a] -> [a]
insertarAdelanteAux a l = a : l

insertarAdelante :: a -> [[a]] -> [[a]]
insertarAdelante a l = map (insertarAdelanteAux a) l

ll = [[1,2,3,4], [5,6,7,8], [9, 10, 11, 12]]
l = ["hola", "que", "haces"]

boolInteger :: Bool -> Integer
boolInteger b
    |b == True = 1
    |b == False = 0

aridad :: [String] -> [Integer]
aridad lstr = map (boolInteger.not.esPar.toInteger.length) lstr

--aridad2 :: [String] -> [Integer]
--aridad2 lstr = map (mod (sum lstr) 2) lstr

sumCuadrados2 :: Integer -> Integer
sumCuadrados2 x = sum (map cuadrado [1..x])

--eliminarRepetidos :: Eq a => [a] -> [a]
--eliminarRepetidos 