a = [1..10]

positivo :: Integer -> Bool
positivo x
    | x >= 0 = True
    | otherwise = False

cuadrado :: Integer -> Integer
cuadrado x = x * x

ej1 :: [Integer] -> Integer
ej1 x = foldr (+) 0 (map (cuadrado) (filter positivo x))

areaTriangulo :: Float -> Float -> Float
areaTriangulo x y = (x * y) / 2

areaRectangulo :: Float -> Float -> Float
areaRectangulo x y = x * y

ej2 :: [Float] -> Float
ej2 l
    |(length l) == 2 = areaTriangulo  (head l) (head (tail l))
    |(length l) == 4 = areaRectangulo (head l) (head (tail l))
    |otherwise = 0

invertir :: (a,b) -> (b, a)
invertir x = ((snd x), (fst x))

ll = [(1, True), (3, False)]

ej3 :: [(a,b)] -> [(b,a)]
ej3 l = map invertir l

ej4 :: [a] -> ([a], [a])
ej4 x = (x,x)

