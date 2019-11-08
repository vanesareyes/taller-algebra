f :: Bool -> Bool
f x = not x

g :: Bool -> Float
g x = pi

h :: Integer -> Integer -> Bool -> Bool
h a b c = c || (a>b)

doble :: Float -> Float
doble x = x + x

cuadruple :: Float -> Float
cuadruple x = doble (doble x)

--dist :: Float -> Float -> Float -> Float -> Float

esPar :: Integer -> Bool
esPar x | mod x 2 == 0 = True
 	| otherwise = False

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe a b | mod a b == 0 = True 
		 | otherwise = False

identidad :: a -> a -- las variables las pongo siempre en minùscula y los 				tipos en mayùscula
identidad x = x

triple x = x * 3

crearPar :: a -> b -> (a,b)
crearPar x y = (x,y) 

invertir :: (a,b) -> (b,a)
invertir (x,y) = (y,x)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos x y = sqrt ((fst (y)-fst(x))^2 + (snd(y)-snd(x))^2)  

f1 :: Float -> (Float,Float,Float)
f1 x = (2*x, x^2, x-7) 

f2 :: Integer -> Integer
f2 n | mod n 2 == 0 = div n 2  -- no sirve poner n/2 porque es integer
     | otherwise = n + 1 

f3 :: Integer -> Integer
f3 n | mod n 6 == 0 = div (n^2) 2
     | otherwise = 3*n + 1

g1 :: (Integer,Integer) -> Integer
g1 x = fst(x)*(snd(x)+1)
-- (fog)(3,4)=46

h1 :: (Integer,Integer) -> Integer
h1 b = f3 (g1 b) --h(3,2) = 28

