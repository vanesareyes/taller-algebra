--module Clase6
--where
--import Clase5


alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar a b c | mod a 2 /= 0 ||  mod b 2 /= 0 || mod c 2 /= 0 = True
                     | otherwise = False  

alMenosUnImpar2 :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar2 a b c = not (esPar a) || not (esPar b) || not (esPar c) 

(**) 2 3.1 -- potencia racional

tuplas (Bool,(Float,Integer))
Recursion siempre se acerca a un caso base

sumaLosPrimerosNImpares :: Integer -> Integer
sumaLosPrimerosNImpares n
| n == 1 = 1
| n > 1 = n_esimoImpar + sumaLosPrimerosNImpares (n -1)
where n_esimoImpar = 2 * n - 1

A veces ciertas funciones esperan un Float y nosotros tenemos un Integer.
Para estos casos podemos utilizar la funcion fromInteger :: Integer -> Float.

eAprox :: Integer -> Float
eAprox n |n==0 = 1
         |n > 0 = 1/ (fromInteger(factorial2 n)) + eAprox (n-1)  

parteEntera :: Float -> Integer
parteEntera x |x>= 0 && x< 1 = 0 
              |x>= 1 = 1 + parteEntera (x-1) 
              |x< 0 = (-1) + parteEntera (x+1)

division :: Integer -> Integer -> (Integer, Integer)  -- d>0
division a d | abs a < d = (0,abs a)
             | a<0 = (fst (divisionNeg (a+d) d) -1, snd (divisionNeg (a+d) d))    
             | otherwise = (fst (division (a-d) d) +1, snd (division (a-d) d)) 

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + otrosDivisores           
                       | mod n k /= 0 = otrosDivisores    
                         where otrosDivisores = sumaDivisoresHasta n (k-1)   

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n d | mod n d == 0 = d --caso base es es el primer divisor
                      | otherwise = menorDivisorDesde n (d+1) 

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

sumaInterna :: Integer -> Integer -> Integer
sumaInterna n m | n==1 = m
                | m==1 = n
                | otherwise = n^m + sumaInterna n (m-1)

sumaTotal :: Integer -> Integer -> Integer  -- chequear si necesita caso base
sumaTotal n m = sumaInterna n m + sumaTotal (n-1) m

sumaPotencia :: Integer -> Integer -> Integer -> Integer --chequear
sumaPotencia q n m | m == 1 = q^(n+1)
                   | otherwise = q^(m+n) + sumaPotencia q n (m-1) 

sumaPotenciaTotal :: Integer -> Integer -> Integer -> Integer
sumaPotenciaTotal q 1 m = sumaPotencia q 1 m 
sumaPotenciaTotal q n m = sumaPotencia q n m + sumaPotenciaTotal q (n-1) m 

sumaRacInt :: Integer -> Integer -> Float  --chequear
sumaRacInt 0 m = 0 
sumaRacInt n 1 = n
sumaRacInt n m = fromInteger n / fromInteger m + sumaRacInt n (m-1) 

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m = sumaRacInt n m + sumaRacionales n-1 m 

sumaVectorial2 :: ( Float , Float ) -> ( Float , Float ) -> ( Float , Float )
sumaVectorial2 (x1,y1) (x2,y2) = (x1+x2,y1+y2) --- matcheo cada vector con el formato x1,y1

-- expresion factorial (n + 1) = (n + 1) * factorial n ES INCORRECTA
-- expresion iguales x x = True ES INCORRECTA
sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n-1)

algunoEsCero1 :: (Integer, Integer, Integer) -> Bool
algunoEsCero1 (x,y,z) = x==0 || y==0 || z==0

cantidadDigitos :: Integer -> Integer
cantidadDigitos n | n >= 0 && n <= 9 = 1
                  | n >= 10 = 1 + cantidadDigitos (div n 10)

esCapicuaAux :: Integer -> Integer -> Integer -> Bool
esCapicuaAux n m k | n >= 0 && n <= 9 = True
                   | m < k = True
                   | iesimoDig n k == iesimoDig n m = esCapicuaAux n  (m-1) (k+1)  
                   | otherwise = False

esCapicua :: Integer -> Bool
esCapicua n = esCapicuaAux n (cantidadDigitos n) 1

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0 
sumaDigitos n =  mod n 10 + sumaDigitos (div n 10)

digitosIguales :: Integer -> Bool
digitosIguales n | 0 <= n && n <= 9 = True  -- PREGUNTAR QUE PASA CUANDO LA RECURSION LLEGA A LA PRIMERA CIFRA
                 | otherwise = mod n 10 == mod (div n 10) 10 && digitosIguales (div n 10) 

esSumaDeDosPrimosHasta :: Integer -> Integer -> Bool
esSumaDeDosPrimosHasta n m | m==2 = esPrimo (n-m)
                           | otherwise = esPrimo m && esPrimo (n-m) || esSumaDeDosPrimosHasta n (m-1)    

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos n = esSumaDeDosPrimosHasta n (n-2)

goldbach :: Integer -> Bool --todo par puede escribirse como suma de primos
goldbach n | n == 4 = True
           | otherwise = esSumaDeDosPrimos n && goldbach (n-2)

--listas
d = [1,2] : []  -- [[1,2]]
listar :: a -> a -> a -> [a]
listar x y z = [x,y,z] -- otra opcion x : y : z : []

pertenece3 :: Eq a => a -> [a] -> Bool --Eq son tipos que soportan la igualdad
pertenece3 n [] = False
pertenece3 n (x:xs) = n == x || pertenece n xs

primerMultiplode45345 :: [Integer] -> Integer
primerMultiplode45345 xs | mod (head xs) 45345 == 0 = head xs
                         | otherwise = primerMultiplode45345 (tail xs)

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n xs | xs == [] = []
            | n>= 0 = head xs + n : sumarN n (tail xs)

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs 
         	 | otherwise = pares xs  

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo (x:xs) = sumarN (last xs) (x:xs) 

menorLex2 :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
menorLex2 (x1,x2,x3) (y1,y2,y3) = x1 < y1 || (x1 == y1 && x2 < y2) || (x1 == y1 && x2 == y2 && x3 < y3)

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1   
            | n >= 2 = fibonacci (n-1) + fibonacci (n-2)

sumaFibonacci :: Integer -> Integer
sumaFibonacci 0 = 1
sumaFibonacci n = sumaFibonacci (n-1) + fibonacci n

maximaDistancia :: [Integer] -> Integer
maximaDistancia x:y:[] = abs (y-x)
maximaDistancia (x:y:xs) = max (abs(y-x), maximaDistancia (y:xs))

compAux :: [Integer] -> [(Integer, Integer)]
compAux [] = []
compAux (x:xs) = (x,1): compAux xs

comprimir :: [(Integer, Integer)] -> [(Integer, Integer)]
comprimir [] = []
comprimir ((n,c):[]) = [(n,c)]
comprimir ((n,c):(n2,c2):xs) | n == n2 = comprimir (n,c+c2):xs
                             | otherwise = (n,c):comprimir (n2,c2:xs) 

comprimirFinal :: [Integer] -> [(Integer, Integer)]
comprimirFinal xs = comprimir (compAux xs)

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:[]) = [x]
ordenar (x:y:xs) | x<=y = x : ordenar (y:xs)
				 | otherwise = y : ordenar (x:xs)

esTerminodeSuc :: Integer -> Integer -> Bool
esTerminodeSuc n m | sucesion m == n = True
                   | sucesion m < n = esTerminodeSuc n (m+1)
                   | sucesion m > n = False

listarTerminosSuc :: [Integer] -> [Integer]
listarTerminosSuc [] = 0
listarTerminosSuc (x:xs) | esTerminodeSuc x 1 = x:listarTerminosSuc xs
                         | otherwise = listarTerminosSuc xs

sumarLista :: [Integer] -> Integer
sumarLista [] = 0
sumarLista (x:xs) = fromInteger x + sumarLista xs

promedioTerminos :: [Integer] -> Float
promedioTerminos xs | longitud (listarTerminosSuc (xs)) == 0 = 0
                    | otherwise = sumarLista (listarTerminosSuc (xs)) / (fromInteger(longitud (listarTerminosSuc (xs))))