module Clase4
where
--import Clase3
factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n -1)

factorial2 :: Integer -> Integer

factorial2 n | n == 0 = 1
             | otherwise = n * factorial2 (n -1)

--Usando pattern matching
factorial3 0 = 1
factorial3 n = n * factorial3 ( n - 1)

--factorial2 0 = 1, factorial2 3 = 6, factorial2 (-1) error xq no termina nunca 

esPar :: Integer -> Bool
esPar n | n ==0 = True
	| otherwise = esPar (n -2)

-- esta funcion tiene errores porque nunca da falso

esPar2 :: Integer -> Bool
esPar2 n | n ==0 = True
         | otherwise = not ( esPar (n -1) ) --si n es par n-1 va a ser impar y viceversa

esPar3 :: Integer -> Bool
esPar3 n | n ==0 = True
         | n ==1 = False
         | otherwise = esPar3 (n -2)

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1   
            | n >= 2 = fibonacci (n-1) + fibonacci (n-2)

a1 :: Integer -> Integer
a1 n | n == 1 = 2
     | n > 1 = 2 * (n - 1) * a1 (n-1) + 2^n * factorial (n-1) 
 
a2 :: Integer -> Integer
a2 n | n == 1 = 1
     | n == 2 = 2
     | n > 2 = (n-2) * a2 (n-1) + 2 * (n-1) * a2 (n-2)

a3 :: Integer -> Integer
a3 n | n == 1 = -3
     | n == 2 = 6
     | esPar3 n = -(a3 (n-1)) - 3
     | otherwise = (a3 (n-1)) + 2 * (a3 (n-2)) + 9 -- otra opcion es not(esPar3 n)

-- n > 2 && esPar n = (-1) * a3 (n-1) - 3      
-- otherwise = a3 (n-1) + 2 * a3 (n-2) + 9

f1 :: Integer -> Integer
f1 n |n == 0 = 1
     |n >= 1 = f1(n-1) + 2^n

f2 :: Integer -> Float -> Float 
f2 n q | n == 1 = q
       | n > 1 = f2 (n-1) q + q^n  

--f3 :: Integer -> Float -> Float 
--f3 n q | n == 0 = 0
       | n > 1 = f3 (n-1) q + q^(2*n-1) + q^(2*n) 

f3bis :: Integer -> Float -> Float 
f3bis n q | n == 0 = 0
          | n > 0 = f2 (2*n) q --es la misma sumatoria que f2 pero hasta 2n
  
f4 :: Integer -> Float -> Float 
f4 n q | n == 0 = 1
       | n == 1 = q + q^2 -- f3bis n q
       | n > 1 = f4 (n-1) q + q^(2*n-1) + q^(2*n)  -- f3bis n q - f2 (n-1) q

esMultiplode3 :: Integer -> Bool
esMultiplode3 n | n <= 2 = False
                | n == 3 = True
                | n > 3 = esMultiplode3 (n-3)   

sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = 1
              | esPar3 n = sumaImpares(n-1)  
              | otherwise = n + sumaImpares(n-1)

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact 2 = 2
medioFact n | n > 2 = n*medioFact() 
                

