module Clase5
(esPrimo)
where 
import Clase4

eAprox :: Integer -> Float
eAprox n |n==0 = 1
         |n > 0 = 1/ (fromInteger(factorial2 n)) + eAprox (n-1)  
          

e :: Float
e = eAprox 100

parteEntera :: Float -> Integer
parteEntera x |x>= 0 && x< 1 = 0 
                    |x>= 1 = 1 + parteEntera (x-1) 
                    |x< 0 = (-1) + parteEntera (x+1)

division :: Integer -> Integer -> (Integer, Integer)
division a d | a>=0 && a < d = (0,a)
             | otherwise = (fst (division (a-d) d) +1, snd (division (a-d) d))     

divisionNeg :: Integer -> Integer -> (Integer, Integer)
divisionNeg a d | a>=0 && a < d = (0,a)
                | a<0 = (fst (divisionNeg (a+d) d) -1, snd (divisionNeg (a+d) d))    
                | otherwise = (fst (division (a-d) d) +1, snd (division (a-d) d))     


sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + otrosDivisores           -- snd(division n k)/=0  
                       | mod n k /= 0 = otrosDivisores    
                         where otrosDivisores = sumaDivisoresHasta n (k-1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n d | mod n d == 0 = d --caso base es es el primer divisor
                      | otherwise = menorDivisorDesde n (d+1)  

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

sumaInterna :: Integer -> Integer -> Integer
sumaInterna n m | m==1 = n
                | otherwise = n^m + sumaInterna n (m-1)

sumaTotal :: Integer -> Integer -> Integer
sumaTotal 1 m = sumaInterna 1 m
sumaTotal n m = sumaInterna n m + sumaTotal (n-1) m

sumaPotencia :: Integer -> Integer -> Integer -> Integer
sumaPotencia q n m | m == 1 = q^(n+1)
                   | otherwise = q^(m+n) + sumaPotencia q n (m-1) 

sumaPotenciaTotal :: Integer -> Integer -> Integer -> Integer
sumaPotenciaTotal q 1 m = sumaPotencia q 1 m 
sumaPotenciaTotal q n m = sumaPotencia q n m + sumaPotenciaTotal q (n-1) m --ver en la clase del 4/10 que me falta el caso base

sumaRacionales :: Float -> Float -> Integer -> Integer -> Float
sumaRacionales p q n m 