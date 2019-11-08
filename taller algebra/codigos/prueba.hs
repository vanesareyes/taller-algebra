digitosIguales :: Integer -> Bool
digitosIguales n | 0 <= n && n <= 9 = True 
                 | otherwise = mod n 10 == mod (div n 10) 10 && digitosIguales (div n 10) 

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n xs | xs == [] = []
            | n>= 0 = head xs + n : sumarN n (tail xs)

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo (x:xs) = sumarN (last xs) (x:xs)

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + otrosDivisores           
                       | mod n k /= 0 = otrosDivisores    
                         where otrosDivisores = sumaDivisoresHasta n (k-1)

esDefectivo :: Integer -> Bool
esDefectivo n = sumaDivisoresHasta n (n-1) < n

maximaDistancia :: [Integer] -> Integer
maximaDistancia (x:y:[]) = abs (y-x)
maximaDistancia (x:y:xs) = max (abs(y-x)) (maximaDistancia (y:xs))

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:[]) = [x]
ordenar (x:y:xs) | x<=y = x : ordenar (y:xs)
				 | otherwise = y : ordenar (x:xs)


reverso :: [Integer] -> [Integer] 
reverso [] = []
reverso (x:[]) = [x]
reverso (x:xs) = reverso(xs) ++ [x]

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b = mod (-a) b == 0 

cantidadDigitos :: Integer -> Integer
cantidadDigitos n | n >= 0 && n <= 9 = 1
                  | n >= 10 = 1 + cantidadDigitos (div n 10)

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n d | mod n d == 0 = d --caso base es es el primer divisor
                      | otherwise = menorDivisorDesde n (d+1) 

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

zipPrimos :: [Integer] -> [Integer] -> [(Integer,Integer)]
zipPrimos [] [] = []
zipPrimos (x:xs) (y:ys) | esPrimo x && esPrimo y = (x,y) : zipPrimos xs ys
                        | otherwise = zipPrimos xs ys 