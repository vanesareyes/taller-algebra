menorLex :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
menorLex (x1,x2,x3) (y1,y2,y3) | x1 < y1 = True
                               | x1 == y1 && x2 < y2 = True
                               | x1 == y1 && x2 == y2 && x3 < y3 = True
                               | otherwise = False

--- otra opcion
menorLex2 :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
menorLex2 (x1,x2,x3) (y1,y2,y3) = x1 < y1 || (x1 == y1 && x2 < y2) || (x1 == y1 && x2 == y2 && x3 < y3)

fibonacci :: Integer -> Integer
fibonacci n |n == 0 = 1
            |n == 1 = 1
            |n >= 2 = fibonacci (n-1) + fibonacci (n-2)

sumaFibonacci :: Integer -> Integer
sumaFibonacci 0 = 1
sumaFibonacci n = sumaFibonacci (n-1) + fibonacci n

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + otrosDivisores           
                       | mod n k /= 0 = otrosDivisores    
                         where otrosDivisores = sumaDivisoresHasta n (k-1)

esDefectivo :: Integer -> Bool
esDefectivo n = sumaDivisoresHasta n n-1 < n

--otra opcion para esDefectivo

divPropiosHasta :: Integer -> Integer -> [Integer]
divPropiosHasta n 1 = [1]
divPropiosHasta n m | mod n m == 0 = m : divPropiosHasta n (m-1) 
                    | otherwise = divPropiosHasta n (m-1)

suma :: [Integer] -> Integer
suma [] = 0
suma (x:xs) = x + suma xs

divPropios :: Integer -> [Integer]
divPropios n = divPropiosHasta n (n-1)

esDefectivo2 :: Integer -> Bool
esDefectivo2 n = suma (divPropios n) < n 

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




















