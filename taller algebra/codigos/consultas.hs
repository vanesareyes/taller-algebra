elementoK :: [Integer] -> Integer -> Integer
elementoK (x:xs) 1 = x
elementoK (x:xs) k = elementoK xs (k-1)

lugarCuadradoHasta (x:xs) k | k == 0 = 0 
							| k^2 == (elementoK (x:xs) k) = 1 + lugarCuadradoHasta (x:xs) (k-1) 
							| otherwise = lugarCuadradoHasta (x:xs) (k-1) 

lugarCuadrado (x:xs) = lugarCuadradoHasta (x:xs) (fromIntegral(length (x:xs)))