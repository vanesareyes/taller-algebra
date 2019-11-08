a = head [(1,2), (3,4), (5,2)]
b = tail [1,2,3,4,4,3,2,1]
c = [True, True] ++ [False, False] 
d = [1,2] : []  -- [[1,2]]
e = head []
f = head [1,2,3] : [4,5] -- [1,4,5]
-- g = head ([1,2,3] : [4,5]) -- error porque deberìa usar el ++ [1,2,3] : [[4,5]]
h = head ([1,2,3] : [4,5] : []) -- [1,2,3] h::[Integer] No tira error porque los : son asociativos hacia la derecha

listar :: a -> a -> a -> [a]
listar x y z = [x,y,z] -- otra opcion x : y : z : []

pertenece :: Eq a => a -> [a] -> Bool --Eq son tipos que soportan la igualdad
pertenece n xs |length xs == 0 = False
               |otherwise = n == head xs || pertenece n (tail xs)
                
pertenece2 :: Eq a => a -> [a] -> Bool --Eq son tipos que soportan la igualdad
pertenece2 n xs |xs == [] = False
                |n == head xs = True 
                |otherwise = pertenece2 n (tail xs)

pertenece3 :: Eq a => a -> [a] -> Bool --Eq son tipos que soportan la igualdad
pertenece3 n [] = False
pertenece3 n (x:xs) = n == x || pertenece n xs

primerMultiplode45345 :: [Integer] -> Integer
primerMultiplode45345 xs | mod (head xs) 45345 == 0 = head xs
                         | otherwise = primerMultiplode45345 (tail xs)  

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x* productoria xsx 

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n xs | xs == [] = []
            | n>= 0 = head xs + n : sumarN n (tail xs)

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero xs = sumarN (head xs) xs  

sumarElPrimero2 :: [Integer] -> [Integer]
sumarElPrimero2 (x:xs) = sumarN x (x:xs)  

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs 
         	 | otherwise = pares xs  

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo (x:xs) = sumarN (last xs) (x:xs) 

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x : multiplosDeN n xs 
		      | otherwise = multiplosDeN n xs 

quitar :: Integer -> [Integer] -> [Integer] 
quitar n [] = []
quitar n (x:xs) | x == n = xs
		| x /= n = x : quitar n xs

hayRepetidos :: [Integer] -> Bool 
hayRepetidos [] = False
hayRepetidos (x:[]) = False
hayRepetidos (x:y:xs) | elem x xs = True
                      | otherwise = x==y || hayRepetidos (y:xs)


maximo :: [Integer] -> Integer 
maximo [] = 0
maximo (x:[]) = x
maximo (x:y:xs) = max (max x y) (maximo (y:xs))


ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:[]) = x
ordenar (x:y:xs) | x<=y = x : ordenar (y:xs)
		 | otherwise = y : ordenar (x:xs)
				 

reverso :: [Integer] -> [Integer] 
reverso [] = []
reverso (x:[]) = [x]
reverso (x:xs) = reverso(xs) ++ [x]

