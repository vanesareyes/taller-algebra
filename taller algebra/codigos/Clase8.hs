type Set a = [a]
vacio :: Set Integer
vacio = []

agregar :: Integer -> Set Integer -> Set Integer
agregar n cx |elem n cx = cx -- primero tengo que ver que n no pertenece a cx xq no puede haber elementos repetidos
             |otherwise = n:cx

incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True
incluido (x:cx) cy = elem x cy && incluido cx cy

incluido2 :: Set Integer -> Set Integer -> Bool
incluido2 (x:cx) cy |elem x cy = incluido2 cx cy
                    |otherwise = False

iguales :: Set Integer -> Set Integer -> Bool
iguales cx cy = incluido cx cy && incluido cy cx 

iguales2 :: Set Integer -> Set Integer -> Bool
iguales2 cx cy = incluido cx cy && length cx == length cy
               
perteneceC :: Set Integer -> Set (Set Integer) -> Bool -- elem no sirve xq haskell lo implementa como si fueran listas
perteneceC ca [] = False
perteneceC ca (cb:ccb) = iguales ca cb || perteneceC ca ccb

agregarC :: Set Integer -> Set (Set Integer) -> Set (Set Integer)    
agregarC ca cb |perteneceC ca cb = cb
               |otherwise = ca:cb

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos [] = []
agregarATodos n (ca:cca) = agregarC (agregar n ca) (agregarATodos n cca)

agregar2 :: Eq a=> a -> Set a -> Set a
agregar2 n cx |elem n cx = cx 
              |otherwise = n:cx

union2 :: Eq a => Set a -> Set a -> Set a
union2 [] ys = ys
union2 [x:xs] ys |elem x ys = union xs ys
                 |otherwise = x:(union xs ys)

union :: Eq a => Set a -> Set a -> Set a
union [x:xs] ys = agregar x (union xs ys)

partes2 :: Integer -> Set (Set Integer)
partes2 0 = [[]]
partes2 n = agregarATodos n (partes (n-1) ++ partes (n-1) --sirve xq sabemos que no hay conjuntos repetidos entre el primer termino y el segundo

partes :: Integer -> Set (Set Integer)
partes n = union (agregarATodos n (partes (n-1)) (partes (n-1)) 

formarTuplas :: Integer -> Set Integer -> Set (Integer, Integer)
formarTuplas x [] = []
formarTuplas x (y:ys) = agregar2 (x,y) formarTuplas x ys --tb funciona con (x,y):formar...

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano [] _ = []
productoCartesiano (x:xs) ys = union (formarTuplas x ys) (productoCartesiano xs ys)

prefijarATodos :: Integer -> Set [Integer] -> Set [Integer]
prefijarATodos n [] = []
prefijarATodos n (x:xs) = agregar (n:x) (prefijarATodos n xs)

prefijarTodosATodos :: Set Integer -> Set [Integer] -> Set [Integer]
prefijarTodosATodos [] ys = []
prefijarTodosATodos (x:xs) ys = union (prefijarATodos x ys) (prefijarTodosATodos xs ys) --tb podrìa ser sin union y con ++  

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones xs 0 = [[]]
variaciones xs n = prefijarTodosATodos xs (variaciones xs n-1)



