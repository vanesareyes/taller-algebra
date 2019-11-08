module Clase3
(esPar)  --esta lìnea me permite seleccionar las funciones exportables, si no especifico nada importo todo
where


resta :: Integer -> Integer -> Integer
resta x y = x - y

suma :: Integer -> Integer -> Integer
suma x y = x + y

negar :: Integer -> Integer
negar x = -x

suc :: Integer -> Integer
suc x = x + 1

inv :: Float -> Float
inv x | x /= 0 = 1/x 

--funciòn parcial porque para el 0 devuelve bottom
--  | x == 0 = undefined 
--  | x == 0 = error "no se puede" devuelve error con cartel no se puede
--  | x == 0 = 0 -- la converti en funciòn completa

--inv2 :: Float -> Float
--inv2 x = 1/x  -- si evaluo esta funciòn en 0 me devuelve infinity que es un valor posible del tipo float pero que en verdad no existe en los reales (float es la mejor implementacion posible de los reales pero no es exacta, tratar de evitar infinity en los càlculos colocando guardas)


unidades :: Integer -> Integer
unidades x = mod (abs x) 10

--unidades x |x >=0 = mod x 10   -- devuelve el ùltimo dìgito significativo
--           |x < 0 = resta 10 (mod x 10)  -- mod (-x ) 10 o 

sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 a b c = (unidades a) + (unidades b) + (unidades c)  

todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares a b c | mod a 2 /= 0 && mod b 2 /= 0 && mod c 2 /= 0 = True
                   | otherwise = False  

-- Definimos funciòn esPar
esPar :: Integer -> Bool
esPar x = mod x 2 == 0

paridad :: Integer -> Integer
paridad x = mod x 2  -- 0 si x es par y 1 si x es impar

todosImpares2 :: Integer -> Integer -> Integer -> Bool
todosImpares2 a b c = not (esPar a) && not (esPar b) && not (esPar c) 

todosImpares3 :: Integer -> Integer -> Integer -> Bool
todosImpares3 a b c = not (esPar (a * b * c)) -- el producto de 3 impares da impar

todosImpares4 :: Integer -> Integer -> Integer -> Bool
todosImpares4 a b c = paridad a + paridad b + paridad c == 3

alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar a b c | mod a 2 /= 0 ||  mod b 2 /= 0 || mod c 2 /= 0 = True
                     | otherwise = False  

alMenosUnImpar2 :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar2 a b c = not (esPar a) || not (esPar b) || not (esPar c) 

alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares a b c | (mod b 2 /= 0 || mod c 2 /= 0) && mod a 2 /= 0 = True
                        | mod b 2 /= 0 && mod c 2 /= 0 = True
                        | otherwise = False  

alMenosDosImpares2 :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares2 a b c = paridad a + paridad b + paridad c >= 2

alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares a b c = paridad a + paridad b + paridad c <= 1

alMenosDosPares2 :: Integer -> Integer -> Integer -> Bool
alMenosDosPares2 a b c = not (alMenosDosImpares a b c)

r1 :: Integer -> Integer -> Bool
r1 a b = paridad a == paridad b

r2 :: Integer -> Integer -> Bool
r2 a b = mod (2*a + 3*b) 5 == 0

r3 :: Integer -> Integer -> Bool
r3 a b = unidades a /= unidades b && unidades b /= unidades (a*b) && unidades a /= unidades (a*b)


