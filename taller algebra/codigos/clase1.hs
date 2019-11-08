f x y = x * x + y * y -- f calcula suma de cuadrados
g x y z = x + y + z * z
doble x = 2 * x
suma x y = x + y
normaVectorial x1 x2 = sqrt (f x1 x2)
funcionConstante8 x = 8
respuestaATodo = 42

h n | n == 0 = 1
    | n/= 0 = 0 

t n | n == 0 = 1
    | otherwise = 0 

hi n | n/= 0 = 0 
     | n == 0 = 1

ti n | otherwise = 0 -- da siempre 0 para todo n
     | n == 0 = 1

signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1  -- otra opcion es poner otherwise en esta lìnea

absoluto n | n >= 0 = n
           | n < 0 = -n  -- otra opcion es poner otherwise en esta lìnea

absolutob n = (signo n) * n

maximo n m | n < m = m
	   | n >= m = n  -- otra opcion es poner otherwise en esta lìnea

maximo3 n m v | v >= (maximo n m) = v
	      | n >= (maximo v m) = n
	      | m >= (maximo n v) = m

maximo3b n m v | v >= (maximo n m) = v
	       | otherwise = (maximo n m) 


maximo3c n m v = maximo (maximo n m) v

