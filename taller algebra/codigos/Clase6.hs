module Clase6
where
import Clase5

negLogica :: Bool -> Bool
negLogica x | x == True = False
            | x == False = True

negLogica2 :: Bool -> Bool
negLogica2 True = False  --patròn True
negLogica2 False = True  --patròn False

factorial3 :: Integer -> Integer
factorial3 0 = 1
factorial3 n = n * factorial3 ( n - 1)

esLaRespuestaATodo :: Integer -> Bool
esLaRespuestaATodo 42 = True
esLaRespuestaATodo _ = False

sumaVectorial :: ( Float , Float ) -> ( Float , Float ) -> ( Float , Float )  -- Dominio R2 x R2 toma dos vectores R2
sumaVectorial t1 t2 = ( fst t1 + fst t2 , snd t1 + snd t2 )

sumaVectorial2 :: ( Float , Float ) -> ( Float , Float ) -> ( Float , Float )
sumaVectorial2 (x1,y1) (x2,y2) = (x1+x2,y1+y2) --- matcheo cada vector con el formato x1,y1

yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _= False

oLogico :: Bool -> Bool -> Bool
oLogico False False = False 
oLogico _ _ = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _= True

sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n-1)

algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (0,_,_) = True  -- podria poner (o,y,z) pero no es necesario xq no hacemos cuentas
algunoEsCero (_,0,_) = True
algunoEsCero (_,_,0) = True
algunoEsCero _ = False  -- (_,_,_) tb es valido

algunoEsCero1 :: (Integer, Integer, Integer) -> Bool
algunoEsCero1 (x,y,z) = x==0 || y==0 || z==0

algunoEsCero2 :: (Integer, Integer, Integer) -> Bool
algunoEsCero2 (x,y,z) = x*y*z == 0

productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1,y1) (x2,y2) = x1*x2 + y1*y2 

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0 
sumaDigitos n =  mod n 10 + sumaDigitos (div n 10)

digitosIguales :: Integer -> Bool
digitosIguales n | 0 <= n && n <= 9 = True 
                 | otherwise = mod n 10 == mod (div n 10) 10 && digitosIguales (div n 10) 

esSumaDeDosPrimosHasta :: Integer -> Integer -> Bool
esSumaDeDosPrimosHasta n m | m==2 = esPrimo (n-m)
                           | otherwise = esPrimo m && esPrimo (n-m) || esSumaDeDosPrimosHasta n (m-1)    

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos n = esSumaDeDosPrimosHasta n (n-2)

goldbach :: Integer -> Bool
goldbach n | n == 4 = True
           | otherwise = esSumaDeDosPrimos n && goldbach (n-2) --donde el n que quiero probar es 4*10^18 
