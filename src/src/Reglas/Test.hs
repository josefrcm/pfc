-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: batería de pruebas del módulo Reglas.
-------------------------------------------------------------------------------

module Reglas.Test where


-- Módulos externos
import Test.QuickCheck
import Control.Monad

-- Módulos del programa
import Reglas.Base
import Reglas.Tipos
import Reglas.Zonas
import Reglas.Conexiones
import Reglas.Canonizacion
import Reglas.Formalizacion
import Reglas.Tuneles
import Topologia



-------------------------------------------------------------------------------
-- Métodos para generar datos aleatorios
--------------------------------------------------------------------------------

-- Generación aleatoria de operadores de conexión.
instance Arbitrary Conexion where
	arbitrary = return Directa
	coarbitrary = undefined


-------------------------------------------------------------------------------
-- Funciones auxiliares.
-------------------------------------------------------------------------------

-- Encadena la aplicación de dos reglas
encadenar :: ReglaP -> ReglaP -> Escenario -> Escenario
encadenar f g rs = rs''
	where
	(rs', _) = aplicarR f rs
	(rs'', _) = aplicarR g rs'


-- Prueba que dos reglas sean conmutativas
conmutativas :: ReglaP -> ReglaP -> Escenario -> Bool
conmutativas f g xs = (f `encadenar` g $ xs) == (g `encadenar` f $ xs)


-- Prueba que una regla reduzca la longitud de una lista
reduccion :: ReglaP -> Red -> Bool
reduccion op xs = case op xs of
	Just (xs', _) -> length xs' <= length xs
	Nothing -> True


-- Comprueba que no haya zonas adyacentes del mismo tipo
redundancia :: Red -> Bool
redundancia ((Directa,Z4):(Directa,Z4):_) = False
redundancia ((Directa,Z6):(Directa,Z6):_) = False
redundancia ((Directa,Zd):(Directa,Zd):_) = False
redundancia (_:xs) = redundancia xs
redundancia _ = True



-------------------------------------------------------------------------------
-- Pruebas con QuickCheck.
-------------------------------------------------------------------------------

-- Prueba que la formalización no genere zonas redundantes.
prop_redundancia :: Red -> Bool
prop_redundancia red = redundancia red'
	where
	escenario = (A4, N4, red, N4, A4)
	((_, _, red', _, _), _) = formalizar escenario


-- Comprueba que la creación de zonas siempre reduce la longitud de la red
prop_reduccion :: Red -> Bool
prop_reduccion [] = True
prop_reduccion red = (length red') <= (length red)
	where
	escenario = (A4, N4, red, N4, A4)
	((_, _, red', _, _), _) = formalizar escenario


-- Prueba que los operadores de conexión siempre reduzcan el número de nodos
prop_op1, prop_op2, prop_op3, prop_op4, prop_op5 :: Red -> Bool
prop_op1 = reduccion (conexiones !! 0)
prop_op2 = reduccion (conexiones !! 1)
prop_op3 = reduccion (conexiones !! 2)
prop_op4 = reduccion (conexiones !! 3)
prop_op5 = reduccion (conexiones !! 4)


-- Prueba que las reglas de canonización siempre reduzcan el número de nodos
prop_can1, prop_can2, prop_can3, prop_can4, prop_can5, prop_can6, prop_can7, prop_can8 :: Red -> Bool
prop_can1 = reduccion (canonizaciones !! 0)
prop_can2 = reduccion (canonizaciones !! 1)
prop_can3 = reduccion (canonizaciones !! 2)
prop_can4 = reduccion (canonizaciones !! 3)
prop_can5 = reduccion (canonizaciones !! 4)
prop_can6 = reduccion (canonizaciones !! 5)
prop_can7 = reduccion (canonizaciones !! 6)
prop_can8 = reduccion (canonizaciones !! 7)


-- Ejecuta todos los casos de prueba
probar :: IO ()
probar = do
	putStrLn $ (replicate 80 '-')
	putStr "prop_redundancia... "
	quickCheck prop_redundancia
	putStr "prop_reduccion... "
	quickCheck prop_reduccion

	putStrLn $ (replicate 80 '-')
	putStr "prop_op1... "
	quickCheck prop_op1
	putStr "prop_op2... "
	quickCheck prop_op2
	putStr "prop_op3... "
	quickCheck prop_op3
	putStr "prop_op4... "
	quickCheck prop_op4
	putStr "prop_op5... "
	quickCheck prop_op5

	putStrLn $ (replicate 80 '-')
	putStr "prop_can1... "
	quickCheck prop_can1
	putStr "prop_can2... "
	quickCheck prop_can2
	putStr "prop_can3... "
	quickCheck prop_can3
	putStr "prop_can4... "
	quickCheck prop_can4
	putStr "prop_can1... "
	quickCheck prop_can1
	putStr "prop_can2... "
	quickCheck prop_can2
	putStr "prop_can3... "
	quickCheck prop_can3
	putStr "prop_can4... "
	quickCheck prop_can4



-------------------------------------------------------------------------------
-- Otras pruebas.
-- Nota: terminar de integrar con el resto del programa.
--------------------------------------------------------------------------------

-- Escenarios de ejemplo
test01 :: Red
test01 = [(Directa,Z4) , (Directa,Zd) , (Directa,Z6) , (Directa,Z6)]

test02 :: Red
test02 = [(Directa,Z4) , (Directa,R4_td) , (Directa,Z6) , (Directa,Z6)]

test03 :: Red
test03 = [(Directa,Z4) , (Directa,Zd) , (Directa,Rd_td) , (Directa,Z6) , (Directa,Z6)]

test04 :: Red
test04 = [(Directa,Z4) , (Directa,Zd) , (Directa,Rd_tp) , (Directa,Zd) , (Directa,Zd) , (Directa,Z6)]

test05 :: Red
test05 = [(Directa,Z6) , (Directa,Rd_tptd) , (Directa,Zd) , (Directa,Zd) , (Directa,Z4)]

test06 :: Red
test06 = [(Directa,Z4) , (Directa,Z4) , (Directa,Zd) , (Directa,Z6) , (Directa,Z6)]

test07 :: Red
test07 = [(OpD,Z4) , (OpD,Z4) , (Directa,Z4) , (Directa,Rd_td) , (OpD,Z6)]

test08 :: Red
test08 = [(Directa,Z4) , (Directa,Zd) , (Directa,Z6) , (Directa,Zd) , (Directa,Z4)]

test09 :: Red
test09 = [(Directa,Z4) , (Directa,R4_td) , (Directa,Z6)]

test10 :: Red
test10 = [(Directa,Z4) , (Directa,Rd_td) , (Directa,Z4)]

test11 :: Red
test11 = [(Directa,Z4) , (Directa,Zd) , (Directa,Rd_tp) , (Directa,Z4)]

test12 :: Red
test12 = [(Directa,Z4) , (Directa,Rd_tptd) , (Directa,Zd) , (Directa,Z4)]


-- Escenarios de prueba
escenario1 :: Escenario
escenario1 = (A6, Nd, [(Directa,R4) , (Directa,R4) , (Directa,Rd) , (Directa,R4) , (Directa,Rd_td) , (Directa,R6) , (Directa,R6)], Nd, A6)

escenario2 :: Escenario
escenario2 = (A6, Nd, [(Directa,R4) , (Directa,Rd) , (Directa,R4) , (Directa,Rd) , (Directa,R4) , (Directa,Rd) , (Directa,R4)], Nd, A6)

escenario3 :: Escenario
escenario3 = (A6, Nd, [(Directa,R4), (Directa,Rd), (Directa,Rd_tptd), (Directa,Rd), (Directa,R6)], Nd, A6)

escenario4 :: Escenario
escenario4 = (A4, Nd, [(Directa,R4), (Directa,Rd_td), (Directa,Rd), (Directa,Rd_tptd), (Directa,R4), (Directa,Rd), (Directa,R6), (Directa,Rd), (Directa,R4)], Nd, A4)


-- Prueba del operador 3
e1 :: Escenario
e1 = (A4, N4, [(Directa,R4), (Directa,Rd_td), (Directa,R4)], N4, A4)

e2 :: Escenario
e2 = (A4, N4, [(Directa,R4), (Directa,Rd_td), (Directa,Rd), (Directa,R4)], N4, A4)

e3 :: Escenario
e3 = (A4, N4, [(Directa,R4), (Directa,Rd), (Directa,Rd_td), (Directa,R4)], N4, A4)

e4 :: Escenario
e4 = (A4, N4, [(Directa,R4), (Directa,Rd), (Directa,Rd_td), (Directa,Rd), (Directa,R4)], N4, A4)

e5 :: Escenario
e5 = (A4, N4, [(Directa,R4), (Directa,Rd), (Directa,Rd_td), (Directa,Rd)], N4, A4)

e6 :: Escenario
e6 = (A4, N4, [(Directa,Rd), (Directa,Rd_td), (Directa,Rd), (Directa,R4)], N4, A4)

e7 :: Escenario
e7 = (A4, N4, [(Directa,Rd), (Directa,Rd_td), (Directa,Rd)], N4, A4)



-- -------------------------------------------------------------------------------
-- -- Simplificación del grafo
-- -------------------------------------------------------------------------------
-- 
-- type Grafo = IntMap [Int]
-- type Test = (String, Int, Int, Grafo)
-- 
-- 
-- merge :: Ord a => [a] -> [a] -> [a]
-- merge [] ys = ys
-- merge xs [] = xs
-- merge (x:xs) (y:ys)
-- 	| x<y = x : merge xs (y:ys)
-- 	| x>y = y : merge (x:xs) ys
-- 	| otherwise = x : merge xs ys
-- 
-- 
-- fusionar :: Grafo -> Grafo -> Grafo
-- fusionar g1 g2 = unionWith (++) g1 g2
-- 
-- 
-- -------------------------------------------------------------------------------
-- -- Generación de topologías de ejemplo
-- -------------------------------------------------------------------------------
-- 
-- lineal :: Int -> Grafo
-- lineal n = insertarLista vacio [(x, x+1) | x <- [1 .. n-1]]
-- 
-- anillo :: Int -> Grafo
-- anillo n = if n < 1 then vacio else insertar (lineal n) (1,n)
-- 
-- estrella :: Int -> Grafo
-- estrella n = insertarLista vacio [(n+1,x) | x <- [1 .. n]]
-- 
-- malla :: Int -> Int -> Grafo
-- malla x y = insertarLista vacio (a ++ b)
-- 	where
-- 	a = [(indice i j, 1 + indice i j) | i <- [0 .. y-1], j <- [0 .. x-2]]
-- 	b = [(indice i j, x + indice i j) | i <- [0 .. y-2], j <- [0 .. x-1]]
-- 	indice i j = j + i*x + 1
-- 
-- 
-- -------------------------------------------------------------------------------
-- -- Batería de pruebas
-- -------------------------------------------------------------------------------
-- 
-- -- Topologías simples
-- simple1, simple2, simple3, simple4, simple5, simple6 :: Test
-- simple1 = ("Lineal 3", 1, 3, lineal 3)
-- simple2 = ("Lineal 4", 1, 4, lineal 4)
-- simple3 = ("Lineal 5", 1, 5, lineal 5)
-- simple4 = ("Anillo 3", 1, 2, anillo 3)
-- simple5 = ("Anillo 4", 1, 2, anillo 4)
-- simple6 = ("Anillo 5", 1, 2, anillo 5)
-- simple7 = ("Malla 3x3", 1, 9, malla 3 3)
-- simple8 = ("Malla 4x4", 1, 16, malla 4 4)
-- simple9 = ("Malla 5x5", 1, 25, malla 5 5)
-- 
-- -- Topologías complejas
-- complex1, complex2, complex3 :: Test
-- complex1 = ("Anillo + estrella", 1, 3, fusionar (anillo 6) (estrella 6))
-- complex2 = ("8", 1, 8, insertar (anillo 8) (3,7))
-- complex3 = ("Cosa rara", 1, 3, insertarLista vacio [(1,2),(2,3),{-(2,4),(2,5),(4,5),-}(1,4),(2,4)])
-- 
-- -- Ejecuta una prueba
-- probar :: (String, Int, Int, Grafo) -> (String, Grafo)
-- probar (name, src, dst, graph) = (name, simplificar src dst graph)
-- 
-- 
-- 
-- -------------------------------------------------------------------------------
-- -- Lo que falta
-- -------------------------------------------------------------------------------
-- 
-- main = do
-- 	-- Pruebas con topologías simples
-- 	mapM_ (print . probar) [simple1, simple2, simple3, simple4, simple5, simple6, simple7, simple8, simple9]
-- 
-- 	-- Pruebas con topologías complejas
-- 	mapM_ (print . probar) [complex1, complex2, complex3]
-- 
