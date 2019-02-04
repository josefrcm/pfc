-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: batería de pruebas del módulo Topología.
-------------------------------------------------------------------------------

module Topologia.Test (
	probar
	) where


-- Módulos externos
import Control.Monad
import Data.Char
import Data.Map
import Test.QuickCheck


-- Módulos del programa
import Topologia.GML
import Topologia.Grafo
import Topologia.Parser
import Topologia.Tipos



-------------------------------------------------------------------------------
-- Métodos para generar datos aleatorios
--------------------------------------------------------------------------------

-- Generación de un carácter de texto.
instance Arbitrary Char where
	arbitrary     = oneof [
		choose ('0', '9'),
		choose ('A', 'Z'),
		choose ('a', 'z')]
	coarbitrary c = variant (ord c `rem` 4)


-- Genera una clave con el formato [a-zA-Z][a-zA-Z0-9]*
genKey :: Gen String
genKey = do
	h <- oneof [choose ('a', 'z'), choose ('A', 'Z')]
	t <- arbitrary
	return (h:t)


-- Genera un valor del árbol sintáctico. El parámetro n es la profundidad
-- del árbol: 0 para elementos terminales (números y cadenas de texto),
-- y k para listas de pares clave-valor. El valor de n se decrementa al
-- generar valores hijos, para no entrar en una recursividad infinita.
genValue :: Int -> Gen Valor
genValue 0 = oneof [
	liftM VI arbitrary,
	liftM VR arbitrary,
	liftM VS arbitrary]
genValue n = do
	key <- genKey
	value <- genValue (n `div` 2)
	return $ VM (singleton key [value])


-- Genera una lista de pares clave-valor.
genAtrib :: Gen Atributos
genAtrib = do
	key <- genKey
	value <- sized genValue
	return $ singleton key [value]


-- Generación aleatoria de árbol sintácticos GML.
instance Arbitrary Valor where
	arbitrary = return VM `ap` genAtrib
	coarbitrary = undefined


-- Generación aleatoria de vectores.
instance Arbitrary Vector where
	arbitrary = return Vector `ap` arbitrary `ap` arbitrary `ap` arbitrary
	coarbitrary = undefined


-- Generación aleatoria de atributos gráficos.
instance Arbitrary Graficos where
	arbitrary = return Graficos `ap` arbitrary `ap` genAtrib
	coarbitrary = undefined


-- Generación aleatoria de tipos de router.
instance Arbitrary Router where
	arbitrary = oneof (Prelude.map return [R4, R6, Rd, R4_td, Rd_td, Rd_tp, Rd_tptd])
	coarbitrary = undefined


-- Generación aleatoria de nodos.
instance Arbitrary Nodo where
	arbitrary = return Nodo `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` genAtrib
	coarbitrary = undefined


-- Generación aleatoria de arcos.
instance Arbitrary Arco where
	arbitrary = return Arco `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` genAtrib
	coarbitrary = undefined


-- Generación aleatoria de grafos.
instance Arbitrary Grafo where
	arbitrary = sized $ \nnodos -> do
		let id_nodos = [1 .. nnodos + 1]
		id_arcos <- liftM concat $ forM id_nodos $ \i -> do
			narcos <- arbitrary :: Gen Int
			return [(i, j) | j <- [1 .. narcos]]
		
		-- Genera el grafo
		dirigido' <- arbitrary
		atribs    <- genAtrib
		nodos'    <- forM id_nodos $ \i -> do
			n <- arbitrary
			return (n {ident = i})
		arcos'    <- forM id_arcos $ \(i,j) -> do
			a <- arbitrary
			return (a {origen = i, destino = j})
		
		return $ vacio {dirigido = dirigido', atr_grafo = atribs} `insertarNodos` nodos' `insertarArcos` arcos'
	coarbitrary = undefined



-------------------------------------------------------------------------------
-- Implementación de las pruebas
--------------------------------------------------------------------------------

-- Prueba que al convertir un tipo a un árbol sintáctico GML y otra vez al tipo
-- original, el resultado obtenido sea el mismo.
probGML :: (Eq a, GML a) => a -> Bool
probGML a = (fromGML . toGML) a == a


-- Prueba que al convertir un tipo a texto y otra vez al tipo original,
-- el resultado obtenido sea el mismo.
probTexto :: (Eq a, GML a) => a -> Bool
probTexto a = (fromGML . leerGML "" . escribirGML . toGML) a == a


-- Ejecuta una prueba, añadiendo una cabecera al principio de la línea.
verificar :: Testable a => String -> a -> IO ()
verificar s t = do
	putStr (s ++ "... ")
	quickCheck t


-- Ejecuta todas las pruebas.
probar :: IO ()
probar = do
	-- Conversiones de tipos a GML
	putStrLn (replicate 80 '-')
	verificar "Vector -> GML -> Vector"     (probGML :: Vector -> Bool)
	verificar "Graficos -> GML -> Graficos" (probGML :: Graficos -> Bool)
	verificar "Nodo -> GML -> Nodo"         (probGML :: Nodo -> Bool)
	verificar "Arco -> GML -> Arco"         (probGML :: Arco -> Bool)
	verificar "Grafo -> GML -> Grafo"       (probGML :: Grafo -> Bool)

	-- Conversiones de tipos a texto
	putStrLn (replicate 80 '-')
	verificar "Vector -> Texto -> Vector"     (probTexto :: Vector -> Bool) 
	verificar "Graficos -> Texto -> Graficos" (probTexto :: Graficos -> Bool)
	verificar "Nodo -> Texto -> Nodo"         (probTexto :: Nodo -> Bool)
	verificar "Arco -> Texto -> Arco"         (probTexto :: Arco -> Bool)
	verificar "Grafo -> Texto -> Grafo"       (probTexto :: Grafo -> Bool)
