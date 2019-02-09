-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: búsqueda de rutas en un grafo.
-------------------------------------------------------------------------------

module Topologia.Rutas (
	buscarRutas
	) where


-- Módulos externos
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set

-- Módulos del programa
import Topologia.Grafo
import Topologia.Tipos



-------------------------------------------------------------------------------
-- Tipos de datos
--------------------------------------------------------------------------------

-- | Representación del grafo desenrollado en un árbol.
data Arbol = Rama {
	padre :: !Id,
	hijos :: [Arbol]
	}



-------------------------------------------------------------------------------
-- Búsqueda de caminos
--------------------------------------------------------------------------------

-- Convierte el grafo en un árbol.
desenrollar :: Id -> Grafo -> Arbol
desenrollar src grafo = loop Set.empty src
	where
	loop :: Set.IntSet -> Id -> Arbol
	loop visitados nodo = Rama nodo (map (loop visitados') adyacentes)
		where
		-- Nodos adyacentes al actual, que no hayan sido visitados antes.
		adyacentes = [a | arco <- (arcos grafo) Map.! nodo,
			let a = destino arco, a `Set.notMember` visitados]
		
		-- Añade el nodo actual al conjunto de nodos visitados.
		visitados' = Set.insert nodo visitados


-- Explora el árbol, anotando todas las posibles rutas desde la raíz hasta
-- las hojas, y eliminando aquellas que no lleguen al nodo destino.
buscarDestino :: Id -> Arbol -> [Ruta]
buscarDestino dst (Rama p h) = if p == dst
	then [[p]]
	else map (p:) (concatMap (buscarDestino dst) h)


-- Devuelve una lista con el tipo de los routers por los que pasa una ruta.
protocolos :: Grafo -> Ruta -> [Router]
protocolos _ [] = []
protocolos grafo (p:ps) = protocolo nodo : protocolos grafo ps
	where
	nodo = (nodos grafo) Map.! p


-- Busca todas las rutas entre dos nodos, aplicando las optimizaciones deseadas:
--   long_max    -> Limitación de la longitud máxima de las rutas.
--   elim_bucles -> Eliminación de bucles.
--   forzar_ady  -> Fuerza a que las rutas pasen por nodos adyacentes.
buscarRutas :: (Maybe Int, Bool, Bool) -> Grafo -> Id -> Id -> [([Id], [Router])]
buscarRutas (long_max, elim_bucles, forzar_ady) grafo src dst = paso6
	where
	paso1 = if elim_bucles then simplificar src dst grafo else grafo
	paso2 = desenrollar src paso1
	paso3 = case long_max of
		Nothing -> paso2
		Just lm -> limitarLongitud (lm + rutaMasCorta dst paso2) paso2
	paso4 = if forzar_ady then forzarAdyacentes paso3 else paso3
	paso5 = buscarDestino dst paso4
	paso6 = [(r1,r2) | r1 <- paso5, let r2 = protocolos grafo r1]



-------------------------------------------------------------------------------
-- Simplificación del grafo
-------------------------------------------------------------------------------

-- Devuelve el conjunto de nodos adyacentes al conjunto de nodos dado.
conjAdyacentes :: Grafo -> Set.IntSet -> Set.IntSet
conjAdyacentes grafo ks = Set.fromList $ concatMap adyacentes (Set.toList ks)
	where
	adyacentes :: Int -> [Int]
	adyacentes key = case (Map.lookup key (arcos grafo)) of
		Nothing -> error "Alcanzado nodo sin adyacentes, esto no debería darse nunca"
		Just xs -> map destino xs


-- Intenta eliminar los bucles de un grafo.
simplificar :: Int -> Int -> Grafo -> Grafo
simplificar src dst grafo = loop (Set.singleton src) (Set.singleton dst) Set.empty inicial
	where
	inicial :: Grafo
	inicial = vacio {dirigido = True} `insertarNodos` (Map.elems $ nodos grafo)

	loop :: Set.IntSet -> Set.IntSet -> Set.IntSet -> Grafo -> Grafo
	loop forward backwards visited acc
		| (Set.null forward) && (Set.null backwards) = acc
		| otherwise = loop forward' backwards' visited' acc'
			where
			-- 'Forward' es el conjunto de nodos que fluyen hacia adelante,
			-- alejándose del origen, y 'Backwards' es el conjunto de nodos que
			-- fluyen hacia atrás, alejándose del destino. 'Visited' es el conjunto
			-- de nodos que ya han sido visitados, y 'Acc' el nuevo grafo que se
			-- está creando.
			forward'   = (conjAdyacentes grafo forward)   `Set.difference` visited'
			backwards' = (conjAdyacentes grafo backwards) `Set.difference` visited'
			visited'   = visited `Set.union` forward `Set.union` backwards
			acc'       = acc `insertarArcos` (a ++ b)

			-- Enlaces que parten de los nodos del conjunto 'forward',
			-- y no llegan a nodos ya visitados
			a :: [Arco]
			a = [arco | nodo <- Set.toList forward,
				arco <- (arcos grafo) Map.! nodo,
				Set.notMember (destino arco) visited]

			-- Enlaces que llegan a los nodos del conjunto 'backwards',
			-- y no parten de nodos ya visitados
			b :: [Arco]
			b = [arco | nodo <- Set.toList backwards,
				arco <- (arcos grafo) Map.! nodo,
				Set.notMember (origen arco) visited]



-------------------------------------------------------------------------------
-- Criterios de poda
--------------------------------------------------------------------------------

-- Busca la ruta más corta
rutaMasCorta :: Id -> Arbol -> Int
rutaMasCorta dst rama = loop (expandir 0 rama)
	where
	-- Recorre la lista generada por el recorrido, parando cuando encuentra
	-- la primera aparición del destino, y devolviendo la profundidad mínima
	-- a la que se encuentra.
	loop :: [(Id, Int)] -> Int
	loop [] = 0
	loop ((nodo, nivel) : resto) = if nodo == dst then nivel else loop resto

	-- Realiza un recorrido en anchura del árbol, anotando a qué profundidad
	-- se encuentra cada nodo.
	expandir :: Int -> Arbol -> [(Id, Int)]
	expandir nivel (Rama p []) = [(p, nivel)]
	expandir nivel (Rama p h) = (p, nivel) : concatMap (expandir (nivel+1)) h


-- Limita la longitud máxima de las rutas, podando el árbol a la longitud establecida.
limitarLongitud :: Int -> Arbol -> Arbol
limitarLongitud 1 (Rama p _) = Rama p []
limitarLongitud n (Rama p h) = Rama p (map (limitarLongitud (n-1)) h)


-- Fuerza a que si dos nodos A y B son adyacentes, todas las rutas
-- que pasen por esos dos nodos usen el enlace directo.
forzarAdyacentes :: Arbol -> Arbol
forzarAdyacentes arbol = podar Set.empty arbol
	where
	podar :: Set.IntSet -> Arbol -> Arbol
	podar visitados r = r {hijos = map (podar visitados') hijos'}
		where
		hijos' = filter (\h -> padre h `Set.notMember` visitados) (hijos r)
		adyacentes = Set.fromList $ map padre (hijos r)
		visitados' = Set.union visitados adyacentes
