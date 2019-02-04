-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: implementación de las operaciones de creación, modificación
--              y consulta de grafos.
-------------------------------------------------------------------------------

module Topologia.Grafo where


-- Módulos externos
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

-- Módulos del programa
import Topologia.Tipos



-------------------------------------------------------------------------------
-- Tipos de datos.
-------------------------------------------------------------------------------

-- Definición de los grafos.
data Grafo = Grafo {
	nodos :: IntMap.IntMap Nodo,
	arcos :: IntMap.IntMap [Arco],
	dirigido :: Bool,
	atr_grafo :: Atributos
	}
	deriving Eq


-- Instanciación de los grafos para la clase Show (depuración)
instance Show Grafo where
	show g = dirigido' ++ nodos' ++ arcos'
		where
		dirigido' = if dirigido g then "-> Dirigido\n" else "-> No dirigido\n"
		nodos' = concat [show s ++ "\n" | s <- IntMap.elems $ nodos g]
		arcos' = concat [show s ++ "\n" | s <- concat $ IntMap.elems $ arcos g] 



-------------------------------------------------------------------------------
-- Operaciones de creación del grafo.
-------------------------------------------------------------------------------

-- Crea un grafo vacío.
vacio :: Grafo
vacio = Grafo IntMap.empty IntMap.empty False Map.empty


-- Devuelve un nuevo identificador no usado para un nodo.
nuevoId :: Grafo -> Id
nuevoId grafo = if null keys then 1 else 1 + maximum keys
	where
	keys = IntMap.keys (nodos grafo)



-------------------------------------------------------------------------------
-- Inserción de elementos.
-------------------------------------------------------------------------------

-- Añade un nuevo nodo al grafo. Si el nodo ya existía, lanza una excepción.
insertarNodo :: Grafo -> Nodo -> Grafo
insertarNodo grafo n = if (ident n) `IntMap.member` (nodos grafo)
	then error $ "Nodo " ++ show (ident n) ++ " duplicado"
	else grafo {nodos = nodos', arcos = arcos'}
	where
		nodos' = IntMap.insert (ident n) n (nodos grafo)
		arcos' = IntMap.insert (ident n) [] (arcos grafo)


-- Añade una lista de nodos al grafo.
insertarNodos :: Grafo -> [Nodo] -> Grafo
insertarNodos = foldl insertarNodo


-- Añade un nuevo arco al grafo, y devuelve el grafo modificado. Si los nodos
-- de origen y destino no existen, no hace nada, devolviendo el grafo original.
insertarArco :: Grafo -> Arco -> Grafo
insertarArco grafo arco
	| ((origen arco) `IntMap.member` (arcos grafo)) && ((destino arco) `IntMap.member` (arcos grafo)) =
 		if dirigido grafo
			then grafo `insertar` arco
			else grafo `insertar` arco `insertar` arco {origen = destino arco, destino = origen arco}
	| otherwise = grafo
	where
	insertar :: Grafo -> Arco -> Grafo
	insertar g a = g {arcos = arcos'}
		where
		arcos' = IntMap.insertWith (merge) (origen a) [a] (arcos g)


-- Añade una lista de arcos al grafo.
insertarArcos :: Grafo -> [Arco] -> Grafo
insertarArcos = foldl insertarArco



-------------------------------------------------------------------------------
-- Borrado de elementos.
-------------------------------------------------------------------------------

-- Borra un nodo del grafo. Si el nodo no existe, lanza una excepción.
borrarNodo :: Grafo -> Id -> Grafo
borrarNodo grafo n = if n `IntMap.member` (nodos grafo)
	then grafo' {nodos = nodos', arcos = arcos''}
	else error $ "El nodo " ++ show n ++ " no existe"
	where
		-- Obtenemos la lista de los nodos adyacentes
		adyacentes' = if dirigido grafo
			then listaNodos grafo
			else listaNodos grafo --map destino ((arcos grafo) IntMap.! n)

		-- Borramos los enlaces de los nodos adyacentes que apunten al nodo borrado
		grafo' = borrarArcos grafo [(src, n) | (src, _) <- adyacentes']

		-- Borramos el nodo
		nodos' = IntMap.delete n (nodos grafo')
		arcos'' = IntMap.delete n (arcos grafo')


-- Borra una lista de nodos.
borrarNodos :: Grafo -> [Id] -> Grafo
borrarNodos = foldl borrarNodo


-- Borra el arco entre dos nodos dados, devolviendo el grafo modificado.
borrarArco :: Grafo -> (Id, Id) -> Grafo
borrarArco grafo (src, dst) = if dirigido grafo
	then grafo `borrar` (src, dst)
	else grafo `borrar` (src, dst) `borrar` (dst, src)
	where
	borrar :: Grafo -> (Id, Id) -> Grafo
	borrar g (src', dst') = g {arcos = arcos'}
		where
		arcos' = IntMap.adjust (filter (\a -> destino a /= dst')) src' (arcos g)


-- Borra una lista de arcos.
borrarArcos :: Grafo -> [(Id, Id)] -> Grafo
borrarArcos = foldl borrarArco



-------------------------------------------------------------------------------
-- Funciones de actualización.
-------------------------------------------------------------------------------

-- Actualiza el nodo 'key', aplicándole una función. El identificador no se
-- modifica en ningún caso.
actualizarNodo :: Grafo -> Id -> (Nodo -> Nodo) -> Grafo
actualizarNodo grafo key func = grafo {nodos = nodos'}
	where
	func' nodo = case nodo of
		Nothing -> Nothing
		Just n  -> Just $ (func n) {ident = ident n}
	nodos' = IntMap.alter func' key (nodos grafo)


-- Actualiza una lista de nodos, aplicándoles a todos la misma función.
actualizarNodos :: Grafo -> [Id] -> (Nodo -> Nodo) -> Grafo
actualizarNodos grafo keys func = foldl (\g k -> actualizarNodo g k func) grafo keys 


-- Actualiza el arco 'src'-'dst', aplicándole una función. El origen y el
-- destino no se modifican en ningún caso.
actualizarArco :: Grafo -> (Id, Id) -> (Arco -> Arco) -> Grafo
actualizarArco grafo (src, dst) func = grafo {arcos = arcos3}
	where
	arcos1 = arcos grafo
	arcos2 = IntMap.alter (actualizar (src, dst) func) src arcos1
	arcos3 = if dirigido grafo
		then arcos2
		else IntMap.alter (actualizar (dst, src) func) dst arcos2 
	
	--arco' = arco {origen=destino arco, destino=origen arco}
	
	actualizar :: (Id, Id) -> (Arco -> Arco) -> Maybe [Arco] -> Maybe [Arco]
	actualizar _ _ Nothing = Nothing
	actualizar (origen', destino') func (Just xs) = Just (bucle xs)
		where
		bucle :: [Arco] -> [Arco]
		bucle [] = []
		bucle (x:xs)
			| ((origen x) == origen') && ((destino x) == destino') = (func x) : xs
			| otherwise = x : bucle xs 


-- Actualiza una lista de arcos, aplicándoles a todos la misma función.
actualizarArcos :: Grafo -> [(Id, Id)] -> (Arco -> Arco) -> Grafo
actualizarArcos grafo keys func = foldl (\g k -> actualizarArco g k func) grafo keys



-------------------------------------------------------------------------------
-- Búsqueda de elementos.
-------------------------------------------------------------------------------

-- Busca un nodo por su identificador.
buscarNodo :: Grafo -> Id -> Nodo
buscarNodo g n = (nodos g) IntMap.! n


-- Busca un arco según el identificador de los nodos de origen y destino.
buscarArco :: Grafo -> (Id, Id) -> Arco
buscarArco g (src, dst) = head $ filter (\x -> destino x == dst) $ (arcos g) IntMap.! src


-- Busca todo los arcos que parten de un nodo.
buscarArcos :: Grafo -> Id -> [Arco]
buscarArcos g n = (arcos g) IntMap.! n


-- Devuelve una lista con todos los nodos del grafo.
listaNodos :: Grafo -> [(Id, Nodo)]
listaNodos g = map (\n -> (ident n, n)) (IntMap.elems $ nodos g)


-- Devuelve una lista con todos los arcos del grafo.
listaArcos :: Grafo -> [((Id, Id), Arco)]
listaArcos g = map (\a -> ((origen a, destino a), a)) (concat $ IntMap.elems $ arcos g)


-- Busca el nodo que se encuentre a menos de la distancia 'radio' del
-- punto 'pos'. Si no encuentra nada, devuelve Nothing.
buscarNodoCoord :: Grafo -> Vector -> Double -> Maybe Id
buscarNodoCoord g pos radio = if Prelude.null ids then Nothing else Just (head ids)
	where
	-- Busca los nodos que se encuentran entre las coordenadas establecidas.
	ids = [k | (k, n) <- IntMap.toList (nodos g),
		distancia_vv (center (graficos n)) pos <= (radio*radio)]


-- Busca el arco que se encuentre a menos de la distancia 'radio' del
-- punto 'pos'. Si no encuentra nada, devuelve Nothing.
buscarArcoCoord :: Grafo -> Vector -> Double -> Maybe (Id, Id)
buscarArcoCoord g pos radio = listToMaybe $ List.sort ids
	where
	-- Busca los arcos que se encuentren dentro de las coordenadas
	-- establecidas. Se eliminan los arcos que queden fuera del espacio
	-- comprendido entre dos nodos.
	ids = [(src, dst) | ((src, dst), _) <- listaArcos g,
		let p1 = center $ graficos $ (nodos g) IntMap.! src,
		let p2 = center $ graficos $ (nodos g) IntMap.! dst,
		dot (pos .- p1) (p2 .- p1) >= 0,
		dot (pos .- p2) (p1 .- p2) >= 0,
		distancia_lv (p1, p2) pos <= radio]



-------------------------------------------------------------------------------
-- Otras operaciones sobre grafos
-------------------------------------------------------------------------------

-- Área ocupada por el grafo.
areaGrafo :: Grafo -> Vector
areaGrafo g = if Prelude.null nodos' then Vector 0.0 0.0 0.0 else Vector xrange yrange zrange
	where
	nodos' = IntMap.elems (nodos g)
	lista_x = [x (center (graficos n)) | n <- nodos']
	lista_y = [y (center (graficos n)) | n <- nodos']
	lista_z = [z (center (graficos n)) | n <- nodos']
	xrange = (maximum lista_x) - (minimum lista_x)-- + 2.0 * desp_base
	yrange = (maximum lista_y) - (minimum lista_y)-- + 2.0 * desp_base
	zrange = (maximum lista_z) - (minimum lista_z)-- + 2.0 * desp_base


-- Centro del grafo en el espacio de coordenadas.
centroGrafo :: Grafo -> Vector
centroGrafo grafo = if null pos then Vector 0.0 0.0 0.0 else Vector cx cy cz
	where
	pos = map (center . graficos) (IntMap.elems (nodos grafo))
	px = map x pos
	py = map y pos
	pz = map z pos
	cx = (sum px) / (fromIntegral $ length px)	
	cy = (sum py) / (fromIntegral $ length py)
	cz = (sum pz) / (fromIntegral $ length pz)


-- Devuelve la coordenadas de un nodo.
coordenadasNodo :: Grafo -> Id -> Vector
coordenadasNodo g key = center (graficos (nodos g IntMap.! key))


-- Convierte una secuencia de nodos en una secuencia de arcos.
camino :: Grafo -> [Id] -> [Arco]
camino grafo (n1:n2:ns) = arco : camino grafo (n2:ns)
	where
	arco = head [a | a <- (arcos grafo) IntMap.! n1, destino a == n2]
camino _ _ = []


-- Desplaza todos los nodos de la lista 'ids' según el vector 'delta'.
moverNodos :: Grafo -> [Id] -> Vector -> Grafo
moverNodos grafo ids delta = grafo {nodos = nodos'}
	where
	nodos' = IntMap.mapWithKey (\k n -> if k `elem` ids then desplazarNodo n else n) (nodos grafo)

	desplazarNodo :: Nodo -> Nodo
	desplazarNodo nodo = nodo {graficos = g'}
		where
		g = graficos nodo
		g' = g {center = center g .+ delta}



-------------------------------------------------------------------------------
-- Operaciones sobre vectores.
-------------------------------------------------------------------------------

-- Suma de vectores.
(.+) :: Vector -> Vector -> Vector
(.+) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)


-- Resta de vectores.
(.-) :: Vector -> Vector -> Vector
(.-) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1-x2) (y1-y2) (z1-z2)


-- Multiplicación de un vector por un escalar.
(.*) :: Vector -> Double -> Vector
(.*) (Vector x1 y1 z1) s = Vector (s*x1) (s*y1) (s*z1)


-- División de un vector por un escalar.
(./) :: Vector -> Double -> Vector
(./) (Vector x1 y1 z1) s = Vector (x1/s) (y1/s) (z1/s)


-- Producto escalar de dos vectores.
dot :: Vector -> Vector -> Double
dot v1 v2 = (x v1)*(x v2) + (y v1)*(y v2) + (z v1)*(z v2)


-- Producto vectorial de dos vectores.
cross :: Vector -> Vector -> Vector
cross v1 v2 = Vector {
	x = (y v1 * z v2) - (z v1 * y v2),
	y = (z v1 * x v2) - (x v1 * z v2),
	z = (x v1 * y v2) - (y v1 * x v2)
	}


-- Normaliza un vector para hacerlo unitario.
norm :: Vector -> Double
norm v = sqrt (dot v v)


-- Distancia entre dos puntos.
distancia_vv :: Vector -> Vector -> Double
distancia_vv c1 c2 = dot delta delta
	where
	delta = c2 .- c1


-- Distancia entre un punto y una línea.
distancia_lv :: (Vector, Vector) -> Vector -> Double
distancia_lv (p1, p2) p0  = a / b
	where
	a = norm $ cross (p2 .- p1) (p1 .- p0)
	b = norm $ p2 .- p1


-- Distancia entre un punto y un plano.
distancia_pv :: (Vector, Vector) -> Vector -> Double
distancia_pv (n, p) v = (a * x v) + (b * y v) + (c * z v) + d
	where
	a = x n
	b = y n
	c = z n
	d = - (dot n p)



-------------------------------------------------------------------------------
-- Funciones auxiliares.
-------------------------------------------------------------------------------

-- Fusiona dos listas ordenadas, devolviendo una nueva lista ordenada.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
	| x<y = x : merge xs (y:ys)
	| x>y = y : merge (x:xs) ys
	| otherwise = x : merge xs ys


-- Permite ordenar los arcos, primero por origen y luego por destino.
instance Ord Arco where
	compare a1 a2 = case compare (origen a1) (origen a2) of
		LT -> LT
		GT -> GT
		EQ -> compare (destino a1) (destino a2)
