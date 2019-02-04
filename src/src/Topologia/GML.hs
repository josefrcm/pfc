-------------------------------------------------------------------------------
-- Autor: José Franco Campos
-- 
-- Generación, análisis y optimización de escenarios de migración
-- de redes Ipv4/IPv6 mediante programación funcional.
-- 
-- Diciembre 2008
--
--
-- Descripción: conversión de los diferentes tipos de datos que forman un
--              grafo hacia y desde GML.
-------------------------------------------------------------------------------

module Topologia.GML (
	GML (toGML, fromGML),
	cargarGrafo,
	guardarGrafo
	) where


-- Módulos externos
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

-- Módulos del programa
import Topologia.Grafo
import Topologia.Parser
import Topologia.Tipos



-------------------------------------------------------------------------------
-- Funciones auxiliares.
-------------------------------------------------------------------------------

-- Borra una lista de claves de un mapa clave-valor.
deleteList :: Ord k => Map.Map k a -> [k] -> Map.Map k a
deleteList = foldl (\atribs key -> Map.delete key atribs)


-- Carga un fichero GML, devolviendo un grafo si todo va bien, o lanzando
-- una excepción si hay algún error.
cargarGrafo :: FilePath -> IO Grafo
cargarGrafo filename = do
	str <- readFile filename
	let g = fromGML $ leerGML filename str
	return g


-- Guarda un grafo en un fichero GML, lanzando una excepción si hay algún error.
guardarGrafo :: FilePath -> Grafo -> IO ()
guardarGrafo filename grafo = do
	let str = escribirGML $ toGML grafo
	writeFile filename str



-------------------------------------------------------------------------------
-- Conversión entre tipos de datos y árboles GML.
-------------------------------------------------------------------------------

-- La clase GML define las dos operaciones mínimas que debe soportar un tipo
-- de datos para poder ser convertido a GML.
class GML a where
	toGML :: a -> Valor
	fromGML :: Valor -> a



-------------------------------------------------------------------------------
-- Conversión del tipo Grafo
-------------------------------------------------------------------------------

instance GML Grafo where
	toGML g = VM $ Map.singleton "graph" [VM $ base `Map.union` atributos' `Map.union` nodos' `Map.union` arcos']
		where
		-- Convierte los atributos básicos.
		base = Map.fromList [
			("directed", [VI $ fromEnum $ dirigido g])
			]
		atributos' = atr_grafo g
		nodos' = Map.singleton "node" $ map toGML (IntMap.elems $ nodos g)
		arcos' = Map.singleton "edge" $ map toGML $ concat (IntMap.elems $ arcos g)

	fromGML (VM atributos) = case Map.lookup "graph" atributos of
		Just [VM grafo] -> vacio {atr_grafo=atributos' , dirigido=dirigido'} `insertarNodos` nodos' `insertarArcos` arcos'
			where
			-- Nodos del grafo.
			nodos' = case Map.lookup "node" grafo of
				Just lnodos -> map fromGML lnodos
				Nothing     -> error "El grafo no tiene nodos"
			
			-- Arcos del grafo.
			arcos' = case Map.lookup "edge" grafo of
				Just larcos -> map fromGML larcos
				Nothing     -> []
			
			-- ¿El grafo es dirigido o no?
			dirigido' = case Map.lookup "directed" grafo of
				Just [VI 0] -> False
				Just [VI 1] -> True
				Just _      -> error "El atributo del valor 'directed' es incorrecto"
				Nothing     -> False
			
			-- Atributos del grafo.
			atributos' = grafo `deleteList` ["edge", "node", "directed"]
		_ -> error "El fichero no contiene grafos"

	-- Si el grafo no tiene atributos, falla.
	fromGML _ = error "Grafo incorrecto"



-------------------------------------------------------------------------------
-- Conversión del tipo Nodo
-------------------------------------------------------------------------------

instance GML Nodo where
	toGML n = VM $ base `Map.union` atr_nodo n
		where
		-- Convierte los atributos básicos.
		base = Map.fromList [
			("id", [VI $ ident n]),
			("protocol", [VS $ show $ protocolo n]),
			("graphics", [toGML $ graficos n]),
			("name", [VS $ nombre n]),
			("label", [VS $ etiqueta n])]
	
	fromGML (VM atributos) = Nodo id' protocolo' graficos' nombre' etiqueta' atributos'
		where
		-- Identificador del nodo.
		id' = case Map.lookup "id" atributos of
			Just [VI i] -> i
			Just _      -> error "Nodo con identificador incorrecto"
			Nothing     -> error "Nodo sin identificador"
	
		-- Protocolo del nodo (si no tiene, se asume que es IPv4).
		protocolo' = case Map.lookup "protocol" atributos of
			Just [VS p] -> read p
			Just _      -> error "Nodo con protocolo incorrecto"
			Nothing     -> R4
	
		-- Representación gráfica del nodo.
		graficos' = case Map.lookup "graphics" atributos of
			Just [VM xs] -> fromGML (VM xs)
			Just _       -> error "Nodo con parámetros gráficos incorrectos"
			Nothing      -> Graficos {center = Vector 0.0 0.0 0.0, atr_graficos = Map.empty}
		
		-- Nombre del nodo (opcional).
		nombre' = case Map.lookup "name" atributos of
			Just [VS xs] -> xs
			Just _       -> error "Nodo con nombre incorrecto"
			Nothing      -> ""
		
		-- Etiqueta del nodo (opcional).
		etiqueta' = case Map.lookup "label" atributos of
			Just [VS xs] -> xs
			Just _       -> error "Nodo con etiqueta incorrecta"
			Nothing      -> ""
		
		-- Resto de atributos.
		atributos' = atributos `deleteList` ["id", "protocol", "graphics", "name", "label"]

	-- Si el nodo no tiene atributos, falla.
	fromGML _ = error "Nodo incorrecto"



-------------------------------------------------------------------------------
-- Conversión del tipo Graficos
-------------------------------------------------------------------------------

instance GML Graficos where
	toGML g = VM $ base `Map.union` atr_graficos g
		where
		-- Convierte los atributos básicos.
		base = Map.fromList [
			("center", [toGML $ center g])]
	
	fromGML (VM atributos) = Graficos center' atributos'
		where
		-- Centro del nodo.
		center' = case Map.lookup "center" atributos of
			Just [VM xs] -> fromGML (VM xs)
			Just _ -> error "Centro incorrecto"
			Nothing -> Vector 0.0 0.0 0.0
	
		-- Resto de atributos.
		atributos' = atributos `deleteList` ["center"]

	-- Si los graficos no tienen atributos, falla.
	fromGML _ = error "Gráficos incorrectos"



-------------------------------------------------------------------------------
-- Conversión del tipo Vector
-------------------------------------------------------------------------------

instance GML Vector where
	toGML c = VM $ base
		where
		-- Convierte los atributos básicos.
		base = Map.fromList [
			("x", [VR $ x c]),
			("y", [VR $ y c]),
			("z", [VR $ z c])]

	fromGML (VM atributos) = Vector x' y' z'
		where
		-- Coordenada X.
		x' = case Map.lookup "x" atributos of
			Just [VI i] -> fromIntegral i
			Just [VR r] -> r
			Just _      -> error "Coordenada X incorrecta"
			Nothing     -> 0.0
	
		-- Coordenada Y.
		y' = case Map.lookup "y" atributos of
			Just [VI i] -> fromIntegral i
			Just [VR r] -> r
			Just _      -> error "Coordenada Y incorrecta"
			Nothing     -> 0.0
	
		-- Coordenada Z.
		z' = case Map.lookup "z" atributos of
			Just [VI i] -> fromIntegral i
			Just [VR r] -> r
			Just _      -> error "Coordenada Z incorrecta"
			Nothing     -> 0.0

	-- Si el vector no tiene atributos, falla.
	fromGML _ = error "Coordenadas incorrectas"



-------------------------------------------------------------------------------
-- Conversión del tipo Arco
-------------------------------------------------------------------------------

instance GML Arco where
	toGML a = VM $ base `Map.union` atr_arco a
		where
		-- Convierte los atributos básicos.
		base = Map.fromList [
			("source", [VI $ origen a]),
			("target", [VI $ destino a]),
			("latency", [VR $ latencia a]),
			("bandwidth", [VR $ anchoBanda a]),
			("metric", [VR $ metrica a])]

	fromGML (VM atributos) = Arco origen' destino' latencia' anchoBanda' metrica' atributos'
		where
		-- Origen del arco.
		origen' = case Map.lookup "source" atributos of
			Just [VI o] -> o
			Just _      -> error "Arco con origen incorrecto"
			Nothing     -> error "Arco sin origen"
		
		-- Destino del arco.
		destino' = case Map.lookup "target" atributos of
			Just [VI d] -> d
			Just _      -> error "Arco con destino incorrecto"
			Nothing     -> error "Arco sin destino"
			
		-- Latencia del arco.
		latencia' = case Map.lookup "latency" atributos of
			Just [VR l] -> l
			Just [VI l] -> fromIntegral l
			Just _      -> error "Arco con latencia incorrecta"
			Nothing     -> 0.0
			
		-- Ancho de banda del arco.
		anchoBanda' = case Map.lookup "bandwidth" atributos of
			Just [VR b] -> b
			Just [VI b] -> fromIntegral b
			Just _      -> error "Arco con ancho de banda incorrecto"
			Nothing     -> 1.0
			
		-- Latencia del arco.
		metrica' = case Map.lookup "metric" atributos of
			Just [VR l] -> l
			Just [VI l] -> fromIntegral l
			Just _      -> error "Arco con métrica incorrecta"
			Nothing     -> 0.0
	
		-- Resto de atributos.
		atributos' = atributos `deleteList` ["source", "target", "latency", "bandwidth", "metric"]

	-- Si el arco no tiene atributos, falla.
	fromGML _ = error "Arco incorrecto"
